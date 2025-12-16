#######################################################################################################
##################################  Robustness of Processing Result ##################################
# 1) Remove problematic codes
# 2) Processing robustness results
#######################################################################################################
# 1) Remove problematic codes
imports_robust <- imports %>% filter(ind_process_robust==0)
results <- list()
candidate_models <- list()
#######################################################################################################
# 2) Processing robustness results
process_data <- imports_robust %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & !is.na(process_group)) %>% group_by(month_yr, process_group) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
process_data$dollars_per_mton <- (process_data$real_dollars / process_data$mtons_raw)                                                       
quant <- process_data %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01") %>% group_by(process_group, month_yr) %>% summarise(mtons_raw=sum(mtons_raw))
quant <- quant %>% arrange(desc(mtons_raw))

month_fig<- list()
plots <- list()
for(i in unique(process_data$process_group)){
  data <- process_data %>% filter(process_group==i) %>% arrange(month_yr)
  data <- (ts(data$dollars_per_mton, frequency=12, start=c(2012,1)))
  post_quant <- quant %>% filter(process_group==i)
  
  n<-length(data)
  dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
  start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
  
  
  file_name <- paste("Supercomputer/Output/Robust_", i,".csv", sep="")
  input_dataframe <- read_csv(file_name)

  processing_forecasts <- MakeForecasts(y = data,candidate.models=input_dataframe ,dates = dates, int.date = int.date,xreg =xreg_vars, nboot=10000)
  
  processing_impacts <- ImpactEstimates(y = data,point.fcast=processing_forecasts$combined.point.fcast, simulations=processing_forecasts$combined.simulations, fitted.vals=processing_forecasts$fitted.vals,dates = dates, int.date = int.date,weights=post_quant$mtons_raw)
  
  # Calculate perc price change
  print(processing_impacts$wtd.est / weighted.mean(processing_forecasts$combined.point.fcast, post_quant$mtons_raw))
  
  # save parallel trends figures
  plots[[i]] <- processing_impacts[["forecast.plot"]] + ggtitle(i) +scale_y_continuous(label=comma)+theme(legend.position="none",plot.title = element_text(size=28,hjust = 0.5),
                                                              axis.text=element_text(size=18),
                                                              axis.title=element_text(size=24), strip.text.x = element_text(size = 14), legend.title=element_text(size=20), 
                                                              legend.text=element_text(size=18)) 
  
  # save estimates to dataframe
  plot <- tibble(
    Group = i,
    Coefficient = processing_impacts$wtd.est,
    p = processing_impacts$p.val,
    High95 = processing_impacts$avg.tau.high, # 95%
    Low95 = processing_impacts$avg.tau.low,
    `2016 Quantity` = sum(post_quant$mtons_raw))
  
  results <- rbind(results,plot)
  
  candidate_models_temp <- processing_forecasts$candidate_models %>% rename(Model=model, `Exogenous Variables`=xreg, `Weight`=w_i) %>% select(!c(delta_i, ic))
  candidate_models_temp$Series <- i
  
  candidate_models <- rbind(candidate_models, candidate_models_temp)
  
  # monthly plot
  temp_month_processing <- tibble(
    month=rep(1:12),
    Estimate=as.vector(processing_impacts$raw.diff),
    High95 = processing_impacts$monthly_high,
    Low95 = processing_impacts$monthly_low,
    Group=i
  )
  
  month_fig[[i]] <- ggplot(temp_month_processing, aes(x=month, y=Estimate)) + 
    geom_point(size=10) +
    #geom_errorbar(aes(ymin = Low90, ymax = High90), width = 0.75, linewidth=2) + 
    geom_errorbar(aes(ymin = Low95, ymax = High95), width = 0.75, linewidth=2) + 
    #geom_errorbar(aes(ymin = Low99, ymax = High99), width = 0.75, linewidth=2) + 
    geom_hline(yintercept = 0, color="red", linetype="dashed", linewidth=1) +
    labs(x=" ", y=" ") +
    theme_bw()+
    scale_x_continuous(breaks=c(3,6,9,12), labels = c( "Mar", "June", "Sept", "Dec"))+
    scale_y_continuous(label=comma)+
    facet_wrap(~Group)+
    theme(axis.text=element_text(size=52),
          axis.title=element_text(size=40), legend.text=element_text(size=52), legend.title=element_text(size=52),strip.text.x = element_text(size = 42))
 # monthly_impact_processing <- rbind(monthly_impact_processing, temp_month_processing)
  
}

# save parallel trends figure
parallel_trends <- ggarrange(plotlist=plots, common.legend = TRUE, legend="bottom") 
parallel_trends <- annotate_figure(parallel_trends, left = textGrob("2019 USD/ton Imported", rot = 90, vjust = 1, gp = gpar(cex = 2)),
                                   bottom = textGrob("Date", vjust = -3.5, gp = gpar(cex = 2)))
ggsave(filename=paste(output_location, 'Figures/ModelAvgResults/processing_robust.png', sep=''), plot=parallel_trends, width = 15, height = 5, dpi = 200)

by_months <- ggarrange(plotlist=month_fig) 
by_months <- annotate_figure(by_months, left = textGrob("Observed - Forecast (2019 USD/tonne)", rot = 90, vjust = 1, gp = gpar(cex = 3.25)),
                                   bottom = textGrob("Date", vjust = -.5, gp = gpar(cex = 3.5)))
ggsave(filename=paste(output_location, 'Figures/ModelAvgResults/by_month_processing_robust.png', sep=''),plot=by_months, width = 25, height = 10, dpi = 200)



