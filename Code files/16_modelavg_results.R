#######################################################################################################
##################################  Main Price Analysis and Placebos ##################################
# 1) SARIMAX aggregate for all SIMP, drop shrimp and abalone
# 2) Placebo: non-SIMP species
# 3) Placebo years
# 4) Processing
# 5) NMFS species group
#######################################################################################################
#######################################################################################################
# 1) SARIMAX aggregate for all SIMP, drop shrimp and abalone
simp_species_imports <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone") %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
quant_2017 <- simp_species_imports %>% filter(month_yr>="2017-01-01")

simp_species_imports$dollars_per_mton <- (simp_species_imports$real_dollars / simp_species_imports$mtons_raw)
post_quant <- simp_species_imports %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01")
quant_2017 <- simp_species_imports %>% filter(month_yr>="2017-01-01")

data <- (ts(simp_species_imports$dollars_per_mton, frequency=12, start=c(2012,1)))
n<-length(data)
dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))

all_SIMP <- read_csv("Supercomputer/Output/all_SIMP.csv")

forecasts <- MakeForecasts(y = data,candidate.models=all_SIMP ,dates = dates, int.date = int.date,xreg =xreg_vars, nboot=10000)

impacts <- ImpactEstimates(y = data,point.fcast=forecasts$combined.point.fcast, simulations=forecasts$combined.simulations, fitted.vals=forecasts$fitted.vals,dates = dates, int.date = int.date,weights=post_quant$mtons_raw)

simp_all <- impacts[["forecast.plot"]] + ggtitle("SIMP Species")+ scale_y_continuous(label=comma) + theme(plot.title = element_text(size=28,hjust = 0.5),
                                                                                                        axis.text=element_text(size=22),
                                                                                                        axis.title=element_text(size=28), strip.text.x = element_text(size = 14),  legend.title=element_text(size=28), 
                                                                                                        legend.text=element_text(size=24))  + xlab("Date") + ylab("2019 USD/tonne")
file_name = paste(output_location, 'Figures/ModelAvgResults/simp_impact_all_modelavg.png', sep='')
ggsave(filename=file_name, plot=simp_all,width = 15, height = 7, dpi = 200)


# monthly plot
monthly_impact <- tibble(
  month=rep(1:12),
  Estimate=impacts$raw.diff,
  High95 = impacts$monthly_high,
  Low95 = impacts$monthly_low,
)

ggplot(monthly_impact, aes(x=month, y=Estimate)) + 
  geom_point(size=10) +
  #geom_errorbar(aes(ymin = Low90, ymax = High90), width = 0.75, linewidth=2) + 
  geom_errorbar(aes(ymin = Low95, ymax = High95), width = 0.75, linewidth=2) + 
  #geom_errorbar(aes(ymin = Low99, ymax = High99), width = 0.75, linewidth=2) + 
  geom_hline(yintercept = 0, color="red", linetype="dashed", linewidth=1) +
  labs(x="Date", y="Observed - Forecast (2019 USD/tonne)") +
  theme_bw()+
  scale_x_continuous(breaks=c(3,6,9,12), labels = c( "Mar", "June", "Sept", "Dec"))+
  scale_y_continuous(label=comma)+
  theme(axis.text=element_text(size=52),
        axis.title=element_text(size=42), legend.text=element_text(size=52), legend.title=element_text(size=52)) 
ggsave(filename=paste(output_location, 'Figures/ModelAvgResults/by_month_modelavg.png', sep=''),width = 25, height = 12, dpi = 200)




print(impacts$wtd.est * sum(post_quant$mtons_raw))
print(paste("2017:", sum(quant_2017$mtons_raw) * impacts$wtd.est), sep=" ")

#impacts$wtd.est * sum(quant_2017$mtons_raw)

# Calculate perc price change
print(impacts$wtd.est / weighted.mean(forecasts$combined.point.fcast, post_quant$mtons_raw))

results <- tibble(
  Group= "SIMP Species", 
  Coefficient = impacts$wtd.est,
  p = impacts$p.val,
  High95 = impacts$avg.tau.high, # 95%
  Low95 = impacts$avg.tau.low,
  `2016 Quantity` = sum(post_quant$mtons_raw))

# Output full set of candidate models to overleaf table
candidate_model_table <- forecasts$candidate_models %>% rename(Model=model, `Exogenous Variables`=xreg, `Weight`=w_i) %>% select(!c(delta_i, ic))
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "fuel_oil2", "Fuel Oil")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "DispInc", "Disposable Income")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "FishMeal", "Fish Meal")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "SoyMeal", "Soy Meal")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "exog_fishCPI", "Fish CPI Residuals")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "Exchange", "Exchange")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "ContainerShip", "Containerized Shipping")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "FedFunds", "Federal Funds")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "ColdStorage", "Cold Storage")
candidate_model_table$`Exogenous Variables` <- str_replace_all(candidate_model_table$`Exogenous Variables`, "Wheat", "Wheat Price")

candidate_model_table$Model <- str_remove_all(candidate_model_table$Model, "Regression with ")
candidate_model_table$Model <- str_remove_all(candidate_model_table$Model, "ARIMA")
candidate_model_table$Model <- str_remove_all(candidate_model_table$Model, "\\[12\\] errors")

print(xtable(candidate_model_table, digits=2, type = "latex", caption = 'Candidate models used to create the combined forecast for all SIMP species prices. We include a description of the SARIMA model orders (p,d,q)(P,D,Q), exogenous variables selected, and Akaike weight.', label='table:main_candidate_models'), 
      tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top',
      file = paste(output_location, 'Tables/main_candidate_models.tex', sep=''))

#######################################################################################################
# 2) Placebo: all non-SIMP
candidate_models_all <- list()
placebo_species <- imports %>% filter(!species_group %in% simp_groups & species_group!="Unidentified Species" & species_group!="Whitefish" 
                                      & !str_detect(species_group, "Shellfish") & !str_detect(species_group, "Groundfish") & !str_detect(species_group, "Crustaceans")
                                      & !species_group %in% simp_adj_groups & !str_detect(species_group, "Crab") & species_group!="Rays Skates") %>% group_by(species_group) %>% summarise()

non_simp_species_imports <- imports %>% filter(species_group %in% placebo_species$species_group) %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
print(paste(unlist(placebo_species), collapse =", "))

non_simp_species_imports$dollars_per_mton <- (non_simp_species_imports$real_dollars / non_simp_species_imports$mtons_raw)
post_quant <- non_simp_species_imports %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01")

data <- (ts(non_simp_species_imports$dollars_per_mton, frequency=12, start=c(2012,1)))
n<-length(data)
dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))

placebospecies_models <- read_csv("Supercomputer/Output/placebospecies.csv")

placebo_forecasts <- MakeForecasts(y = data,candidate.models=placebospecies_models ,dates = dates, int.date = int.date,xreg =xreg_vars, nboot=10000)

placebo_impacts <- ImpactEstimates(y = data,point.fcast=placebo_forecasts$combined.point.fcast, simulations=placebo_forecasts$combined.simulations, fitted.vals=placebo_forecasts$fitted.vals,dates = dates, int.date = int.date,weights=post_quant$mtons_raw)



placebo_species_plot <- placebo_impacts[["forecast.plot"]] + ggtitle("Placebo Species Price")+ scale_y_continuous(label=comma) + theme(plot.title = element_text(size=28,hjust = 0.5),
                                                                                                                   axis.text=element_text(size=22),
                                                                                                                   axis.title=element_text(size=28), strip.text.x = element_text(size = 14),  legend.title=element_text(size=28), 
                                                                                                                   legend.text=element_text(size=24))  + xlab("Date") + ylab("2019 USD/tonne")
file_name = paste(output_location,'Figures/ModelAvgResults/placebo_species.png' , sep='')
ggsave(filename=file_name, plot=placebo_species_plot,width = 15, height = 7, dpi = 200)


results_placebo <- tibble(
  Group= "Placebo Species", 
  Coefficient = placebo_impacts$wtd.est,
  p = placebo_impacts$p.val,
  High95 = placebo_impacts$avg.tau.high, # 95%
  Low95 = placebo_impacts$avg.tau.low,
  `2016 Quantity` = sum(post_quant$mtons_raw))


results <- rbind(results, results_placebo)


candidate_models <- placebo_forecasts$candidate_models %>% rename(Model=model, `Exogenous Variables`=xreg, `Weight`=w_i) %>% select(!c(delta_i, ic))
candidate_models$Series <- "Placebo Species"

#######################################################################################################
# 3) Placebo years 
placebo_year_imports <- imports_placebo_yrs %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone") %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
placebo_year_imports <- placebo_year_imports %>% filter(month_yr>= "2007-01-01" & month_yr<"2017-01-01")

placebo_year_imports$dollars_per_mton <- (placebo_year_imports$real_dollars / placebo_year_imports$mtons_raw)

results_placebo_years <-  as_tibble(list())
placebo_yr_plots <- list()

for(k in as.character(unique(placebo_year_imports$month_yr))){
  if(k >="2012-01-01" & k <"2017-01-01" & str_detect(k, "-01-01")){
    start_date <- as.Date(k) %m-% years(5) # use 5 years to fit model
    end_date <- as.Date(k) %m+% years(1) # look at impacts one year out
    ts_start <- as.numeric(format(start_date,"%Y"))
    
    data <- placebo_year_imports %>% filter(month_yr<end_date & month_yr>=start_date) %>% arrange(month_yr)
    
    post_quant <- data %>% filter(month_yr >= as.Date(k) %m-% years(1) & month_yr<k) # Save off quantity in post-treatment period
    
    data <- (ts(data$dollars_per_mton, frequency=12, start=c(ts_start,1)))
    
    file_name <- paste("Supercomputer/Output/placeboyear_", format(as.Date(k),"%Y"),".csv", sep="")
    input_dataframe <- read_csv(file_name)
    
    # specify covariates
    covars_placeboyrs <- covars %>% filter(month_yr<end_date & month_yr>=start_date)
    xreg_vars_placeboyrs <- covars_placeboyrs %>% select(all_of(xreg_vars_list))
    
    n<-length(data)
    dates <- seq.Date(from = as.Date(start_date), by = "months", length.out = n)
    start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
    
    placebo_forecasts <- MakeForecasts(y = data,candidate.models=input_dataframe ,dates = dates, int.date = as.Date(k),xreg =xreg_vars_placeboyrs, nboot=10000)
    
    placebo_impacts <- ImpactEstimates(y = data,point.fcast=placebo_forecasts$combined.point.fcast, simulations=placebo_forecasts$combined.simulations, placebo_forecasts$fitted.vals,dates = dates, int.date = as.Date(k),weights=post_quant$mtons_raw)
    
    placebo_yr_plots[[k]] <- placebo_impacts[["forecast.plot"]] + ggtitle(format(as.Date(k),"%Y")) +theme(legend.position="none",plot.title = element_text(size=28,hjust = 0.5),
                                                                                                 axis.text=element_text(size=20),
                                                                                                 axis.title=element_text(size=22), strip.text.x = element_text(size = 10), legend.title=element_text(size=22), 
                                                                                                 legend.text=element_text(size=20)) +
                                                                                                 labs(color="Time series", linetype="Placebo date") +
                                                                                                 guides(linetype = guide_legend(override.aes = list(fill = NA))) 

    # save estimates to dataframe
    plot <- tibble(
      Group = paste("Placebo Year", format(as.Date(k),"%Y"), sep=" "),
      Coefficient = placebo_impacts$wtd.est,
      p = placebo_impacts$p.val,
      High95 = placebo_impacts$avg.tau.high, # 95%
      Low95 = placebo_impacts$avg.tau.low,
      `2016 Quantity` = NA)
    
    results_placebo_years <- rbind(results_placebo_years,plot)
    
    candidate_models_temp <- placebo_forecasts$candidate_models %>% rename(Model=model, `Exogenous Variables`=xreg, `Weight`=w_i) %>% select(!c(delta_i, ic))
    candidate_models_temp$Series <- paste("Placebo Year", format(as.Date(k),"%Y"), sep=" ")
    
    candidate_models <- rbind(candidate_models, candidate_models_temp)
    
  }
}

results <- rbind(results, results_placebo_years)

results_placebo_years$month_yr <- as.Date(paste(str_remove_all(results_placebo_years$Group, "Placebo Year"),"-01-01",sep=""))

# make figure
placebo_year_figure <- ggplot(results_placebo_years, aes(y=Coefficient, x=as.Date(month_yr))) + 
  geom_line(size=2) +
  #geom_text(aes(label = year), hjust=-0.002, vjust = -0.03)+
  geom_hline(yintercept = 0, color="red", linetype="dashed", linewidth=1) +
  geom_ribbon(aes(ymin = Low95, ymax = High95), alpha = .3)+
  #geom_errorbar(aes(ymin = Coefficient$estimate - sd$sd*1.96, ymax = Coefficient$estimate + sd$sd*1.96), width = 0.1, size = 3.5,color = "lightgrey", alpha=0.7) +
  labs(x="Date", y="Estimate (2019 USD/tonne)") +
  #scale_color_manual(values = (RColorBrewer::brewer.pal(8,'BrBG'))) +
  scale_y_continuous(label=comma)+
  #labs(color = "Product Form") +
  theme_bw()+
  theme(plot.title = element_text(size=20,hjust = 0.5),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20), strip.text.x = element_text(size = 10)) 
#geom_rect(inherit.aes = F, data = shades, mapping = aes(xmin=xmin, xmax=xmax, ymin = ymin, ymax = ymax), alpha = 0.2)+
#ggtitle("Placebo Species Price Impacts of the U.S. SIMP") 
ggsave(filename=paste(output_location,'Figures/ModelAvgResults/placebo_years.png' , sep=''), plot=placebo_year_figure, width = 10, height = 5, dpi = 200)

# Make time series fig for each placebo year
parallel_trends <- ggarrange(plotlist=placebo_yr_plots, common.legend = TRUE, legend="bottom")
parallel_trends <- annotate_figure(parallel_trends, left = textGrob("2019 USD/tonne Imported", rot = 90, vjust = 1, gp = gpar(cex = 2)),
                                   bottom = textGrob("Date", vjust = -3, gp = gpar(cex = 2)))
ggsave(filename=paste(output_location, 'Figures/ModelAvgResults/placebo_yr_ts.png', sep=''), plot=parallel_trends, width = 20, height = 10, dpi = 200)

#######################################################################################################
# 4) Processing
process_data <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & !is.na(process_group)) %>% group_by(month_yr, process_group) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
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
  
  
  file_name <- paste("Supercomputer/Output/", i,".csv", sep="")
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
          axis.title=element_text(size=42), legend.text=element_text(size=52), legend.title=element_text(size=52),strip.text.x = element_text(size = 42))
 # monthly_impact_processing <- rbind(monthly_impact_processing, temp_month_processing)
  
}

# save parallel trends figure
parallel_trends <- ggpubr::ggarrange(plotlist=plots, common.legend = TRUE, legend="bottom") 
parallel_trends <- annotate_figure(parallel_trends, left = textGrob("2019 USD/tonne Imported", rot = 90, vjust = 1, gp = gpar(cex = 2)),
                                   bottom = textGrob("Date", vjust = -3.5, gp = gpar(cex = 2)))
ggsave(filename=paste(output_location, 'Figures/ModelAvgResults/processing.png', sep=''), plot=parallel_trends, width = 15, height = 5, dpi = 200)

by_months <- ggarrange(plotlist=month_fig) 
by_months <- annotate_figure(by_months, left = textGrob("Observed - Forecast (2019 USD/tonne)", rot = 90, vjust = 1, gp = gpar(cex = 3.25)),
                                   bottom = textGrob("Date", vjust = -.5, gp = gpar(cex = 3.5)))
ggsave(filename=paste(output_location, 'Figures/ModelAvgResults/by_month_processing.png', sep=''),plot=by_months, width = 25, height = 10, dpi = 200)


#######################################################################################################
# 5) NMFS species group
species_imports <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & species_group!="") %>% group_by(species_group, month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))

species_imports$dollars_per_mton <- (species_imports$real_dollars / species_imports$mtons_raw)

# Remove any species with NA in the ts
species_imports <- species_imports %>% group_by(species_group) %>% filter(!any(is.na(dollars_per_mton))) %>% ungroup

quant <- species_imports %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01") %>% group_by(species_group,month_yr) %>% summarise(mtons_raw=sum(mtons_raw))
quant <- quant %>% arrange(desc(mtons_raw))

plots <- list()
for(i in unique(quant$species_group)){
  data <- species_imports %>% filter(species_group==i) %>% arrange(month_yr)
  data <- (ts(data$dollars_per_mton, frequency=12, start=c(2012,1)))
  post_quant <- quant %>% filter(species_group==i)
  
  n<-length(data)
  dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
  start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
  
  j <- str_replace_all(i, ':', '_')
  file_name <- paste("Supercomputer/Output/", j,".csv", sep="")
  input_dataframe <- read_csv(file_name)
  
  fpi_forecasts <- MakeForecasts(y = data,candidate.models=input_dataframe ,dates = dates, int.date = int.date,xreg =xreg_vars, nboot=10000)
  
  fpi_impacts <- ImpactEstimates(y = data,point.fcast=fpi_forecasts$combined.point.fcast, simulations=fpi_forecasts$combined.simulations, fitted.vals=fpi_forecasts$fitted.vals,dates = dates, int.date = int.date,weights=post_quant$mtons_raw)
  
  # save parallel trends figures
  plots[[i]] <- fpi_impacts[["forecast.plot"]] + ggtitle(i) +theme(legend.position="none",plot.title = element_text(size=30,hjust = 0.5),
                                                                   axis.text=element_text(size=24),
                                                                   axis.title=element_text(size=28), strip.text.x = element_text(size = 20), legend.title=element_text(size=20), 
                                                                   legend.text=element_text(size=18)) 
  
  # save estimates to dataframe
  plot <- tibble(
    Group = i,
    Coefficient = fpi_impacts$wtd.est,
    p = fpi_impacts$p.val,
    High95 = fpi_impacts$avg.tau.high, # 95%
    Low95 = fpi_impacts$avg.tau.low,
    `2016 Quantity` = sum(post_quant$mtons_raw))
  
  results <- rbind(results,plot)
  
  candidate_models_temp <- fpi_forecasts$candidate_models %>% rename(Model=model, `Exogenous Variables`=xreg, `Weight`=w_i) %>% select(!c(delta_i, ic))
  candidate_models_temp$Series <- i
  
  candidate_models <- rbind(candidate_models, candidate_models_temp)
  
}


# save parallel trends figure
parallel_trends <- ggarrange(plotlist=plots, common.legend = TRUE, legend="bottom")
parallel_trends <- annotate_figure(parallel_trends, left = textGrob("2019 USD/ton Imported", rot = 90, vjust = 1, gp = gpar(cex = 2)),
                                   bottom = textGrob("Date", vjust = -3.5, gp = gpar(cex = 2)))
ggsave(filename=paste(output_location,'Figures/ModelAvgResults/by_NMFS_speciesgroup.png' , sep=''), plot=parallel_trends, width = 30, height = 15, dpi = 200)

species_impacts <- results %>% filter(Group %in% simp_groups)
species_impacts$Group <- factor(species_impacts$Group, levels = rev(unique(species_impacts$Group)))
species_impacts$subset <- "Other fish" 
species_impacts$subset <- if_else(str_detect(species_impacts$Group, "Tuna"), "Tunas", species_impacts$subset)
species_impacts$subset <- if_else(str_detect(species_impacts$Group, "Groundfish: Cod"), "White fish", species_impacts$subset)
species_impacts$subset <- if_else(str_detect(species_impacts$Group, "Crab"), "Crustaceans", species_impacts$subset)
species_impacts <- species_impacts %>% group_by(subset) %>% mutate(group_q=sum(`2016 Quantity`)) 
species_impacts$subset <- factor(species_impacts$subset, levels=c("Tunas", "White fish", "Other fish", "Crustaceans"))

ggplot(species_impacts, aes(x=Group, y=Coefficient)) + 
  geom_point(size=5) +
  geom_errorbar(aes(ymin = Low95, ymax = High95), width = 0.75, linewidth=2) + 
  geom_hline(yintercept = 0, color="red", linetype="dashed", linewidth=1) +
  labs(x="", y="Estimate (2019 USD/tonne)") +
  theme_bw()+
  facet_wrap(~subset, scales = "free_y")+
  scale_y_continuous(label=comma)+
  coord_flip()+
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30), strip.text.x = element_text(size = 28)) 
ggsave(filename=paste(output_location,'Figures/ModelAvgResults/by_NMFS_speciesgroup_impacts.png' , sep=''),  width = 20, height = 7, dpi = 200)


#######################################################################################################
# Save off candidate models

candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "fuel_oil2", "Fuel Oil")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "DispInc", "Disposable Income")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "FishMeal", "Fish Meal")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "SoyMeal", "Soy Meal")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "exog_fishCPI", "Fish CPI Residuals")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "Exchange", "Exchange")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "ContainerShip", "Containerized Shipping")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "FedFunds", "Federal Funds")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "ColdStorage", "Cold Storage")
candidate_models$`Exogenous Variables` <- str_replace_all(candidate_models$`Exogenous Variables`, "Wheat", "Wheat Price")

candidate_models$Model <- str_remove_all(candidate_models$Model, "Regression with ")
candidate_models$Model <- str_remove_all(candidate_models$Model, "ARIMA")
candidate_models$Model <- str_remove_all(candidate_models$Model, "\\[12\\]")
candidate_models$Model <- str_remove_all(candidate_models$Model, "errors")
candidate_models$Model <- str_remove_all(candidate_models$Model, "with non-zero mean")

candidate_models <- candidate_models %>% select(`Series`, Model, `Exogenous Variables`,`Weight`)
candidate_models$Series[duplicated(candidate_models$Series)] <- NA

print(xtable(candidate_models, digits=2, type = "latex", caption = 'Candidate models used to create the combined forecast for each price time series. We include a description of the SARIMA model orders (p,d,q)(P,D,Q), exogenous variables selected, and Akaike weight.', label='table:candidate_models_all'), 
      tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top',
      file = paste(output_location, 'Tables/candidate_models_all.tex', sep=''))


