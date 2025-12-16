###################################  Covariates and ADF Tests ###################################
# 1) Load in exogenous covariates
# 2) Run ADF tests
# 3) ADF test table, save
# 4) Prep for analysis
#######################################################################################################
#######################################################################################################
# 1) Load in exogenous variates
load("Data/covariates.RData")
full_data <- full_data %>% rename(month=Month) %>% filter(year>=2006 & year<2020)
covars <- full_data %>% arrange(year, month) 

# change to numeric
covars <- sapply(covars, as.numeric) # convert to numeric
covars <- as.data.frame(covars)

# save copy of untransformed data
covars_plot <- covars

#######################################################################################################
# 2) Run ADF tests
# untransformed data
results_levels <- as_tibble(list())
for(i in 1:ncol(covars)){
  if(i >2){ # don't do test on month and year vars
    data <- ts(covars[,i], frequency=12)
    
    test_result <- adf.test(data)
    
    temp_result <- tibble(
      Variable = names(covars[i]),
      Statistic = round(test_result$statistic,4),
      `P-value` = round(test_result$p.value,4))
    
    results_levels <- rbind(results_levels,temp_result)
    
  }
}

results_levels$Statistic <- as.character(results_levels$Statistic)
results_levels$Statistic <- if_else(results_levels$`P-value` < 0.05, paste(results_levels$Statistic, "**"), results_levels$Statistic)

# define explan_vars
explan_vars <- colnames(covars)
explan_vars <- explan_vars[-c(1,2)]

# Log transform
covars[explan_vars] <- lapply(covars[explan_vars], log)

results_log <- as_tibble(list())
for(i in 1:ncol(covars)){
  if(i >2){ # don't do test on month and year vars
    data <- ts(covars[,i], frequency=12)
    
    test_result <- adf.test(data)
    
    temp_result <- tibble(
      Variable = names(covars[i]),
      Statistic = round(test_result$statistic,4),
      `P-value` = round(test_result$p.value,4))
    
    results_log <- rbind(results_log,temp_result)
    
  }
}
results_log$Statistic <- as.character(results_log$Statistic)
results_log$Statistic <- if_else(results_log$`P-value` < 0.05, paste(results_log$Statistic, "**"), results_log$Statistic)

full_covars <- covars

# FD transform covariates 
covars2 <- covars[2:nrow(covars),]
covars2[explan_vars] <- as.data.frame((lapply(covars[explan_vars], diff, lag=1)))# take FD for all vars

covars <- covars2

results_logFD <- as_tibble(list())
for(i in 1:ncol(covars)){
  if(i >2){ # don't do test on month and year vars
    data <- ts(covars[,i], frequency=12)
    
    test_result <- adf.test(data)
    
    temp_result <- tibble(
      Variable = names(covars[i]),
      Statistic = round(test_result$statistic,4),
      `P-value` = round(test_result$p.value,4))
    
    results_logFD <- rbind(results_logFD,temp_result)
    
  }
}
results_logFD$Statistic <- as.character(results_logFD$Statistic)
results_logFD$Statistic <- if_else(results_logFD$`P-value` < 0.05, paste(results_logFD$Statistic, "**"), results_logFD$Statistic)

# SD transform covariates 
covars2 <- covars[2:nrow(covars),]
covars2[explan_vars] <- as.data.frame((lapply(covars[explan_vars], diff, lag=1)))# take SD for all vars

results_logSD <- as_tibble(list())
for(i in 1:ncol(covars2)){
  if(i >2){ # don't do test on month and year vars
    data <- ts(covars2[,i], frequency=12)
    
    test_result <- adf.test(data)
    
    temp_result <- tibble(
      Variable = names(covars2[i]),
      Statistic = round(test_result$statistic,4),
      `P-value` = round(test_result$p.value,4))
    
    results_logSD <- rbind(results_logSD,temp_result)
    
  }
}
results_logSD$Statistic <- as.character(results_logSD$Statistic)
results_logSD$Statistic <- if_else(results_logSD$`P-value` < 0.05, paste(results_logSD$Statistic, "**"), results_logSD$Statistic)

#######################################################################################################
# 3) ADF test table, save
adf_table <- as.data.frame(cbind(Variable=results_levels$Variable, Levels=results_levels$Statistic, `Log`=results_log$Statistic, `Log-FD` =results_logFD$Statistic))


adf_table$Variable <- if_else(adf_table$Variable=="fuel_oil2", "Fuel Oil", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="DispInc", "Disposable Income", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="FishMeal", "Fish Meal Price Index", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="SoyMeal", "Soy Meal Price Index", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="exog_fishCPI", "Fish CPI Residuals", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="Exchange", "Exchange Rates", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="ContainerShip", "Containerized Shipping Rate", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="FedFunds", "Federal Funds Rate", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="ColdStorage", "Cold Storage Rate", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="Wheat", "Wheat Price Index", adf_table$Variable)
adf_table$Variable <- if_else(adf_table$Variable=="food_cpi", "All Food CPI", adf_table$Variable)


comm <- paste0("\\hline \\hline \n \\multicolumn{4}{l}",
               "{\\scriptsize{Note: ** indicates significance at the 5\\% level.}} \n")
print(xtable(adf_table, type = "latex", caption = 'Test statistics from Augmented Dickey-Fuller tests for unit root on covariates. For
the log-first diﬀerence transformation, we reject the null hypothesis of a unit root in favor of
the alternative that the time series are stationary. For fish CPI residuals and containerized
shipping rates, we reject the null hypothesis for the log transformation as well.', label='table:adf_table'), 
      tabular.environment="tabular", include.rownames = FALSE, caption.placement = 'top', hline.after=c(-1, 0), add.to.row = list(pos = list(nrow(adf_table)), command = comm),
      file = paste(output_location, "Tables/adf_table.tex", sep=''))


#######################################################################################################
# 4) Prep for analysis
covars$month_yr <- (paste0(covars$year,"-", covars$month, "-01"))
covars$month_yr <- as.Date(covars$month_yr)

# use log fish cpi residuals and containership, no differencing
full_covars <- full_covars[-1,]
covars$exog_fishCPI <- full_covars$exog_fishCPI
covars$ContainerShip <- full_covars$ContainerShip

#######################################################################################################
# 5) Plot time series of covariates 
covars_plot <- covars_plot %>% filter(year>=2012 & year<2018) %>% select(!c(month,year)) %>% rename(`Fuel Oil` = fuel_oil2, `Fish Meal`=FishMeal, `Fish CPI Residuals`=exog_fishCPI, `Soy Meal`=SoyMeal, 
                                                                                                    `Disposable Income`=DispInc, `Containerized Shipping Rate`=ContainerShip, `Federal Funds Rate`=FedFunds,
                                                                                                    `Exchange Rates`=Exchange, `Cold Storage Rate`=ColdStorage, `Wheat Price Index`=Wheat)


plots <- list()
for(i in unique(colnames(covars_plot))){
  temp_data <- ts(covars_plot %>% select(all_of(i)) ,frequency=12, start=c(2012,1))
  
  plots[[i]] <- autoplot(temp_data) + theme_bw() + ggtitle(i)  + xlab("") + ylab("") +  theme(plot.title = element_text(size=28,hjust = 0.5),
                                                                                              axis.text.y=element_text(size=22),axis.text.x=element_text(size=22))
}

# save figure
covars_ts <- ggpubr::ggarrange(plotlist=plots) 
covars_ts <- annotate_figure(covars_ts, left = textGrob("Price Index", rot = 90, vjust = 1, gp = gpar(cex = 3)),
                             bottom = textGrob("Date", vjust = 0, gp = gpar(cex = 3)))
ggsave(filename=paste(output_location,'Figures/ARIMA_Price/covars_ts.png' , sep=''), plot=covars_ts, width = 25, height = 15, dpi = 200)

