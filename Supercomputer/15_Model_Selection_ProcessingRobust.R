#######################################################################################################  
###################################  Run Code for SIMP Impacts Paper ###################################
# 1) Remove problematic hts codes
# 2) Run robustness for processing groups
#######################################################################################################
# Model selection
source("Functions_ModelSelection.R")

#######################################################################################################
# 1) Remove problematic hts codes
imports_robust <- imports %>% filter(ind_process_robust==0)
#######################################################################################################
# 2) Run robustness for processing groups
process_data <- imports_robust %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & !is.na(process_group)) %>% group_by(month_yr, process_group) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
process_data$dollars_per_mton <- (process_data$real_dollars / process_data$mtons_raw)                                                       

for(i in unique(process_data$process_group)){
  data <- process_data %>% filter(process_group==i) %>% arrange(month_yr)
  data <- log(ts(data$dollars_per_mton, frequency=12, start=c(2012,1)))
  
  n<-length(data)
  dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
  start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
  
  selection <- ModelSelectionManual(y = data, dates = dates, int.date = int.date,ic="aicc",auto.args = arima_args, xreg =xreg_vars, n.cores=n_cores)

  filename <- paste( 'Output/Robust_', i, '.csv', sep='')
  write_csv(selection, filename) # save 
  
  rm(list=c("selection"))
}




