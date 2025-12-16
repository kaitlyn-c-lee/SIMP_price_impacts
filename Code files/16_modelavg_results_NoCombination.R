#######################################################################################################
##################################  Main Price Analysis and Placebos ##################################
# 1) SARIMAX aggregate for all SIMP, drop shrimp and abalone
# 2) Placebo: non-SIMP species
# 3) Placebo years
# 4) Processing
# 5) NMFS species group
#######################################################################################################
source("Code files/Functions_ForecastingNoCombination.R")

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

results_NC <- tibble(
  Group= "SIMP Species", 
  Coefficient = impacts$wtd.est,
  p = impacts$p.val,
  High95 = impacts$avg.tau.high, # 95%
  Low95 = impacts$avg.tau.low,
  `2016 Quantity` = sum(post_quant$mtons_raw))

#######################################################################################################
# 2) Placebo: all non-SIMP
placebo_species <- imports %>% filter(!species_group %in% simp_groups & species_group!="Unidentified Species" & species_group!="Whitefish" 
                                      & !str_detect(species_group, "Shellfish") & !str_detect(species_group, "Groundfish") & !str_detect(species_group, "Crustaceans")
                                      & !species_group %in% simp_adj_groups & !str_detect(species_group, "Crab") & species_group!="Rays Skates") %>% group_by(species_group) %>% summarise()

non_simp_species_imports <- imports %>% filter(species_group %in% placebo_species$species_group) %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
print(paste(unlist(placebo_species), collapse =", "))
# print(xtable(placebo_species_save, type = "latex", caption = 'List of placebo species', label='table:placebo_species_list'), tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top', file = "/Users/kaitlynmalakoff/Dropbox (ASU)/Apps/Overleaf/Blue Crab/Tables/placebo_species_list.tex")

non_simp_species_imports$dollars_per_mton <- (non_simp_species_imports$real_dollars / non_simp_species_imports$mtons_raw)
post_quant <- non_simp_species_imports %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01")

data <- (ts(non_simp_species_imports$dollars_per_mton, frequency=12, start=c(2012,1)))
n<-length(data)
dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))

placebospecies_models <- read_csv("Supercomputer/Output/placebospecies.csv")

placebo_forecasts <- MakeForecasts(y = data,candidate.models=placebospecies_models ,dates = dates, int.date = int.date,xreg =xreg_vars, nboot=10000)

placebo_impacts <- ImpactEstimates(y = data,point.fcast=placebo_forecasts$combined.point.fcast, simulations=placebo_forecasts$combined.simulations, fitted.vals=placebo_forecasts$fitted.vals,dates = dates, int.date = int.date,weights=post_quant$mtons_raw)

results_placebo <- tibble(
  Group= "Placebo Species", 
  Coefficient = placebo_impacts$wtd.est,
  p = placebo_impacts$p.val,
  High95 = placebo_impacts$avg.tau.high, # 95%
  Low95 = placebo_impacts$avg.tau.low,
  `2016 Quantity` = sum(post_quant$mtons_raw))


results_NC <- rbind(results_NC, results_placebo)


#######################################################################################################
# 3) Placebo years 
placebo_year_imports <- imports_placebo_yrs %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone") %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
placebo_year_imports <- placebo_year_imports %>% filter(month_yr>= "2007-01-01" & month_yr<"2017-01-01")

placebo_year_imports$dollars_per_mton <- (placebo_year_imports$real_dollars / placebo_year_imports$mtons_raw)

results_placebo_years <-  as_tibble(list())

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
    
    # save estimates to dataframe
    plot <- tibble(
      Group = paste("Placebo Year", format(as.Date(k),"%Y"), sep=" "),
      Coefficient = placebo_impacts$wtd.est,
      p = placebo_impacts$p.val,
      High95 = placebo_impacts$avg.tau.high, # 95%
      Low95 = placebo_impacts$avg.tau.low,
      `2016 Quantity` = NA)
    
    results_placebo_years <- rbind(results_placebo_years,plot)
    
  }
}

results_NC <- rbind(results_NC, results_placebo_years)
#######################################################################################################
# 4) Processing
process_data <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & !is.na(process_group)) %>% group_by(month_yr, process_group) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
process_data$dollars_per_mton <- (process_data$real_dollars / process_data$mtons_raw)                                                       
quant <- process_data %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01") %>% group_by(process_group, month_yr) %>% summarise(mtons_raw=sum(mtons_raw))
quant <- quant %>% arrange(desc(mtons_raw))

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
  
   # save estimates to dataframe
  plot <- tibble(
    Group = i,
    Coefficient = processing_impacts$wtd.est,
    p = processing_impacts$p.val,
    High95 = processing_impacts$avg.tau.high, # 95%
    Low95 = processing_impacts$avg.tau.low,
    `2016 Quantity` = sum(post_quant$mtons_raw))
  
  results_NC <- rbind(results_NC,plot)
  
  
}

#######################################################################################################
# 5) NMFS species group
species_imports <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & species_group!="") %>% group_by(species_group, month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))

species_imports$dollars_per_mton <- (species_imports$real_dollars / species_imports$mtons_raw)

# Remove any species with NA in the ts
species_imports <- species_imports %>% group_by(species_group) %>% filter(!any(is.na(dollars_per_mton))) %>% ungroup

quant <- species_imports %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01") %>% group_by(species_group,month_yr) %>% summarise(mtons_raw=sum(mtons_raw))
quant <- quant %>% arrange(desc(mtons_raw))

arima_models_results_species <- as_tibble(list())
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
  
  # save estimates to dataframe
  plot <- tibble(
    Group = i,
    Coefficient = fpi_impacts$wtd.est,
    p = fpi_impacts$p.val,
    High95 = fpi_impacts$avg.tau.high, # 95%
    Low95 = fpi_impacts$avg.tau.low,
    `2016 Quantity` = sum(post_quant$mtons_raw))
  
  results_NC <- rbind(results_NC,plot)
  
}



#######################################################################################################
# 7) Save off results table to overleaf
results_NC$`95% Confidence Interval` <- paste(" (",round(results_NC$Low95,3),", ", round(results_NC$High95,3), ")", sep="")
results_NC <- results_NC %>% rename(Estimate=Coefficient) %>% select(`Group`, Estimate, p,`95% Confidence Interval`, `2016 Quantity`)

filename <- paste(output_location, 'Tables/NoComb_modelavg_robust.tex', sep='')
print(xtable(results_NC, digits=4, type = "latex", caption = 'Estimated average per-ton price impact of the U.S. SIMP over 2017 using only
the AICc-minimizing model. The first row reports the aggregate impact of the SIMP for all first-wave species. The subsequent results are for our placebo species test, the 5 placebo year tests, product forms, and NMFS species groups. Estimates for $\\widehat{\\bar{\\tau}}$ are reported with
p-values associated with the hypothesis test that $\\widehat{\\bar{\\tau}}=0$. We also include bootstrapped 95%
confidence intervals on $\\widehat{\\bar{\\tau}}$ and quantity imported in 2016.', label='table:nc_robust'), 
      tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top',
      file = filename)
