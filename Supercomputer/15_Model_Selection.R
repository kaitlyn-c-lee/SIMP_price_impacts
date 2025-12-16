#######################################################################################################  
###################################  Run Code for SIMP Impacts Paper ###################################
# 1) All SIMP
# 2) Placebo species
# 3) Placebo years
# 4) Processing
# 5) FPI species
#######################################################################################################
# Model selection
source("Functions_ModelSelection.R")

#######################################################################################################
# 1) All SIMP
simp_species_imports <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone") %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
quant_2017 <- simp_species_imports %>% filter(month_yr>="2017-01-01")

simp_species_imports$dollars_per_mton <- (simp_species_imports$real_dollars / simp_species_imports$mtons_raw)
post_quant <- simp_species_imports %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01")

data <- log(ts(simp_species_imports$dollars_per_mton, frequency=12, start=c(2012,1)))
n<-length(data)
dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))

selection_allSIMP <- ModelSelectionManual(y = data, dates = dates, int.date = int.date,ic="aicc",auto.args = arima_args, xreg =xreg_vars, n.cores=n_cores)

write_csv(selection_allSIMP, 'Output/all_SIMP.csv') # save 

rm(list=c("selection_allSIMP"))

#######################################################################################################
# 2) Placebo species
placebo_species <- imports %>% filter(!species_group %in% simp_groups & species_group!="Unidentified Species" & species_group!="Whitefish" 
                                      & !str_detect(species_group, "Shellfish") & !str_detect(species_group, "Groundfish") & !str_detect(species_group, "Crustaceans")
                                      & !species_group %in% simp_adj_groups & !str_detect(species_group, "Crab") & species_group!="Rays Skates") %>% group_by(species_group) %>% summarise()

non_simp_species_imports <- imports %>% filter(species_group %in% placebo_species$species_group) %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
print(paste(unlist(placebo_species), collapse =", "))
# print(xtable(placebo_species_save, type = "latex", caption = 'List of placebo species', label='table:placebo_species_list'), tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top', file = "/Users/kaitlynmalakoff/Dropbox (ASU)/Apps/Overleaf/Blue Crab/Tables/placebo_species_list.tex")

non_simp_species_imports$dollars_per_mton <- (non_simp_species_imports$real_dollars / non_simp_species_imports$mtons_raw)
post_quant <- non_simp_species_imports %>% filter(month_yr>="2016-01-01" & month_yr<"2017-01-01")

data <- log(ts(non_simp_species_imports$dollars_per_mton, frequency=12, start=c(2012,1)))
n<-length(data)
dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
selection_placebospecies <- ModelSelectionManual(y = data, dates = dates, int.date = int.date,ic="aicc",auto.args = arima_args, xreg =xreg_vars, n.cores=n_cores)

write_csv(selection_placebospecies, 'Output/placebospecies.csv') # save 

rm(list=c("selection_placebospecies"))
#######################################################################################################
# 3) Placebo years 
placebo_year_imports <- imports_placebo_yrs %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone") %>% group_by(month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
placebo_year_imports <- placebo_year_imports %>% filter(month_yr>= "2007-01-01" & month_yr<"2017-01-01")

placebo_year_imports$dollars_per_mton <- (placebo_year_imports$real_dollars / placebo_year_imports$mtons_raw)

for(k in as.character(unique(placebo_year_imports$month_yr))){
  if(k >="2012-01-01" & k <"2017-01-01" & str_detect(k, "-01-01")){
    start_date <- as.Date(k) %m-% years(5) # use 5 years to fit model
    end_date <- as.Date(k) %m+% years(1) # look at impacts one year out
    ts_start <- as.numeric(format(start_date,"%Y"))
    
    data <- placebo_year_imports %>% filter(month_yr<end_date & month_yr>=start_date) %>% arrange(month_yr)
    
    data <- log(ts(data$dollars_per_mton, frequency=12, start=c(ts_start,1)))
    
    # specify covariates
    covars_placeboyrs <- covars %>% filter(month_yr<end_date & month_yr>=start_date)
    xreg_vars_placeboyrs <- covars_placeboyrs %>% select(all_of(xreg_vars_list))
    
    n<-length(data)
    dates <- seq.Date(from = as.Date(start_date), by = "months", length.out = n)
    start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
    
    selection <- ModelSelectionManual(y = data, dates = dates, int.date = k,ic="aicc",auto.args = arima_args, xreg =xreg_vars_placeboyrs, n.cores=n_cores)
    
    filename <- paste('ModelSelection/placeboyear_', format(as.Date(k),"%Y"), '.csv', sep='')
    write_csv(selection, filename) # save 
    
    rm(list=c("selection"))
  }
}

#######################################################################################################
# 4) Processing
process_data <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & !is.na(process_group)) %>% group_by(month_yr, process_group) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
process_data$dollars_per_mton <- (process_data$real_dollars / process_data$mtons_raw)                                                       

for(i in unique(process_data$process_group)){
  data <- process_data %>% filter(process_group==i) %>% arrange(month_yr)
  data <- log(ts(data$dollars_per_mton, frequency=12, start=c(2012,1)))
  
  n<-length(data)
  dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
  start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
  
  selection <- ModelSelectionManual(y = data, dates = dates, int.date = int.date,ic="aicc",auto.args = arima_args, xreg =xreg_vars, n.cores=n_cores)

  filename <- paste( 'Output/', i, '.csv', sep='')
  write_csv(selection, filename) # save 
  
  rm(list=c("selection"))
}

#######################################################################################################
# 7) NMFS species group
species_imports <- imports %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & species_group!="" & !is.na(process_group)) %>% group_by(species_group, month_yr) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))

species_imports$dollars_per_mton <- (species_imports$real_dollars / species_imports$mtons_raw)

# Remove any species with NA in the ts
species_imports <- species_imports %>% group_by(species_group) %>% filter(!any(is.na(dollars_per_mton))) %>% ungroup

for(i in unique(species_imports$species_group)){
  
  data <- species_imports %>% filter(species_group==i) %>% arrange(month_yr)
  data <- log(ts(data$dollars_per_mton, frequency=12, start=c(2012,1)))
  
  n<-length(data)
  dates <- seq.Date(from = as.Date("2012-01-01"), by = "months", length.out = n)
  start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))
  
  selection <- ModelSelectionManual(y = data, dates = dates, int.date = int.date,ic="aicc",auto.args = arima_args, xreg =xreg_vars, n.cores=n_cores)
  
  filename <- paste('Output/', i, '.csv', sep='')
  write_csv(selection, filename) # save 
  
  
  rm(list=c("selection"))
}

