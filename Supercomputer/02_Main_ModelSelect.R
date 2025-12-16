######################################################################################################  
##############################  Model selection -- run on supercomputer ##############################
# 1) Load in cleaned trade + covariate data
# 2) Define intervention date and ARIMA arguments
# 6) Main SARIMAX impact estimation for simp species, placebo species, and placebo years. Run het analysis and save model results
# 7) Run Heterogeneity Analysis
# 8) Output tables to overleaf
# 9) By species
######################################################################################################
# clear R environment 
rm(list = ls())

# Set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load packages
library(pacman)
p_load(tidyverse, ggpubr, dplyr, stringr, haven, 
       readxl, rjson, jtools, readr, writexl, haven,ggrepel,usethis,shiny,devtools,
       tidybayes, DescTools, purrr,xtable,tseries,texreg, grid, blsAPI, lubridate,scales, fredr, stats,forecast, foreach, doParallel) 


#######################################################################################################
# 1) Load in cleaned trade + covariate data
load("covars.RData")
load("imports.RData")

simp_groups <- c("Abalone", "Crab: Blue", "Crab: King", "Shark", "Dolphinfish", "Groundfish: Cod", "Grouper", "Sea Cucumber", 
                 "Shrimp", "Snapper", "Swordfish", "Crab: Other", "Tuna: Other", "Tuna: Albacore", 
                 "Tuna: Bluefin", "Tuna: Bigeye", "Tuna: Skipjack", "Tuna: Yellowfin") 

simp_adj_groups <- c("Other Molluscs",  "Other Aquatic Invertebrates", "Groundfish: Other",
                     "Other Crustaceans", "Whitefish")

#######################################################################################################
# 2) Define intervention date and ARIMA arguments
# Subset data for placebo years
imports_placebo_yrs <- imports
imports <- imports %>% filter(month_yr>= "2012-01-01" & month_yr<"2018-01-01")

# Adding intervention
int.date <- as.Date("2017-01-01")

# specify arguments for auto arima
arima_args <- c(max.p = 6,max.P=6,max.d = 6,max.D=6,max.q = 6,max.Q=6,seasonal = TRUE,stepwise = FALSE,allowdrift = TRUE,allowmean = TRUE, approximation=FALSE, max.order=24)

# specify covariates (contemporaneous)
covars_group <- covars %>% filter(month_yr>="2012-01-01" & month_yr<"2018-01-01") %>% arrange(month_yr)

xreg_vars <- covars_group %>% select(!c(month, year, month_yr))

xreg_vars <- sapply(xreg_vars, as.numeric) # convert to numeric
xreg_vars <- as.data.frame(xreg_vars)

xreg_vars_list <- colnames(xreg_vars)


#######################################################################################################
# 6) Model selection -- run on supercomputer, will output csv file for each time series of interest
n_cores=50
source("15_Model_Selection.R")

#######################################################################################################
# 7) Robustness to processing changes in hts codes
source("15_Model_Selection_ProcessingRobust.R")


