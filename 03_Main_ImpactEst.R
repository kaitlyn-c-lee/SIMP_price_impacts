#######################################################################################################  
###################################  Run Code for SIMP Impacts Paper ###################################
# 1) Load in import data and covariates
# 2) Create forecasts and estimate impacts 
# 3) Output results tables to overleaf
# 4) Using only the minimum model
# 5) Dropping problematic codes in processing result
#######################################################################################################
# clear R environment 
rm(list = ls())

# Set working directory and input/output folder names
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 


# load packages
library(pacman)
p_load(tidyverse, ggpubr, dplyr, stringr, haven, 
       readxl, rjson, jtools, readr, writexl, haven,ggrepel,usethis,shiny,devtools,
       tidybayes, DescTools, purrr,xtable,tseries,texreg, grid, blsAPI, lubridate,scales, fredr, stats,forecast, foreach, doParallel,qdapRegex,sf) 

# Figure + table location
output_location <- 'Output/'

#######################################################################################################
# 1) Load in import data and covariates
load("Supercomputer/imports.RData")
load("Supercomputer/covars.RData")

# specify covariates (contemporaneous)
covars_group <- covars %>% filter(month_yr>="2012-01-01" & month_yr<"2018-01-01") %>% arrange(month_yr)

xreg_vars <- covars_group %>% select(!c(month, year, month_yr))

xreg_vars <- sapply(xreg_vars, as.numeric) # convert to numeric
xreg_vars <- as.data.frame(xreg_vars)

xreg_vars_list <- colnames(xreg_vars)


# Subset data for placebo years
imports_placebo_yrs <- imports
imports <- imports %>% filter(month_yr>= "2012-01-01" & month_yr<"2018-01-01")

# Adding intervention
int.date <- as.Date("2017-01-01")

# specify arguments for auto arima
#arima_args <- c(max.p = 6,max.P=6,max.d = 6,max.D=6,max.q = 6,max.Q=6,seasonal = TRUE,stepwise = FALSE,allowdrift = TRUE,allowmean = TRUE, approximation=FALSE, max.order=24)

# list of simp species groups
simp_groups <- c("Abalone", "Crab: Blue", "Crab: King", "Shark", "Dolphinfish", "Groundfish: Cod", "Grouper", "Sea Cucumber", 
                 "Shrimp", "Snapper", "Swordfish", "Crab: Other", "Tuna: Other", "Tuna: Albacore", 
                 "Tuna: Bluefin", "Tuna: Bigeye", "Tuna: Skipjack", "Tuna: Yellowfin") 

simp_adj_groups <- c("Other Molluscs",  "Other Aquatic Invertebrates", "Groundfish: Other",
                     "Other Crustaceans", "Whitefish")

#######################################################################################################
# 2) Create forecasts and estimate impacts 
source("Code files/Functions_Forecasting.R")
source("Code files/Functions_ImpactEstimation.R")

source("Code files/16_modelavg_results.R")

#######################################################################################################
# 3) Output results tables to overleaf
# Main Results Table
results1 <- results
results1$Estimate <- round(results1$Coefficient,3)
results1$p <- round(results1$p,3)

results1$`95% Confidence Interval` <- paste(" (",round(results1$Low95,3),", ", round(results1$High95,3), ")", sep="")

results1 <- results1 %>% select(`Group`, Estimate, p,`95% Confidence Interval`, `2016 Quantity`)

# Save off table for main results + placebos
main_result1 <-  results1 %>% select(!`2016 Quantity`) %>% filter(Group %in% c("SIMP Species", "Placebo Species", "Placebo Year 2012", "Placebo Year 2013", "Placebo Year 2014", "Placebo Year 2015", "Placebo Year 2016"))
main_result1$Group <- if_else(main_result1$Group=="SIMP Species", "SIMP Price Impact", main_result1$Group)

print(xtable(main_result1, digits=3, type = "latex", caption = 'Estimated average per-ton price impact of the U.S. SIMP over 2017. The first row reports the aggregate impact of the SIMP for all first-wave species. Estimates for $\\widehat{\\bar{\\tau}}$ reported with p-values associated with the hypothesis test that $\\widehat{\\bar{\\tau}}=0$. We also include bootstrapped 95\\% confidence intervals on $\\widehat{\\bar{\\tau}}$.', label='table:main_result'), 
      tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top',
      file = paste(output_location, 'Tables/main_result.tex', sep=''))

# Save off table for mechanisms
results_mechanisms <-  results1 %>% filter(!Group %in% c("SIMP Species", "Placebo Species", "Placebo Year 2012", "Placebo Year 2013", "Placebo Year 2014", "Placebo Year 2015", "Placebo Year 2016"))

print(xtable(results_mechanisms, digits=3, type = "latex", caption = 'Heterogeneity in estimated average price impacts of the U.S. SIMP over 2017. Panel A. reports the price impact of the SIMP for processed and unprocessed products estimated separately. Panel B. reports the price impact of the SIMP for each NMFS species group. Estimates for $\\widehat{\\bar{\\tau}}$ are reported with p-values associated with the hypothesis test that $\\widehat{\\bar{\\tau}}=0$. We also include bootstrapped 95\\% confidence intervals on $\\widehat{\\bar{\\tau}}$ and quantity imported in 2016.', label='table:mechanisms_result'), 
      tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top',
      file = paste(output_location, 'Tables/mechanisms_result.tex', sep=''))


#######################################################################################################
# 4) Using only the minimum model
source("Code files/16_modelavg_results_NoCombination.R")

#######################################################################################################
# 5) Dropping problematic codes in processing result
source("Code files/16_modelavg_results_ProcessingRobust.R")

