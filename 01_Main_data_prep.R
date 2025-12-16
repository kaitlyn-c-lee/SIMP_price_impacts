#######################################################################################################  
###################################  Run Code for SIMP Impacts Paper ###################################
# 1) Prep US Trade Data
# 2) Run summary stats for US trade data
# 3) Download covariate data for SARIMAX
# 4) Clean covariates and run ADF tests
# 5) Save data to upload to supercomputer for model selection
#######################################################################################################
# clear R environment 
rm(list = ls())

# Set working directory and input/output folder names
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# Make directories for outputting figures and tables

# uncomment on first run
# devtools::install_github("mikeasilva/blsAPI")
# devtools::install_github("FMenchetti/CausalArima")

# load packages
library(pacman)
p_load(tidyverse, ggpubr, dplyr, stringr, haven, 
       readxl, rjson, jtools, readr, writexl, haven,ggrepel,usethis,shiny,devtools,
       tidybayes, DescTools, purrr,xtable,tseries,texreg, grid, blsAPI, lubridate,scales, fredr, stats,forecast, foreach, doParallel,qdapRegex,sf,mintyr) 

# Figure + table location
output_location <- 'Output/'

#######################################################################################################
# 1) Prep US Trade Data
source("Code files/11_TradeData_prep.R")

#######################################################################################################
# 2) Run summary stats for US trade data
source("Code files/12_US_Summary_stats.R")

#######################################################################################################
# 3) Download covariate data for SARIMAX
source("Code files/13_CovariatesGet.R")

#######################################################################################################
# 4) Clean covariates and run ADF tests
rm(list=setdiff(ls(), c("imports","simp_groups", "simp_adj_groups", "output_location")))

source("Code files/14_Covariates_ADF.R")

#######################################################################################################
# 5) Save data to upload to supercomputer for model selection
# Import data
save(imports, file = "Supercomputer/imports.RData")

# Covariates 
save(covars, file = "Supercomputer/covars.RData")

