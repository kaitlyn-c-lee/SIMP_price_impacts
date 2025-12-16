#######################################################################################################  
######################################  Create 03_HTS_trade_SIMP ######################################
# 1) Load NOAA trade csv files by year, append, clean var names and merge to NMFS species group linkage table
# 2) Convert volume to live weight metric tons using EUMOFA CFs
# 3) Convert value to 2019 USD
# 4) Load in SIMP treatment by HTS code (proposed, final rule, updated 2021)
# 5) Edit country names to match FAO
# 6) Save to .dta: 03_HTS_trade_SIMP.dta
#######################################################################################################

# clear R environment 
# rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

library(pacman)
p_load(tidyverse, ggpubr, dplyr, stringr, haven, readxl, rjson, jtools, readr, writexl, haven, plyr) 

CPI_yr <- 2019
CPI_month <- "01"

#######################################################################################################
## Run files to create linkage tables first ##
noaa_imp_location_yr <- "/Users/kaitlynmalakoff/ASU Dropbox/Kaitlyn Lee/US_trade_databases/Source Data/NOAA_imports/byyear" 
noaa_imp_location_species <- "/Users/kaitlynmalakoff/ASU Dropbox/Kaitlyn Lee/US_trade_databases/Source Data/NOAA_imports/byspeciesgroup" 

# this data is found here: https://raw.githubusercontent.com/kaitlyn-c-lee/seafood-traceability-design/tree/main/Source%20Data/NOAA_imports

source("Linkage_HTS_nmfs_group.R")
source("Linkage_HTS_CN8.R")

#######################################################################################################
# 1) Load NOAA import csv files by year, append, clean var names and merge to NMFS species group linkage table
library(plyr)
filenames <- list.files(noaa_imp_location_yr, pattern="*.csv", full.names=TRUE) 
data_list <- lapply(seq_along(filenames), function(x) transform(read_csv(filenames[x], skip=1), file = filenames[x]))

noaa_imports <- ldply(data_list, data.frame)
noaa_imports$trade_flow <- "IMP"

# merge into one file
noaa_data <- noaa_imports

rm(data_list,noaa_imports) # remove unneeded files

detach(package:plyr) # remove package needed for last command, interferes with dplyr

# Rename variables, keep only needed
noaa_data <- noaa_data %>% rename(month=Month.number, year=Year, country= Country.Name, volume=Volume..kg., value=Value..USD.)

noaa_data <- noaa_data %>% select(month, year, country, Product.Name, HTS.Number, US.Customs.District.Name, trade_flow, volume,value)

# Merge to species group linkage table 
species_groups <- read_excel("Output Data/link_HTS_speciesgroup.xlsx")
species_groups <- species_groups %>% select(HTS.Number,species_group)

trade_data <- merge(noaa_data, species_groups, all.x=T)

byproduct_hts <- read_excel("Output Data/byproducts_hts_speciesgroup.xlsx")
trade_data <- trade_data %>% filter(!HTS.Number %in% byproduct_hts$HTS.Number)

trade_data$species_group <- if_else(is.na(trade_data$species_group), "species_unidentified", trade_data$species_group)

# save HTS code to species group for SI
#SI_hts <- trade_data %>% group_by(HTS.Number, Product.Name, species_group) %>% summarise()
#write_xlsx(SI_hts, "Output Data/all_link_HTS_speciesgroup.xlsx")

#######################################################################################################
# 2) Convert volume to live weight metric tons using EUMOFA CFs
# Load in linked CN8 to HTS and CF table
cf_table <- read_excel("Output Data/us_hts_cn8.xlsx")
cf_table <- cf_table %>% select(HTS.Number, CF)

# Merge to NOAA trade data
trade_data <- merge(trade_data, cf_table, by="HTS.Number") 

# Convert to live weight
trade_data$kg_live_wgt <- trade_data$volume * trade_data$CF

# Convert to live weight metric tons
trade_data$mtons_live_wgt <- trade_data$kg_live_wgt / 1000

# keep only needed vars
trade_data <- trade_data %>% select(!c(CF, kg_live_wgt))

# convert raw volume to mtons
trade_data$mtons_raw <- trade_data$volume / 1000
trade_data <- trade_data %>% select(!volume)

#######################################################################################################
# 3) Convert value to 2019 USD
CPI <- read_csv("Source Data/CPIAUCSL.csv") # 01-01-2019. = 100

# Clean CPI data
CPI$month <- format(as.Date(CPI$DATE), "%m") # extract month
CPI$year <- format(as.Date(CPI$DATE), "%Y") # extract year

CPI <- CPI %>% select(month, year, CPIAUCSL_NBD20190101)

CPI$CPIAUCSL_NBD20190101 <- CPI$CPIAUCSL_NBD20190101 / 100

# Merge to NOAA trade data
imports_realUSD <- merge(trade_data, CPI, by = c("month", "year"))

cpi_base <- CPI %>% filter(year==CPI_yr & month==CPI_month) %>% select(CPIAUCSL_NBD20190101)
cpi_base <- cpi_base$CPIAUCSL_NBD20190101

imports_realUSD$real_dollars <- imports_realUSD$value *(cpi_base / imports_realUSD$CPIAUCSL_NBD20190101)

trade_data <- imports_realUSD # rename dataset to match code

# keep only needed vars
trade_data <- trade_data %>% select(!c(CPIAUCSL_NBD20190101))
trade_data <- trade_data %>% rename(nominal_dollars =value)

#######################################################################################################
# 4) Load in SIMP treatment by HTS code (final rule)
simp_hts_final <- read_excel("Source Data/manual_HTS_codes_from_final_rule.xlsx") # load
trade_data$final_SIMP <- if_else(trade_data$HTS.Number %in% simp_hts_final$`HTS code`, 1, 0)

#######################################################################################################
# 5) Edit country names to match FAO
trade_data$country <- tolower(trade_data$country)
trade_data$country <- str_replace_all(trade_data$country, "antigua & barbuda", "antigua and barbuda")
trade_data$country <- str_replace_all(trade_data$country, "brunei", "brunei darussalam")
trade_data$country <- str_replace_all(trade_data$country, "burma", "myanmar")
trade_data$country <- str_replace_all(trade_data$country, "cape verde", "cabo verde")
trade_data$country <- str_replace_all(trade_data$country, "china - hong kong", "china, hong kong sar")
trade_data$country <- str_replace_all(trade_data$country, "faroe is.", "faroe islands")
trade_data$country <- str_replace_all(trade_data$country, "iran", "iran (islamic rep. of)")
trade_data$country <- str_replace_all(trade_data$country, "ivory coast", "côte d'ivoire")
trade_data$country <- str_replace_all(trade_data$country, "maldive is.", "maldives")
trade_data$country <- str_replace_all(trade_data$country, "marshall is.", "marshall islands")
trade_data$country <- str_replace_all(trade_data$country, "reunion", "réunion")
trade_data$country <- str_replace_all(trade_data$country, "solomon is.", "solomon islands")
trade_data$country <- str_replace_all(trade_data$country, "south korea", "korea, republic of")
trade_data$country <- str_replace_all(trade_data$country, "st.vincent-grenadine", "saint vincent/grenadines")
trade_data$country <- str_replace_all(trade_data$country, "taiwan", "taiwan province of china")
trade_data$country <- str_replace_all(trade_data$country, "tokelau is.", "tokelau")
trade_data$country <- str_replace_all(trade_data$country, "trinidad & tobago", "trinidad and tobago")
trade_data$country <- str_replace_all(trade_data$country, "venezuela", "venezuela, boliv rep of")
trade_data$country <- str_replace_all(trade_data$country, "western samoa", "samoa")
trade_data$country <- str_replace_all(trade_data$country, "vietnam", "viet nam")
trade_data$country <- str_replace_all(trade_data$country, "china - macao", "china, macao sar")
trade_data$country <- str_replace_all(trade_data$country, "congo \\(brazzaville\\)", "congo")
trade_data$country <- str_replace_all(trade_data$country, "french pacific is.", "french polynesia")
trade_data$country <- str_replace_all(trade_data$country, "congo \\(kinshasa\\)", "congo, dem. rep. of the")
trade_data$country <- str_replace_all(trade_data$country, "jamaica,turks,caicos", "turks and caicos is.")
trade_data$country <- str_replace_all(trade_data$country, "neth.antilles-aruba", "aruba")
trade_data$country <- str_replace_all(trade_data$country, "netherlands", "netherlands (kingdom of the)")
trade_data$country <- str_replace_all(trade_data$country, "serbia-montenegro", "serbia and montenegro")
trade_data$country <- str_replace_all(trade_data$country, "tanzania", "tanzania, united rep. of")
trade_data$country <- str_replace_all(trade_data$country, "ussr", "russian federation")
trade_data$country <- str_replace_all(trade_data$country, "venezuela, boliv rep of", "venezuela (boliv rep of)")
trade_data$country <- str_replace_all(trade_data$country, "yemen \\(aden\\)", "yemen")
trade_data$country <- str_replace_all(trade_data$country, "bolivia", "bolivia (plurinat.state)")
trade_data$country <- str_replace_all(trade_data$country, "laos", "lao people's dem. rep.")
trade_data$country <- str_replace_all(trade_data$country, "falkland is.", "falkland is.(malvinas)")
trade_data$country <- str_replace_all(trade_data$country, "cayman is.", "cayman islands")
trade_data$country <- str_replace_all(trade_data$country, "syria", "syrian arab republic")
trade_data$country <- str_replace_all(trade_data$country, "turks & caicos is.", "turks and caicos is.")
trade_data$country <- str_replace_all(trade_data$country, "germany \\(east\\)", "germany")
trade_data$country <- str_replace_all(trade_data$country, "central african rep.", "central african republic")
trade_data$country <- str_replace_all(trade_data$country, "fed states of micron", "micronesia (fed. states)")
trade_data$country <- str_replace_all(trade_data$country, "british virgin is.", "british virgin islands")
trade_data$country <- str_replace_all(trade_data$country, "netherlands \\(kingdom of the\\) antilles", "netherlands antilles")
trade_data$country <- str_replace_all(trade_data$country, "st.kitts-nevis", "saint kitts and nevis")
trade_data$country <- str_replace_all(trade_data$country, "st.lucia", "saint lucia")
trade_data$country <- str_replace_all(trade_data$country, "moldova", "moldova, republic of")
trade_data$country <- str_replace_all(trade_data$country, "bosnia-hercegovina", "bosnia and herzegovina")
trade_data$country <- str_replace_all(trade_data$country, "cook is.", "cook islands")
trade_data$country <- str_replace_all(trade_data$country, "czech republic", "czechia")
trade_data$country <- str_replace_all(trade_data$country, "serbia & kosovo", "serbia")
trade_data$country <- str_replace_all(trade_data$country, "swaziland", "eswatini")
trade_data$country <- str_replace_all(trade_data$country, "br.indian ocean ter.", "british indian ocean ter")
trade_data$country <- str_replace_all(trade_data$country, "french southern ter.", "french southern terr")
trade_data$country <- str_replace_all(trade_data$country, "sao tome & principe", "sao tome and principe")
trade_data$country <- str_replace_all(trade_data$country, "st.helena", "saint helena/asc./trist.")
trade_data$country <- str_replace_all(trade_data$country, "st.pierre & miquelon", "st. pierre and miquelon")
trade_data$country <- str_replace_all(trade_data$country, "serbia & kosovo", "serbia")
trade_data$country <- str_replace_all(trade_data$country, "wallis & futuna", "wallis and futuna is.")
trade_data$country <- str_replace_all(trade_data$country, "turkey", "turkiye")

#######################################################################################################
# 6) Save to .dta: 03_HTS_trade_SIMP.dta
# rename vars to save
trade_data <- trade_data %>% rename(hts_code=HTS.Number, product_name= Product.Name, customs_district= US.Customs.District.Name)

# Save to dta
write_dta(trade_data, "Output Data/03_HTS_trade_SIMP.dta")

source("Linkage_HTS_2018_2016.R")
