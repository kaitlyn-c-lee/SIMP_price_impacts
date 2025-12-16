#######################################################################################################  
##################################### Create linkage_HTS_CN8.xlsx ##################################### 
# 1) Load NOAA import csv files by year, append, clean var names and merge to NMFS species group linkage table
# 2) Get unique HTS code, product name, and species group in NOAA trade data
# 3) Load in EUMOFA CF data table, clean
# 4) Remove products with no CF or where species unspecified, append to byproducts excel file
# 5) Manually assign CN8 code to US HTS code
# 6) Merge assigned us hts codes to EUMOFA data, save to excel 
#######################################################################################################

# clear R environment 
# rm(list = ls())

library(pacman)
p_load(tidyverse, ggpubr, dplyr, stringr, haven, readxl, rjson, jtools, readr, writexl, haven, plyr) 

#######################################################################################################
# 1) Load NOAA import csv files by year, append, clean var names and merge to NMFS species group linkage table
filenames <- list.files(noaa_imp_location_yr, pattern="*.csv", full.names=TRUE)
data_list <- lapply(seq_along(filenames), function(x) transform(read_csv(filenames[x], skip=1), file = filenames[x]))

noaa_imports <- ldply(data_list, data.frame)

# merge into one file
noaa_data <- noaa_imports

detach(package:plyr) # remove package needed for last command, interferes with dplyr

# Merge to species group linkage table 
species_groups <- read_excel("Output Data/link_HTS_speciesgroup.xlsx")
species_groups <- species_groups %>% select(HTS.Number,species_group)

trade_data <- merge(noaa_data, species_groups, by="HTS.Number", all.x=T)

byproduct_hts <- read_excel("Output Data/byproducts_hts_speciesgroup.xlsx")
trade_data <- trade_data %>% filter(!HTS.Number %in% byproduct_hts$HTS.Number)

trade_data$species_group <- if_else(is.na(trade_data$species_group), "species_unidentified", trade_data$species_group)

#######################################################################################################
# 2) Get unique HTS code, product name, and species group in NOAA trade data
us_hts_cn8 <- trade_data %>% group_by(HTS.Number, Product.Name, species_group) %>% summarise()

rm(data_list, noaa_imports, noaa_data, species_groups) # remove un-needed files

#######################################################################################################
# 3) Load in EUMOFA CF data table, clean
EUMOFA_data <- read_excel("Source Data/DM_Annex 7 - CF by CN-8 from 2001 to 2021.xlsx", 
                          sheet = "List of CF")

EUMOFA_data <- EUMOFA_data %>% filter(Year=="2021") # use most recent codes

EUMOFA_data <- EUMOFA_data %>% select(`CN-8`, `CN-8 product name`, CF, Explanation)
EUMOFA_data$`CN-8` <- str_remove_all(EUMOFA_data$`CN-8`, " ")

#######################################################################################################
# 4) Remove products with no CF or where species unspecified, append to byproducts excel file
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(Product.Name =="LIVE BAIT OTHER THAN WORMS"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(str_detect(Product.Name, "CLAM JUICE")))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(Product.Name =="CUTTLEFISH BONE"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(str_detect(Product.Name, "ROE")))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(str_detect(Product.Name, "FOR ANIMAL FEED")))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(str_detect(Product.Name, "SEED")))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(str_detect(Product.Name, "SHRIMP CHIPS")))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(species_group== "caviar"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(Product.Name =="FISH,SHELLFISH NSPF JUICE"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(species_group== "seaweed"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(species_group== "coral"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(Product.Name =="FISH NSPF FERTILIZED EGGS"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(Product.Name =="FISH,SHELLFISH JUICE FROM MEAT"))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(str_detect(Product.Name, "OTHER EDIBLE OFFAL")))
byproduct_hts <- rbind(byproduct_hts, us_hts_cn8 %>% filter(str_detect(Product.Name, "MEAL")))

us_hts_cn8 <- us_hts_cn8 %>% filter(!HTS.Number %in% byproduct_hts$HTS.Number) # remove byproducts 
write_xlsx(byproduct_hts, "Output Data/byproducts_hts_speciesgroup.xlsx") # save to excel

#######################################################################################################
# 5) Manually assign CN8 code to US HTS code
# Default is to assign the live/fresh CF using product group name, then overwrite for specific codes
us_hts_cn8$`CN-8` = ""

# abalone
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="abalone","03078100", us_hts_cn8$`CN-8`)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="abalone" & (str_detect(us_hts_cn8$Product.Name, "FROZEN") | str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE")), "03078300", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="abalone" & (str_detect(us_hts_cn8$Product.Name, "PREPAR") | str_detect(us_hts_cn8$Product.Name, "CANNED")), "16055700", us_hts_cn8$`CN-8`) # prepared/preserved

# anchovy
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="anchovy","03024200", us_hts_cn8$`CN-8`)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="anchovy" & str_detect(us_hts_cn8$Product.Name, "SALTED"), "03056300", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="anchovy" & (str_detect(us_hts_cn8$Product.Name, "CANNED") | str_detect(us_hts_cn8$Product.Name, "PREPAR")), "16041600", us_hts_cn8$`CN-8`) # canned

# atka mackerel, use mackerel codes
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="atka mackerel","03024400", us_hts_cn8$`CN-8`)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="atka mackerel" & str_detect(us_hts_cn8$Product.Name, "FROZEN"), "03035410", us_hts_cn8$`CN-8`) # frozen

# bass
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bass","03028410", us_hts_cn8$`CN-8`) # only import fresh

# bonito
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bonito","16041490", us_hts_cn8$`CN-8`) # only import canned

# butterfish (use mackerel codes)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="butterfish","03035410", us_hts_cn8$`CN-8`) # all imports frozen

# capelin
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="capelin","03035990", us_hts_cn8$`CN-8`) # all imports frozen

# carp
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="carp","03027300", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="carp" & str_detect(us_hts_cn8$Product.Name, "FROZEN"), "03032500", us_hts_cn8$`CN-8`) # frozen

# species unidentified, but include carp. Look up 8-digit codes in EU data and check
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0304390000"), "03043900", us_hts_cn8$`CN-8`) # fillet/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0304510090", "0304510190", "0304510000", "0304510100"), "03045100", us_hts_cn8$`CN-8`) # meat/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0304690000"), "03046900", us_hts_cn8$`CN-8`) # fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0304931005", "0304931010", "0304931090"), "03049310", us_hts_cn8$`CN-8`) # surimi
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0304939000"), "03049390", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0305310100", "0305310000"), "03053100", us_hts_cn8$`CN-8`) # dried/salted/brine
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0305440000", "0305440100"), "03054490", us_hts_cn8$`CN-8`) # smoked
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0305641000", "0305645000", "0305640000"), "03056400", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0305520000"), "03053100", us_hts_cn8$`CN-8`) # dried, maybe 03055200?
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0304930000"), "03049390", us_hts_cn8$`CN-8`) #meat frozen

# catfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="catfish","03027200", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="catfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"), "03032400", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="catfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03043900", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="catfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03046900", us_hts_cn8$`CN-8`) # frozen fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="catfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"), "03045100", us_hts_cn8$`CN-8`) # meat

# clam
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="clam","16055600", us_hts_cn8$`CN-8`) # prepared/preserved
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="clam" & (str_detect(us_hts_cn8$Product.Name, "FROZEN") | str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE")), "03077900", us_hts_cn8$`CN-8`) # other
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="clam" & (str_detect(us_hts_cn8$Product.Name, "LIVE") | str_detect(us_hts_cn8$Product.Name, "FRESH")), "03077100", us_hts_cn8$`CN-8`) # fresh

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("1605560500"), "16055600", us_hts_cn8$`CN-8`) # species unidentified, matched using codes

# cobia 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cobia","03024600", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cobia" & str_detect(us_hts_cn8$Product.Name, "FROZEN"), "03035600", us_hts_cn8$`CN-8`) # frozen

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0305540000"), "03055490", us_hts_cn8$`CN-8`) # species unidentified, matched using codes

# conch
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="conch","03078200", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="conch" & str_detect(us_hts_cn8$Product.Name, "FROZEN"), "03078400", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="conch" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"), "03078800", us_hts_cn8$`CN-8`) # other

# all crab groups, then overwrite for individual species where available
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_crab"),"03063390", us_hts_cn8$`CN-8`)
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_crab") & str_detect(us_hts_cn8$Product.Name, "FROZEN"), "03061490", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_crab") & (str_detect(us_hts_cn8$Product.Name, "PREPAR") | str_detect(us_hts_cn8$Product.Name, "ATC") | str_detect(us_hts_cn8$Product.Name, "CANNED")), "16051000", us_hts_cn8$`CN-8`) # prepared
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_crab") & str_detect(us_hts_cn8$Product.Name, "MEAT"), "16051000", us_hts_cn8$`CN-8`) # crab meat
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_crab") & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"), "03069390", us_hts_cn8$`CN-8`) # dried/salted/brine

# snow and king crab
us_hts_cn8$`CN-8` <- if_else((us_hts_cn8$species_group=="snow_crab" | us_hts_cn8$species_group=="king_crab")& str_detect(us_hts_cn8$Product.Name, "FROZEN") & !str_detect(us_hts_cn8$Product.Name, "MEAT"),"03061410", us_hts_cn8$`CN-8`) # frozen snow/king crab

# crawfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="crawfish","03061910", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="crawfish" & str_detect(us_hts_cn8$Product.Name, "PEELED"),"16054000", us_hts_cn8$`CN-8`) # other (peeled), look up by code

# cuttlefish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cuttlefish","03074290", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cuttlefish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03074399", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cuttlefish" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16055400", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cuttlefish" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03074980", us_hts_cn8$`CN-8`) 

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("1605540500"), "16055400", us_hts_cn8$`CN-8`) # species unidentified, matched using codes

# dolphinfish (use tuna codes)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="dolphin","03023190", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="dolphin" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048990", us_hts_cn8$`CN-8`) # use other fish frozen fillets

# eels
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="eels","03027400", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="eels" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03032600", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="eels" & (str_detect(us_hts_cn8$Product.Name, "OIL") | str_detect(us_hts_cn8$Product.Name, "PREPAR")),"16041700", us_hts_cn8$`CN-8`) # prepared

# flounder
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="flounder_flatfish","03022980", us_hts_cn8$`CN-8`) # fresh flat fish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="flounder_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03033910", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="flounder_flatfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044300", us_hts_cn8$`CN-8`) # fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="flounder_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03048330", us_hts_cn8$`CN-8`) # frozen fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="flounder_flatfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat

# halibut
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="halibut_flatfish","03022110", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="halibut_flatfish" & str_detect(us_hts_cn8$Product.Name, "ATLANTIC"),"03022130", us_hts_cn8$`CN-8`) # atlantic fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="halibut_flatfish" & str_detect(us_hts_cn8$Product.Name, "PACIFIC"),"03022190", us_hts_cn8$`CN-8`) # pacific fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="halibut_flatfish" & str_detect(us_hts_cn8$Product.Name, "ATLANTIC") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03033130", us_hts_cn8$`CN-8`) # atlantic frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="halibut_flatfish" & str_detect(us_hts_cn8$Product.Name, "PACIFIC") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03033190", us_hts_cn8$`CN-8`) # atlantic frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="halibut_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"), "03033110", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="halibut_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03048390", us_hts_cn8$`CN-8`) # frozen fillet

# plaice
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="plaice_flatfish","03022200", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="plaice_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03033200", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="plaice_flatfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03048310", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="plaice_flatfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat

# sole
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sole_flatfish","03022300", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sole_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03033300", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sole_flatfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044300", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sole_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03048390", us_hts_cn8$`CN-8`) # frozen fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sole_flatfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat

# turbot
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="turbot_flatfish","03022400", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="turbot_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03033400", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="turbot_flatfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="turbot_flatfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044300", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="turbot_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03048390", us_hts_cn8$`CN-8`) # frozen fillet

# other flatfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_flatfish","03022980", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03033985", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_flatfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044300", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_flatfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03048390", us_hts_cn8$`CN-8`) # frozen fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_flatfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat

# cod 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish","03025190", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036390", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "ATLANTIC"),"03025110", us_hts_cn8$`CN-8`) # atlantic
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "ATLANTIC"), "03036310", us_hts_cn8$`CN-8`) # frozen atlantic
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044410", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "DRIED"),"03055110", us_hts_cn8$`CN-8`) # dried
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "SALTED"),"03055190", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03047190", us_hts_cn8$`CN-8`) # frozen fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "SALTED") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03053219", us_hts_cn8$`CN-8`) # salted fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "DRIED") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03053219", us_hts_cn8$`CN-8`) # dried fillet

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045300", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cod_groundfish" & str_detect(us_hts_cn8$Product.Name, "SMOKED"),"03054980", us_hts_cn8$`CN-8`) # smoked

# cusk, use "molva spp." which is from the same family
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cusk_groundfish","03025940", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cusk_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036980", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cusk_groundfish" & str_detect(us_hts_cn8$Product.Name, "SALTED"),"03056910", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cusk_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044990", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cusk_groundfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cusk_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"), "03047980", us_hts_cn8$`CN-8`) # frozen fillet

# haddock
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="haddock_groundfish","03025200", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="haddock_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036400", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="haddock_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044490", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="haddock_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03047200", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="haddock_groundfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03049530", us_hts_cn8$`CN-8`) # meat

# hake
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="hake_groundfish","03025419", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="hake_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036619", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="hake_groundfish" & str_detect(us_hts_cn8$Product.Name, "SALTED"),"03056910", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="hake_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044490", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="hake_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03047419", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="hake_groundfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03049550", us_hts_cn8$`CN-8`) # meat

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="hake_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET")& str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "UROPHYCIS"),"03047490", us_hts_cn8$`CN-8`) # fillet UROPHYCIS

# ocean perch
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish","03028939", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038939", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish" & str_detect(us_hts_cn8$Product.Name, "ATLANTIC"),"03028931", us_hts_cn8$`CN-8`) # atlantic
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044950", us_hts_cn8$`CN-8`) # fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish" & str_detect(us_hts_cn8$Product.Name, "ATLANTIC") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038931", us_hts_cn8$`CN-8`) # atlantic frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish" & str_detect(us_hts_cn8$Product.Name, "ATLANTIC") & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03048921", us_hts_cn8$`CN-8`) # atlantic frozen fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03048929", us_hts_cn8$`CN-8`) # fillet frozen

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="ocean perch_groundfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03049929", us_hts_cn8$`CN-8`) # meat

# pollock
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish","03025930", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036950", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "ALASKA"),"03025500", us_hts_cn8$`CN-8`) # alaska
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "ALASKA") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036700", us_hts_cn8$`CN-8`) # alaska frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044490", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "ALASKA") & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03047500", us_hts_cn8$`CN-8`) # alaska fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "ALASKA") & (str_detect(us_hts_cn8$Product.Name, "SURIMI") | str_detect(us_hts_cn8$Product.Name, "MINCED")),"03049410", us_hts_cn8$`CN-8`) # alaska surimi
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "CANNED"),"16041995", us_hts_cn8$`CN-8`) # canned
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03049490", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "SALTED") & !str_detect(us_hts_cn8$Product.Name, "FILLET"),"03056910", us_hts_cn8$`CN-8`) # salted, excl. fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pollock_groundfish" & str_detect(us_hts_cn8$Product.Name, "FRESH") & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044490", us_hts_cn8$`CN-8`) # fresh fillet

# whiting and blue whiting
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="whiting_groundfish","03047930", us_hts_cn8$`CN-8`) # only import frozen fillets

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="blue whiting_groundfish","03025600", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="blue whiting_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036810", us_hts_cn8$`CN-8`) # frozen

# other groundfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish","03025990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036990", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044490", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045300", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish" & str_detect(us_hts_cn8$Product.Name, "SURIMI"),"03049510", us_hts_cn8$`CN-8`) # surimi
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish" & str_detect(us_hts_cn8$Product.Name, "DRIED"),"03055390", us_hts_cn8$`CN-8`) # dried
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish" & str_detect(us_hts_cn8$Product.Name, "DRIED") & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03053290", us_hts_cn8$`CN-8`) # dried fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_groundfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03047990", us_hts_cn8$`CN-8`) # frozen fillet

# grouper (use cod codes)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="grouper","03025190", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="grouper" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03036390", us_hts_cn8$`CN-8`) # frozen

# herring
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring","03024100", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & str_detect(us_hts_cn8$Product.Name, "SALTED"),"03056100", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03035100", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048600", us_hts_cn8$`CN-8`) # frozen fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03053990", us_hts_cn8$`CN-8`) # other fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & str_detect(us_hts_cn8$Product.Name, "SMOKED"),"03054200", us_hts_cn8$`CN-8`) # smoked, incl. fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & (str_detect(us_hts_cn8$Product.Name, "PICKLED") | str_detect(us_hts_cn8$Product.Name, "KIPPERED") | str_detect(us_hts_cn8$Product.Name, "PREPAR")),"16041299", us_hts_cn8$`CN-8`) # PICKLED

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & (str_detect(us_hts_cn8$Product.Name, "CANNED") |str_detect(us_hts_cn8$Product.Name, "ATC")),"16041291", us_hts_cn8$`CN-8`) # preparations in containers
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="herring" & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "PICKLED"),"16041210", us_hts_cn8$`CN-8`) # pickeled fillets

# horse mackerel jack
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="horse mackerel_jack","03024590", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="horse mackerel_jack" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03035590", us_hts_cn8$`CN-8`) # frozen

# jellyfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="jellyfish","03083050", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="jellyfish" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16056300", us_hts_cn8$`CN-8`) # prepared

# krill (using "other crustaceans")
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="krill","03063990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="krill" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03061990", us_hts_cn8$`CN-8`) # frozen

# lingcod (use cod code)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lingcod","03025190", us_hts_cn8$`CN-8`) # fresh

# lobster
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster","03063210", us_hts_cn8$`CN-8`) # live
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster" & str_detect(us_hts_cn8$Product.Name, "FRESH"),"03063291", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03061290", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster" & (str_detect(us_hts_cn8$Product.Name, "PREPAR") | str_detect(us_hts_cn8$Product.Name, "TAILS")| str_detect(us_hts_cn8$Product.Name, "CANNED")),"16053090", us_hts_cn8$`CN-8`) # prepared
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster" & str_detect(us_hts_cn8$Product.Name, "MEAT") ,"16053010", us_hts_cn8$`CN-8`) # meat

# HOMARUS lobster
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "HOMARUS"),"03063291", us_hts_cn8$`CN-8`) # HOMARUS fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "HOMARUS") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03061290", us_hts_cn8$`CN-8`) # HOMARUS frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "HOMARUS") & str_detect(us_hts_cn8$Product.Name, "LIVE"),"03063210", us_hts_cn8$`CN-8`) # HOMARUS live
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "HOMARUS") & (str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE") | str_detect(us_hts_cn8$Product.Name, "ATC")),"03069290", us_hts_cn8$`CN-8`) # HOMARUS other

# Norway 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "NORWAY"),"03063400", us_hts_cn8$`CN-8`) # NORWAY fresh

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "NORWAY") & (str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE")),"03069400", us_hts_cn8$`CN-8`) # norway other
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "NORWAY") & (str_detect(us_hts_cn8$Product.Name, "FROZEN")),"03061500", us_hts_cn8$`CN-8`) # norway frozen

# rock
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "ROCK")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03061190", us_hts_cn8$`CN-8`) # NORWAY fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="lobster"& str_detect(us_hts_cn8$Product.Name, "ROCK") & (str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE")),"03069100", us_hts_cn8$`CN-8`) # rock dried

# mackerel
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mackerel","03024400", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mackerel" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03035410", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mackerel" & str_detect(us_hts_cn8$Product.Name, "SMOKED"),"03054930", us_hts_cn8$`CN-8`) # smoked
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mackerel" & str_detect(us_hts_cn8$Product.Name, "SALTED"),"03056980", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mackerel" & str_detect(us_hts_cn8$Product.Name, "FILLET DRIED/SALTED/BRINE"),"16041511", us_hts_cn8$`CN-8`) # fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mackerel" & (str_detect(us_hts_cn8$Product.Name, "PREPAR") | str_detect(us_hts_cn8$Product.Name, "CANNED")),"16041590", us_hts_cn8$`CN-8`) # prep

# monkfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="monkfish","03028950", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="monkfish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038965", us_hts_cn8$`CN-8`) # frozen

# mullet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mullet","03038990", us_hts_cn8$`CN-8`) # all frozen, look up using code

# mussels
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mussels","03073190", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mussels" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03073290", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mussels" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16055390", us_hts_cn8$`CN-8`) # prep
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="mussels" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03073980", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE

# nile perch
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="nile perch","03027900", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="nile perch" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03032900", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="nile perch" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03043300", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="nile perch" & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03046300", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="nile perch" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045100", us_hts_cn8$`CN-8`) # meat

# octopus
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="octopus","03075100", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="octopus" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03075200", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="octopus" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16055500", us_hts_cn8$`CN-8`) # PREPAR
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="octopus" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03075900", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE

# orange roughy 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="orange roughy","03048990", us_hts_cn8$`CN-8`) # only frozen fillet, look up code

# oysters
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="oysters","03071190", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="oysters" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03071200", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="oysters" & (str_detect(us_hts_cn8$Product.Name, "PREPAR") | str_detect(us_hts_cn8$Product.Name, "CANNED")),"16055100", us_hts_cn8$`CN-8`) # prepar
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="oysters" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE") ,"03071900", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE

# perch nspf
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="perch nspf","03027900", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="perch nspf" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="perch nspf" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03043900", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="perch nspf" & str_detect(us_hts_cn8$Product.Name, "FILLET") &str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048990", us_hts_cn8$`CN-8`) # fillet frozen

# pickerel (using perch / lookup codes)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pickerel","03027900", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pickerel" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat, look up code
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pickerel" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044990", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pickerel" & str_detect(us_hts_cn8$Product.Name, "FILLET") &str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048990", us_hts_cn8$`CN-8`) # fillet frozen

# pike (using perch / lookup codes)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pike","03027900", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pike" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat, look up code
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pike" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044990", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="pike" & str_detect(us_hts_cn8$Product.Name, "FILLET") &str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048990", us_hts_cn8$`CN-8`) # fillet frozen

# rays and skates
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="rays skates","03028200", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="rays skates" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038200", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="rays skates" & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044800", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="rays skates" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03049700", us_hts_cn8$`CN-8`) # meat, look up code
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="rays skates" & str_detect(us_hts_cn8$Product.Name, "MEAT") & str_detect(us_hts_cn8$Product.Name, "FRESH"),"03045700", us_hts_cn8$`CN-8`) # meat, look up code

# sablefish (look up codes / other fish)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sablefish","03028990", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sablefish" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038990", us_hts_cn8$`CN-8`) # frozen

# all salmon (use pacific), then overwrite for individual species after
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon"),"03021900", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03031200", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & (str_detect(us_hts_cn8$Product.Name, "FILLET") | str_detect(us_hts_cn8$Product.Name, "STEAKS")),"03044100", us_hts_cn8$`CN-8`) # fillet/steaks
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & (str_detect(us_hts_cn8$Product.Name, "FILLET") | str_detect(us_hts_cn8$Product.Name, "STEAKS")) & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048100", us_hts_cn8$`CN-8`) # frozen fillet/steaks
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & str_detect(us_hts_cn8$Product.Name, "SALTED"),"03056950", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & str_detect(us_hts_cn8$Product.Name, "SMOKED"),"03054100", us_hts_cn8$`CN-8`) # smoked
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & str_detect(us_hts_cn8$Product.Name, "CANNED"),"16041100", us_hts_cn8$`CN-8`) # canned
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16041100", us_hts_cn8$`CN-8`) # prep
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$species_group, "_salmon") & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045200", us_hts_cn8$`CN-8`) # meat

# atlantic and danube salmon
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="atlantic_salmon" | us_hts_cn8$species_group=="danube_salmon","03021400", us_hts_cn8$`CN-8`) # live/fresh
us_hts_cn8$`CN-8` <- if_else((us_hts_cn8$species_group=="atlantic_salmon" | us_hts_cn8$species_group=="danube_salmon") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03031300", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else((us_hts_cn8$species_group=="atlantic_salmon" | us_hts_cn8$species_group=="danube_salmon") & (str_detect(us_hts_cn8$Product.Name, "FILLET") | str_detect(us_hts_cn8$Product.Name, "STEAK")),"03044100", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else((us_hts_cn8$species_group=="atlantic_salmon" | us_hts_cn8$species_group=="danube_salmon") & (str_detect(us_hts_cn8$Product.Name, "FILLET") | str_detect(us_hts_cn8$Product.Name, "STEAK"))& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048100", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else((us_hts_cn8$species_group=="atlantic_salmon" | us_hts_cn8$species_group=="danube_salmon") & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045200", us_hts_cn8$`CN-8`) # meat

# sockeye salmon
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sockeye_salmon" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03031100", us_hts_cn8$`CN-8`) # frozen

# sardine
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sardine","03024330", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sardine" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03035330", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sardine" & str_detect(us_hts_cn8$Product.Name, "CANNED"),"16041311", us_hts_cn8$`CN-8`) # canned
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sardine" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16041311", us_hts_cn8$`CN-8`) # prep

# sauger (look up codes, freshwater fish)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sauger","03038910", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sauger" & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048910", us_hts_cn8$`CN-8`) # fillet frozen

# scallops
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="scallops","03072100", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="scallops" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03072290", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="scallops" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16055200", us_hts_cn8$`CN-8`) # prep
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="scallops" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03072900", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE

# scorpionfish, use sea bass
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="scorpionfish","03038490", us_hts_cn8$`CN-8`) # frozen

# sea bass
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea bass","03028490", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea bass" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038490", us_hts_cn8$`CN-8`) # frozen

# sea cucumber
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea cucumber","03081100", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea cucumber" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03081200", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea cucumber" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16056100", us_hts_cn8$`CN-8`) # prep
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea cucumber" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03081900", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE

# sea urchin
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea urchin","03082100", us_hts_cn8$`CN-8`) # live
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea urchin" & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03082200", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea urchin" & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16056200", us_hts_cn8$`CN-8`) # prep
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="sea urchin" & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03082900", us_hts_cn8$`CN-8`) # dried

# seabream
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="seabream","03028590", us_hts_cn8$`CN-8`) # fresh

# shad
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$Product.Name, "SHAD,STURGEON FRESH"), "03028990", us_hts_cn8$`CN-8`) # species unidentified, use eels
us_hts_cn8$`CN-8` <- if_else(str_detect(us_hts_cn8$Product.Name, "SHAD,STURGEON FROZEN"), "03038990", us_hts_cn8$`CN-8`) # species unidentified, use eels

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0001101595", "0001101599"), "03032600", us_hts_cn8$`CN-8`) # species unidentified, use eels

# shark
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark","03028180", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark"  & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038190", us_hts_cn8$`CN-8`) #  frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark" & str_detect(us_hts_cn8$Product.Name, "DOGFISH"),"03028115", us_hts_cn8$`CN-8`) # dogfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark" & str_detect(us_hts_cn8$Product.Name, "DOGFISH") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038115", us_hts_cn8$`CN-8`) # dogfish frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark"  & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03049690", us_hts_cn8$`CN-8`) #  meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark"  & str_detect(us_hts_cn8$Product.Name, "FIN"),"03029200", us_hts_cn8$`CN-8`) #  fins
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark"  & str_detect(us_hts_cn8$Product.Name, "FIN") & str_detect(us_hts_cn8$Product.Name, "DRIED"),"03057100", us_hts_cn8$`CN-8`) #  dried fin
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark"  & str_detect(us_hts_cn8$Product.Name, "FIN") & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16041800", us_hts_cn8$`CN-8`) #  prep fin
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shark"  & str_detect(us_hts_cn8$Product.Name, "FIN") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03039200", us_hts_cn8$`CN-8`) #  prep fin

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0304880000"), "03048819", us_hts_cn8$`CN-8`) # species unidentified, use other shark, look up codes

# shrimp
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp","03063690", us_hts_cn8$`CN-8`) # warm-water fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp"  & str_detect(us_hts_cn8$Product.Name, "COLD-WATER"),"03063590", us_hts_cn8$`CN-8`) # cold-water fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp"  & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03061799", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp"  & str_detect(us_hts_cn8$Product.Name, "FROZEN")& str_detect(us_hts_cn8$Product.Name, "COLD-WATER"),"03061699", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp"  & str_detect(us_hts_cn8$Product.Name, "FROZEN")& str_detect(us_hts_cn8$Product.Name, "WARM-WATER"),"03061799", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp"  & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03069590", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp"  & (str_detect(us_hts_cn8$Product.Name, "ATC")| str_detect(us_hts_cn8$Product.Name, "CANNED")),"16052900", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="shrimp"  & (str_detect(us_hts_cn8$Product.Name, "PREPAR") | str_detect(us_hts_cn8$Product.Name, "BREADED")),"16052190", us_hts_cn8$`CN-8`) 

# smelts
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="smelts","03028990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="smelts"  & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038990", us_hts_cn8$`CN-8`) #  frozen

# snail 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="snail","03079100", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="snail"  & str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16055900", us_hts_cn8$`CN-8`) #  prep

# snapper
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="snapper","03028990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="snapper"  & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038990", us_hts_cn8$`CN-8`) #  frozen

# squid
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="squid","03074220", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="squid"  & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03074338", us_hts_cn8$`CN-8`) #  frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="squid"  & (str_detect(us_hts_cn8$Product.Name, "PREPAR") | str_detect(us_hts_cn8$Product.Name, "CANNED")),"16055400", us_hts_cn8$`CN-8`) #  prep
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="squid"  & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03074940", us_hts_cn8$`CN-8`) #  dried
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="squid"  & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03074940", us_hts_cn8$`CN-8`) #  fillet, only import frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="squid"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "LOLIGO PEALEI"),"03074333", us_hts_cn8$`CN-8`) #  frozen

# swordfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="swordfish","03024700", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="swordfish"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03035700", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="swordfish" & (str_detect(us_hts_cn8$Product.Name, "FILLET") | str_detect(us_hts_cn8$Product.Name, "STEAKS")),"03044500", us_hts_cn8$`CN-8`) # fillet/steaks
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="swordfish"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") & (str_detect(us_hts_cn8$Product.Name, "FILLET") | str_detect(us_hts_cn8$Product.Name, "STEAKS")),"03048400", us_hts_cn8$`CN-8`) # fillet/steaks frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="swordfish" & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045400", us_hts_cn8$`CN-8`) # meat fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="swordfish"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "MEAT"),"03049100", us_hts_cn8$`CN-8`) #  frozen meat

# tilapia
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="tilapia","03027100", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="tilapia"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03032300", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="tilapia"  & str_detect(us_hts_cn8$Product.Name, "FILLET") ,"03043100", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="tilapia"  & str_detect(us_hts_cn8$Product.Name, "MEAT") ,"03045100", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="tilapia"  & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03046100", us_hts_cn8$`CN-8`) # fillet frozen

# toothfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="toothfish","03028300", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="toothfish"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03038300", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="toothfish"  & str_detect(us_hts_cn8$Product.Name, "FILLET") ,"03044600", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="toothfish"  & str_detect(us_hts_cn8$Product.Name, "MEAT") ,"03045500", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="toothfish"  & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048500", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="toothfish"  & str_detect(us_hts_cn8$Product.Name, "MEAT") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03049200", us_hts_cn8$`CN-8`) # meat frozen

# trout
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="trout","03021110", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="trout"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03031410", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="trout"  & str_detect(us_hts_cn8$Product.Name, "FILLET") ,"03044290", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="trout"  & str_detect(us_hts_cn8$Product.Name, "FILLET") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048250", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="trout"  & str_detect(us_hts_cn8$Product.Name, "SMOKED") ,"03054300", us_hts_cn8$`CN-8`) # smoked

# albacore tuna
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="albacore_tuna","03023190", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="albacore_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03034190", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="albacore_tuna"  & str_detect(us_hts_cn8$Product.Name, "ATC") ,"16041448", us_hts_cn8$`CN-8`) # atc
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="albacore_tuna"  & str_detect(us_hts_cn8$Product.Name, "LOINS") ,"16041931", us_hts_cn8$`CN-8`) # loins

# bigeye tuna
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bigeye_tuna","03023490", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bigeye_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03034490", us_hts_cn8$`CN-8`) # frozen

# bluefin tuna
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bluefin_tuna","03023519", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bluefin_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03034518", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bluefin_tuna"  & str_detect(us_hts_cn8$Product.Name, "SOUTHERN") ,"03023690", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bluefin_tuna"  & str_detect(us_hts_cn8$Product.Name, "PACIFIC") ,"03023599", us_hts_cn8$`CN-8`) # fresh

us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bluefin_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "PACIFIC"),"03034599", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="bluefin_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "SOUTHERN"),"03034690", us_hts_cn8$`CN-8`) # frozen

# other tuna
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_tuna","03023980", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03034985", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_tuna"  & str_detect(us_hts_cn8$Product.Name, "MEAT") ,"03049999", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") & str_detect(us_hts_cn8$Product.Name, "FILLET"),"03048700", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_tuna"  & (str_detect(us_hts_cn8$Product.Name, "ATC") | str_detect(us_hts_cn8$Product.Name, "A.T.C")),"16041441", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_tuna"  & str_detect(us_hts_cn8$Product.Name, "LOINS") ,"16041931", us_hts_cn8$`CN-8`) # loins
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other_tuna"  & str_detect(us_hts_cn8$Product.Name, "PREPAR") ,"16041448", us_hts_cn8$`CN-8`) # prep

# skipjack tuna
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="skipjack_tuna","03023390", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="skipjack_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03034390", us_hts_cn8$`CN-8`) # frozen

# yellowfin tuna
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="yellowfin_tuna","03023290", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="yellowfin_tuna"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03034290", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="yellowfin_tuna"  & str_detect(us_hts_cn8$Product.Name, "PREPAR") ,"16041438", us_hts_cn8$`CN-8`) # prep

# whitefish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="whitefish","03028990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="whitefish"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03038990", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="whitefish"  & str_detect(us_hts_cn8$Product.Name, "FILLET") ,"03044990", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="whitefish"  & str_detect(us_hts_cn8$Product.Name, "MEAT") ,"03045990", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="whitefish"  & str_detect(us_hts_cn8$Product.Name, "MEAT") & str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03049999", us_hts_cn8$`CN-8`) # meat frozen

# wolffish (red fish)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="wolffish","03044950", us_hts_cn8$`CN-8`) # fillet fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="wolffish"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03048929", us_hts_cn8$`CN-8`) # frozen fillet

# yellow perch
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="yellow perch","03043900", us_hts_cn8$`CN-8`) # fillet fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="yellow perch"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03046900", us_hts_cn8$`CN-8`) # frozen fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="yellow perch"  & str_detect(us_hts_cn8$Product.Name, "MEAT") ,"03045990", us_hts_cn8$`CN-8`) # meat

# other molluscs
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other molluscs","03079100", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other molluscs"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03079200", us_hts_cn8$`CN-8`) # frozen 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other molluscs"  & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE") ,"03079900", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other molluscs"  & str_detect(us_hts_cn8$Product.Name, "PREPAR") ,"16055900", us_hts_cn8$`CN-8`) # prep

# other crustaceans
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other crustaceans","03063990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other crustaceans"  & str_detect(us_hts_cn8$Product.Name, "FROZEN") ,"03061990", us_hts_cn8$`CN-8`) # frozen 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other crustaceans"  & str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE") ,"03069990", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other crustaceans"  & str_detect(us_hts_cn8$Product.Name, "PREPAR") ,"16054000", us_hts_cn8$`CN-8`) # prep

# other aquatic invertebrates
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other aquatic invertebrates","03089090", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other aquatic invertebrates"  & str_detect(us_hts_cn8$Product.Name, "PREPAR") ,"16056900", us_hts_cn8$`CN-8`) # prep

# other shellfish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other shellfish","03063990", us_hts_cn8$`CN-8`) # use other crustaceans
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other shellfish"& str_detect(us_hts_cn8$Product.Name, "ANALOG"),"16042005", us_hts_cn8$`CN-8`) # surimi
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other shellfish"& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03061990", us_hts_cn8$`CN-8`) 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="other shellfish"& str_detect(us_hts_cn8$Product.Name, "CANNED"),"03069990", us_hts_cn8$`CN-8`) 

# alewives (use herring)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="alewives","03056100", us_hts_cn8$`CN-8`) # salted

# cockles
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cockles","03077100", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cockles"& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03077900", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cockles"& str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03077900", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="cockles"& str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16055600", us_hts_cn8$`CN-8`) # prep

# fish nspf
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF"),"03028990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038990", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044990", us_hts_cn8$`CN-8`) # fresh fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "SMOKED"),"03054980", us_hts_cn8$`CN-8`) # smoked
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "DRIED"),"03055985", us_hts_cn8$`CN-8`) # dried
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "SALTED"),"03056980", us_hts_cn8$`CN-8`) # salted
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "DRIED/SALTED/BRINE"),"03053990", us_hts_cn8$`CN-8`) # DRIED/SALTED/BRINE, all fillets
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "SURIMI"),"03049910", us_hts_cn8$`CN-8`) # surimi
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FILLET")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048990", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& (str_detect(us_hts_cn8$Product.Name, "MEAT") | str_detect(us_hts_cn8$Product.Name, "MINCED"))& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03049999", us_hts_cn8$`CN-8`) # meat frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "PREPAR"),"16042090", us_hts_cn8$`CN-8`) # prep 
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FISH NSPF")& (str_detect(us_hts_cn8$Product.Name, "ATC") | str_detect(us_hts_cn8$Product.Name, "CANNED")),"16041997", us_hts_cn8$`CN-8`) # atc 

# marine fish nspf
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "MARINE FISH NSPF"),"03028990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "MARINE FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038990", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "MARINE FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044990", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "MARINE FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "MARINE FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FILLET")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048990", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "MARINE FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "MEAT")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03049999", us_hts_cn8$`CN-8`) # meat frozen

# freshwater fish nspf
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FRESHWATER FISH NSPF"),"03028910", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FRESHWATER FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038910", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FRESHWATER FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044910", us_hts_cn8$`CN-8`) # fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FRESHWATER FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045910", us_hts_cn8$`CN-8`) # meat
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FRESHWATER FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "FILLET")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048910", us_hts_cn8$`CN-8`) # fillet frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "FRESHWATER FISH NSPF")& str_detect(us_hts_cn8$Product.Name, "MEAT")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03049921", us_hts_cn8$`CN-8`) # meat frozen

# PIKE,PICKEREL,PIKE PERCH,YELLOW PIKE
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0302895025"), "03028910", us_hts_cn8$`CN-8`) # freshwater perch nspf
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "PIKE"),"03028990", us_hts_cn8$`CN-8`) # fresh
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "PIKE")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03038990", us_hts_cn8$`CN-8`) # frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "PIKE")& str_detect(us_hts_cn8$Product.Name, "FILLET"),"03044990", us_hts_cn8$`CN-8`) # fillet, use other fish
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "PIKE")& str_detect(us_hts_cn8$Product.Name, "FILLET")& str_detect(us_hts_cn8$Product.Name, "FROZEN"),"03048990", us_hts_cn8$`CN-8`) # frozen fillet
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$species_group=="species_unidentified"& str_detect(us_hts_cn8$Product.Name, "PIKE")& str_detect(us_hts_cn8$Product.Name, "MEAT"),"03045990", us_hts_cn8$`CN-8`) # meat

# remaining products, other molluscs and crustaceans (3 HTS codes: 0309905090, 1605900500, 0309903000)
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0309903000"), "03069990", us_hts_cn8$`CN-8`) # crustacean meal
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0309905090"), "03079200", us_hts_cn8$`CN-8`) # mollusc meal
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("1605900500"), "16055900", us_hts_cn8$`CN-8`) # mollusc prep

# cetacea, use other fish codes
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0208400000", "0208400100"), "03045990", us_hts_cn8$`CN-8`) # fresh/frozen
us_hts_cn8$`CN-8` <- if_else(us_hts_cn8$HTS.Number %in% c("0210920100"), "03054980", us_hts_cn8$`CN-8`) # dried

test <- us_hts_cn8 %>% filter(`CN-8`=="") # check, should be 0

#######################################################################################################
# 6) Merge assigned us hts codes to EUMOFA data, save to excel 
# Merge to EUMOFA data
merged_linkage <- merge(us_hts_cn8, EUMOFA_data, by="CN-8")

# Save linkage table for HTS to CN8
write_xlsx(merged_linkage, "Output Data/us_hts_cn8.xlsx")


