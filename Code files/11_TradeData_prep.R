###################################  Prep data for Analysis ###################################
# 1) Load in HTS code database, rename species groups
# 2) Add variable for product form
# 3) Fill missing months with 0 in import data
# 4) Merge in ITC processing data
#######################################################################################################
#######################################################################################################
# 1) Load in HTS code database, rename species groups
trade_data <- read_dta("Trade data/Output data/03_HTS_trade_SIMP.dta")
trade_data <- trade_data %>% rename(prod_country=country)

trade_data$species_group <- if_else(trade_data$hts_code=="0305494020", "cod_groundfish", trade_data$species_group)

# Rename species groups
trade_data$species_group <- str_replace_all(trade_data$species_group, "_", " ")
trade_data$species_group <- str_to_title(trade_data$species_group)

trade_data$species_group <- if_else(trade_data$species_group=="Albacore Tuna", "Tuna: Albacore", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Atlantic Salmon", "Salmon: Atlantic", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Blue Whiting Groundfish", "Groundfish: Blue Whiting", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Bluefin Tuna", "Tuna: Bluefin", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Chinook Salmon", "Salmon: Chinook", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Chum Salmon", "Salmon: Chum", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Cod Groundfish", "Groundfish: Cod", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Coho Salmon", "Salmon: Coho", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Cusk Groundfish", "Groundfish: Cusk", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Danube Salmon", "Salmon: Danube", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Haddock Groundfish", "Groundfish: Haddock", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Hake Groundfish", "Groundfish: Hake", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Halibut Flatfish", "Flatfish: Halibut", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Ocean Perch Groundfish", "Groundfish: Ocean Perch", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Other Flatfish", "Flatfish: Other", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Other Groundfish", "Groundfish: Other", trade_data$species_group)
#trade_data$species_group <- if_else(trade_data$species_group=="Other Molluscs", "Molluscs: Other", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Other Salmon", "Salmon: Other", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Other Shellfish", "Shellfish: Other", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Pink Salmon", "Salmon: Pink", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Plaice Flatfish", "Flatfish: Plaice", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Pollock Groundfish", "Groundfish: Pollock", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Skipjack Tuna", "Tuna: Skipjack", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Sockeye Salmon", "Salmon: Sockeye", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Coho Salmon", "Salmon: Coho", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Sole Flatfish", "Flatfish: Sole", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Turbot Flatfish", "Flatfish: Turbot", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Whiting Groundfish", "Groundfish: Whiting", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Yellowfin Tuna", "Tuna: Yellowfin", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Bigeye Tuna", "Tuna: Bigeye", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Other Tuna", "Tuna: Other", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Other Crab", "Crab: Other", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Flounder Flatfish", "Flatfish: Flounder", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="King Crab", "Crab: King", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Swimming Crab", "Crab: Swimming", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Blue Crab", "Crab: Blue", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Snow Crab", "Crab: Snow", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Dungeness Crab", "Crab: Dungeness", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Species Unidentified", "Unidentified Species", trade_data$species_group)
trade_data$species_group <- if_else(trade_data$species_group=="Dolphin", "Dolphinfish", trade_data$species_group)

#######################################################################################################
# 2) Define SIMP species groups and aggegate import data
#pull US import SIMP species
simp_groups <- c("Abalone", "Crab: Blue", "Crab: King", "Shark", "Dolphinfish", "Groundfish: Cod", "Grouper", "Sea Cucumber", 
                 "Shrimp", "Snapper", "Swordfish", "Crab: Other", "Tuna: Other", "Tuna: Albacore", 
                 "Tuna: Bluefin", "Tuna: Bigeye", "Tuna: Skipjack", "Tuna: Yellowfin") 
#, "Shrimp -15", "Shrimp 15-20", "Shrimp 21-25", "Shrimp 26-30", "Shrimp 31-40", "Shrimp 41-50", "Shrimp 51-60", "Shrimp 61-70", "Shrimp 70-")

simp_adj_groups <- c("Other Molluscs",  "Other Aquatic Invertebrates", "Groundfish: Other",
                     "Other Crustaceans", "Whitefish")

# aggregate to HTS-country level
imports <- trade_data %>% filter(trade_flow=="IMP"& year>=2005 & year<2020) %>% group_by(month, year, species_group, hts_code, product_name,prod_country) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars), mtons_live_wgt=sum(mtons_live_wgt)) 

#######################################################################################################
# 3) Fill missing months with 0 in import data
imports$month_yr <- (paste0(imports$year,"-", imports$month, "-01"))
imports$month_yr <- as.Date(imports$month_yr)

month_df <- tibble(
  month = rep(c(1:12), 15),
  year = c(rep(2005, 12), rep(2006, 12), rep(2007, 12), rep(2008, 12), rep(2009, 12),rep(2010, 12),rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12), rep(2015, 12), rep(2016, 12), rep(2017, 12),rep(2018, 12),rep(2019, 12))
) %>% 
  mutate(month_ind = ifelse(nchar(month) == 1, paste0("0", month), month),
         month_yr = as.Date(paste0(year, "-", month_ind, "-01"))) %>% 
  select(month_yr)

imports <- imports %>% ungroup() %>% 
  group_split(species_group, hts_code, product_name, prod_country) %>% 
  map(., right_join, month_df) %>% 
  map(., fill, c(species_group, hts_code, product_name, prod_country)) %>% 
  bind_rows() %>% 
  arrange(species_group, hts_code, product_name, prod_country, month_yr) %>%
  select(!c(month, year))

# replace NAs w/ 0
imports[is.na(imports)] <- 0 # if nothing reported, it's 0

#######################################################################################################
# 4) Merge in ITC processing data
# Load in and clean ITC excel file
iuu_import_estimates <- read_excel("Trade data/Source Data/iuu_import_estimates.xlsx", sheet = "iuu_trade_all", skip = 1)

# Keep only 2018
iuu_import_estimates <- iuu_import_estimates %>% filter(year==2018) 
iuu_import_estimates$hs_code_10 <- ifelse(str_sub(iuu_import_estimates$hs_code_10,1,1)=="3", paste0('0', iuu_import_estimates$hs_code_10), iuu_import_estimates$hs_code_10)

# Convert to 2016 HTS codes and add our species groups
HTS_2016_2018_link <- read_excel("Trade data/Output Data/HTS_2016_2018_link.xlsx")
HTS_2016_2018_link <- HTS_2016_2018_link %>% filter(species_group!="shrimp" & species_group!="abalone") %>% select(species_group,hts_2018,hts_linkto_2016)
HTS_2016_2018_link$hts_2018 <- if_else(is.na(HTS_2016_2018_link$hts_2018), HTS_2016_2018_link$hts_linkto_2016, HTS_2016_2018_link$hts_2018)

iuu_import_estimates <- merge(iuu_import_estimates, HTS_2016_2018_link, by.x="hs_code_10", by.y="hts_2018")

# Create hts level variable for product form
iuu_import_estimates_byhts <- iuu_import_estimates %>% filter(species_group.y!="shrimp" & species_group.y!="abalone") %>% group_by(hts_linkto_2016) %>% summarise(perc_processed = sum(processed*quantity) / sum(quantity))

# Put import data into time-consistent hts codes
trade_data_itc <- merge(imports, HTS_2016_2018_link,by.x="hts_code", by.y="hts_2018", all.x=T)
trade_data_itc$hts_linkto_2016 <- if_else(trade_data_itc$hts_linkto_2016=="0304991090", "0304991190", trade_data_itc$hts_linkto_2016) # other tuna
trade_data_itc$hts_linkto_2016 <- if_else(trade_data_itc$hts_linkto_2016=="0305620050", "0305620060", trade_data_itc$hts_linkto_2016) # cod
trade_data_itc$hts_linkto_2016 <- if_else(trade_data_itc$hts_linkto_2016=="0303810090", "0303810010", trade_data_itc$hts_linkto_2016) # frozen shark
trade_data_itc$hts_linkto_2016 <- if_else(trade_data_itc$hts_code=="0302810011" | trade_data_itc$hts_linkto_2016=="0302810011", "0302810090", trade_data_itc$hts_linkto_2016) # fresh shark
trade_data_itc$hts_linkto_2016 <- if_else(trade_data_itc$hts_linkto_2016=="0302810010", "0302810090", trade_data_itc$hts_linkto_2016) # fresh shark fillet

trade_data_itc <- merge(trade_data_itc, iuu_import_estimates_byhts,by="hts_linkto_2016", all.x=T)

imports <- trade_data_itc %>% select(!species_group.y) %>% rename(species_group=species_group.x)

imports$process_group <- if_else(imports$perc_processed>0, "Processed", "Unprocessed")


# Save off indicator for codes where processing status changes over time

temp <- imports %>% filter(species_group%in% simp_groups & species_group!="shrimp" & species_group!="abalone" & !is.na(mtons_raw))
temp$ind_processing <- if_else(temp$perc_processed!=1 & temp$perc_processed!=0, 1, 0)
temp <- temp %>% filter(species_group%in% simp_groups & species_group!="shrimp" & species_group!="abalone" & !is.na(ind_processing))
temp$year <- year(temp$month_yr)

test <- temp %>% filter(ind_processing==1) %>% group_by(product_name, hts_code,hts_linkto_2016) %>% summarise() %>% rename(`Product Name`=product_name, `HTS Code`=hts_code, `2016 HTS Code`=hts_linkto_2016)

print(xtable(test, type = "latex", caption = 'HTS codes where processing status changes over time with HTS code revisions', label='table:process_robust_hts'), tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top', file = paste(output_location, 'Tables/process_robust_hts.tex', sep=''))


temp <- temp %>% group_by(year) %>% summarise(mtons_perc_proc = sum(ind_processing*mtons_raw)/sum(mtons_raw))

imports$ind_process_robust <- if_else(imports$hts_code %in% test$`HTS Code`, 1, 0)


