#######################################################################################################  
################################# Create linkage_HTS_2018_2016.xlsx ################################## 

#######################################################################################################
# 1) Load in trade data, keep only SIMP species imports, aggregate to HTS code level
trade_data <- read_dta("Output Data/03_HTS_trade_SIMP.dta")

# keep only imports
imp_data <- trade_data %>% filter(trade_flow=="IMP")

# keep only 2016 and 2017
imp_data <- imp_data %>% filter(year>=2012 & year<=2019)

imp_data$pre_2017 <- if_else(imp_data$year<2017, 1, 0)
imp_data$post_2017 <- if_else(imp_data$year>=2017, 1, 0)

imp_data <- imp_data %>% group_by(species_group, product_name, hts_code, final_SIMP) %>% summarise(mtons_pre2017=sum(mtons_raw*pre_2017), mtons_post2017=sum(mtons_raw*post_2017))

#######################################################################################################
# 2) Split codes before and after HTS change
imp_data$hts_linkto_2016 <- if_else(imp_data$mtons_pre2017>0, imp_data$hts_code, "") # codes that existed in 2016

imp_data$hts_2018 <- if_else(imp_data$mtons_post2017>0, imp_data$hts_code, "") # 2018 HTS codes

# codes that come online after 2017 revision
# Abalone
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code=="0307870000" | imp_data$hts_code=="0307830000"), "0307890000", imp_data$hts_linkto_2016)

# Albacore tuna -- not a real change, just not used before 2017
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code=="1604142251"), "1604142251", imp_data$hts_linkto_2016)

# King crab
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0306144006", "0306144009", "0306144015", "0306144003", "0306144012")), "0306144010", imp_data$hts_linkto_2016)

# Other crab
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0306334000", "0306934000")), "0306244000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0306332000", "0306932000")), "0306242000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code=="1605100510"), "1605100510", imp_data$hts_linkto_2016) # not a real change, just not used before 2017

# Other Tuna -- not a real change, just not used before 2017
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code=="0304991190"), "0304991190", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code=="1604142299"), "1604142299", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code=="1604142291"), "1604142291", imp_data$hts_linkto_2016)

# Sea cucumber
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0308190100", "0308120000")), "0308190000", imp_data$hts_linkto_2016)

# Shark
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0303810011"), "0303810010", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0302810011"), "0302810010", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0303810091"), "0303810090", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0302810091"), "0302810090", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0302920000", "0303920000", "1604189000")), "0305710000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304960000"), "0303810010", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304470000"), "0302810011", imp_data$hts_linkto_2016)

# Shrimp
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0306350040"), "0306260040", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0306350020"), "0306260020", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0306360040"), "0306270040", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0306360020"), "0306270020", imp_data$hts_linkto_2016)

# have two codes, should aggregate
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0306950040"), "0306270040, 0306260040", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0306950020"), "0306260020, 0306270020", imp_data$hts_linkto_2016)

# One cod code gets discarded before 2016, shouldn't matter for Andrew's paper but need to group for Kaitlyn's
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="0305620030", "0305620025", imp_data$hts_linkto_2016)

imp_data$hts_2018 <- if_else(imp_data$hts_code=="0305620030", "0305620030", imp_data$hts_2018)

# Non-SIMP species
# carp
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0303250100"), "0303250000", imp_data$hts_linkto_2016)

# clam
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307790130", "0307720030")), "0307790030", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307790155", "0307720055")), "0307790055", imp_data$hts_linkto_2016)

# cockles
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307790160", "0307720060")), "0307790060", imp_data$hts_linkto_2016)

# conch
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$species_group=="conch"), "0307910130", imp_data$hts_linkto_2016)

# cusk
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code=="0304490125"), "0302895040", imp_data$hts_linkto_2016)

# cuttlefish
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307490160", "0307430060")), "0307490060", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0307420060"), "0307410060", imp_data$hts_linkto_2016)

# herring
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305396110"), "0305396010", imp_data$hts_linkto_2016)

# lobster
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0306320010"), "0306220010", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0306920000", "0306320090")), "0306220090", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0306940000", "0306340000")), "0306250000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0306310000", "0306910000")), "0306210000", imp_data$hts_linkto_2016)

# mussels
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307390100", "0307320000")), "0307390000", imp_data$hts_linkto_2016)

# nile perch
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304510120"), "0304510020", imp_data$hts_linkto_2016)

# ocean perch
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304490120"), "0304490020", imp_data$hts_linkto_2016)

# octopus
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307590100", "0307520000")), "0307590000", imp_data$hts_linkto_2016)

# crustaceans
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0306990000", "0306390000")), "0306290100", imp_data$hts_linkto_2016)

# molluscs
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307920090", "0307990200")), "0307990100", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0307910290"), "0307910190", imp_data$hts_linkto_2016)

# flatfish
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304590061"), "0304590060", imp_data$hts_linkto_2016)

# groundfish
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305530000"), "0305320090", imp_data$hts_linkto_2016)

# oysters
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307190160", "0307120060")), "0307190060", imp_data$hts_linkto_2016)# farmed
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307190180", "0307120080")), "0307190080", imp_data$hts_linkto_2016)# wild

# pickerel
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304490106"), "0304490006", imp_data$hts_linkto_2016)

# pike
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304490103"), "0304490003", imp_data$hts_linkto_2016)

# rays
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0304480000", "0304570000")), "0302820000", imp_data$hts_linkto_2016)# wild
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304970000"), "0303820000", imp_data$hts_linkto_2016)

# scallops
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307290100", "0307220000")), "0307290000", imp_data$hts_linkto_2016)# wild

# whitefish
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304490109"), "0304490009", imp_data$hts_linkto_2016)

# tilapia
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304490112"), "0304490012", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304510125"), "0304510025", imp_data$hts_linkto_2016)

# squid
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0307420040"), "0307410040", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307430050", "0307490150")), "0307490050", imp_data$hts_linkto_2016)# wild
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307430029", "0307490129")), "0307490029", imp_data$hts_linkto_2016)# wild
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307490122", "0307430022")), "0307490022", imp_data$hts_linkto_2016)# wild
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0307430024", "0307490124")), "0307490024", imp_data$hts_linkto_2016)# wild
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0307420020"), "0307410020", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0307430010"), "0307490010", imp_data$hts_linkto_2016)

# unidentified
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304999190"), "0304999191", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304490190"), "0304490090", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304895091"), "0304895090", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0302490000", "0302895077")), "0302895076", imp_data$hts_linkto_2016)# wild
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0303590000", "0303890080")), "0303890079", imp_data$hts_linkto_2016)# wild
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304590091"), "0304590090", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305590001"), "0305590000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305396180"), "0305396080", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304590036"), "0304590035", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304490115"), "0304490015", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305494045"), "0305494041", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="1604192200"), "1604192100", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="1604193200"), "1604193100", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="1604192200"), "1604192100", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304510190"), "0304510090", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305310100"), "0305310000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305440100"), "0305440000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305520000"), "0305310000", imp_data$hts_linkto_2016)
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304999191"), "0304999190", imp_data$hts_linkto_2016)

# sea urchin
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code %in% c("0308220000", "0308290100")), "0308290000", imp_data$hts_linkto_2016)# wild

# shark -- unsure
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0304880000"), "0303810010", imp_data$hts_linkto_2016)

# fish nspf -- unsure
imp_data$hts_linkto_2016 <- if_else(imp_data$hts_linkto_2016=="" & (imp_data$hts_code =="0305540000"), "0305590000", imp_data$hts_linkto_2016)

#######################################################################################################
# 4) Save to excel file
write_xlsx(imp_data, "Output Data/HTS_2016_2018_link.xlsx")

