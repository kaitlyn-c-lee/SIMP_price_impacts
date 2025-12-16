#######################################################################################################  
###################################  Summary Stats ###################################
# 1) Output numbers for Table 1
# 2) Table of classifications for SIMP species + product forms
# 3) 2016 Species map
# 4) 2016 simp coverage by species group
# 5) Apparent consumption by species over time
# 6) Perc of Apparent consumption attributable to aquaculture 2016
# 7) Top production country by species group
#######################################################################################################
#######################################################################################################
# 1) Output numbers for Table 1
simp_byspecies <- trade_data %>% filter(species_group %in% simp_groups & (year==2016 | year==2017) & trade_flow=="IMP") %>% group_by(species_group, year) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
simp_byspecies$real_dollars <- simp_byspecies$real_dollars / 1000000000

simp_all <- trade_data %>% filter(species_group %in% simp_groups & (year==2016 | year==2017) & trade_flow=="IMP") %>% group_by(year) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
simp_all$real_dollars <- simp_all$real_dollars / 1000000000

simp_excl <- trade_data %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & (year==2016 | year==2017) & trade_flow=="IMP") %>% group_by(year) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
simp_excl$real_dollars <- simp_excl$real_dollars / 1000000000

all_us <- trade_data %>% filter( (year==2016 | year==2017) & trade_flow=="IMP") %>% group_by(year) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))
all_us$real_dollars <- all_us$real_dollars / 1000000000

# assign to FPI groups

imports$fpi_species <- ""

# Tunas
imports$fpi_species <- if_else(str_detect(imports$species_group, "Tuna"), "Tunas", imports$fpi_species)

# Whitefish
imports$fpi_species <- if_else(imports$species_group %in% c("Groundfish: Cod"), "White fish", imports$fpi_species)

# Crustaceans
imports$fpi_species <- if_else(str_detect(imports$species_group, "Crab"), "Crustaceans", imports$fpi_species)

# Other fish
imports$fpi_species <- if_else(imports$species_group %in% c("Dolphinfish", "Snapper", "Swordfish", "Grouper", "Shark"), "Other fish", imports$fpi_species)

#######################################################################################################
# 2) Table of classifications for SIMP species + product forms
imports_form <- imports %>% filter(species_group %in% simp_groups & (month_yr>="2012-01-01" & month_yr<="2018-01-01")  & species_group!="Shrimp" & species_group!="Abalone" & !is.na(process_group))  %>% group_by(species_group, hts_linkto_2016, month_yr, hts_code, product_name,process_group, fpi_species) %>% summarise(mtons_raw=sum(mtons_raw), real_dollars=sum(real_dollars))

imports_form$pre_2017 <- if_else(imports_form$month_yr<="2016-01-01", 1, 0)
imports_form$post_2017 <- if_else(imports_form$month_yr>="2017-01-01", 1, 0)
simp_hts_treatment <- imports_form %>% group_by(species_group, hts_code, product_name, hts_linkto_2016,process_group, fpi_species) %>% summarise(mtons_2016=sum(mtons_raw*pre_2017), mtons_2017=sum(mtons_raw*post_2017))
simp_hts_treatment <- simp_hts_treatment %>% rename(`Species Group`=species_group, `HTS Code`=hts_code, `Product Name`=product_name, `2016 HTS Code`=hts_linkto_2016, `2012-2016 Imports`=mtons_2016, `2017 Imports`=mtons_2017, Processing=process_group, `FPI Group`=fpi_species)
  
print(xtable(simp_hts_treatment, type = "latex", caption = 'Grouping of individual HTS codes into NMFS species groups and 2016 HTS codes. Quantity imported in 2016 and 2017 is reported in metric tons.', label='table:form_groups'), tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top', file = paste(output_location, 'Tables/form_groups.tex', sep=''))

#######################################################################################################
# 3) 2016 Species map
top_country <- trade_data %>% filter(trade_flow=="IMP" & species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & year==2016) %>% group_by(species_group, prod_country) %>% summarise(mtons_raw=sum(mtons_raw)) %>% top_n(1,mtons_raw)
top_country <- top_country %>% arrange(desc(mtons_raw))
top_country$species_group <- factor(top_country$species_group, levels = (unique(top_country$species_group)))

# load in shapefile
# Download from https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/
test_shp <- st_read("NaturalEarth/ne_10m_admin_0_countries.shp")
test_shp$NAME_EN <- tolower(test_shp$NAME_EN)

# replace names to match trade data 
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="people's republic of china", "china", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="taiwan", "taiwan province of china", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="vietnam", "viet nam", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="the bahamas", "bahamas", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="south korea", "korea, republic of", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="netherlands", "netherlands (kingdom of the)", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="saint vincent and the grenadines", "saint vincent/grenadines", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="saint helena", "st.helena", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="tanzania", "tanzania, united rep. of", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="turks and caicos islands", "turks and caicos is.", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="venezuela", "venezuela (boliv rep of)", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="hong kong", "china, hong kong sar", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="russia", "russian federation", test_shp$NAME_EN)
test_shp$NAME_EN <- if_else(test_shp$NAME_EN =="the gambia", "gambia", test_shp$NAME_EN)
test_shp <- test_shp %>% rename(prod_country=NAME_EN)

map_data <- merge(test_shp, top_country, by="prod_country",all.x=T )
map_data$species_country <- if_else(!is.na(map_data$species_group),paste(str_to_title(map_data$prod_country), map_data$species_group, sep=" - "), NA)

# plot 
ggplot(map_data) +
  geom_sf(aes(fill = (mtons_raw))) +
  ggrepel::geom_label_repel(
    data = (map_data),
    aes(label = species_country, geometry = geometry),
    stat = "sf_coordinates",
    force = 700, 
    direction="both",
    max.overlaps=1000, size = 7
  )+
  labs(x=" ", y=" ", fill="Tons Imported")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),legend.text=element_text(size=20), legend.title=element_text(size=22))+ 
  scale_fill_gradientn(colors = (colorRampPalette(RColorBrewer::brewer.pal(9,'Blues'))(16)),na.value = "lightgrey", labels = scales::label_comma())
ggsave(filename=paste(output_location,'Figures/simp_map.png' ,sep=''),width = 15, height = 7, dpi = 200)

#######################################################################################################
# 4) 2016 simp coverage by species group
imports_coverage <- trade_data %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone"& year==2016 & trade_flow=="IMP") %>% group_by(species_group) %>% summarise(perc_SIMP=sum(final_SIMP*mtons_live_wgt) / sum(mtons_live_wgt)) 

hts_coverage <- ggplot(imports_coverage, aes(x=reorder(species_group, perc_SIMP), y=perc_SIMP)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  theme(plot.title = element_text(size=18,hjust = 0.5),
        axis.text.y=element_text(size=14),axis.text.x=element_text(size=14),
        axis.title=element_text(size=18), legend.text=element_text(size=18), legend.title=element_text(size=18), legend.position="bottom", strip.text.x = element_text(size = 14)) +
  coord_flip()+
  xlab("Species Group") + ylab("% of Quantity Imported (Metric Tons Live Weight)") + 
  ggtitle("SIMP Final Rule HTS Codes")

imp_3alpha <- read_dta("~/Dropbox (ASU)/US_trade_databases/Output Data/02a_NOAA_FAO_linked_3A_bycountry.dta")
# Rename species groups
imp_3alpha$species_group <- str_replace_all(imp_3alpha$species_group, "_", " ")
imp_3alpha$species_group <- str_to_title(imp_3alpha$species_group)

imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Albacore Tuna", "Tuna: Albacore", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Bluefin Tuna", "Tuna: Bluefin", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Cod Groundfish", "Groundfish: Cod", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Skipjack Tuna", "Tuna: Skipjack", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Yellowfin Tuna", "Tuna: Yellowfin", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Bigeye Tuna", "Tuna: Bigeye", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Other Tuna", "Tuna: Other", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Other Crab", "Crab: Other", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="King Crab", "Crab: King", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Blue Crab", "Crab: Blue", imp_3alpha$species_group)
imp_3alpha$species_group <- if_else(imp_3alpha$species_group=="Dolphin", "Dolphinfish", imp_3alpha$species_group)

imp_3alpha <- imp_3alpha %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone"& year==2016 ) %>% group_by(species_group) %>% summarise(perc_SIMP=sum(SIMP_3A*implied_tons_traded) / sum(implied_tons_traded)) 

alph_coverage <- ggplot(imp_3alpha, aes(x=reorder(species_group, perc_SIMP), y=perc_SIMP)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  theme(plot.title = element_text(size=18,hjust = 0.5),
        axis.text.y=element_text(size=14),axis.text.x=element_text(size=14),
        axis.title=element_text(size=18), legend.text=element_text(size=18), legend.title=element_text(size=18), legend.position="bottom", strip.text.x = element_text(size = 14)) +
  coord_flip()+
  xlab(" ") + ylab("% of Quantity Imported (Metric Tons Live Weight)") + 
  ggtitle("SIMP 3-alpha Codes")

graph <- ggarrange(hts_coverage, alph_coverage, nrow=1)
ggsave(graph, filename=paste(output_location,'Figures/simp_coverage2016.png' ,sep=''),width = 15, height = 7, dpi = 200)

#######################################################################################################
# 5) Apparent consumption by species over time
apparent_consumption <- read_dta("Data/04_apparent_consumption.dta")
apparent_consumption$species_group <- str_replace_all(apparent_consumption$species_group, "_", " ")
apparent_consumption$species_group <- str_to_title(apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Albacore Tuna", "Tuna: Albacore", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Bluefin Tuna", "Tuna: Bluefin", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Cod Groundfish", "Groundfish: Cod", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Skipjack Tuna", "Tuna: Skipjack", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Yellowfin Tuna", "Tuna: Yellowfin", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Bigeye Tuna", "Tuna: Bigeye", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Other Tuna", "Tuna: Other", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Other Crab", "Crab: Other", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="King Crab", "Crab: King", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Blue Crab", "Crab: Blue", apparent_consumption$species_group)
apparent_consumption$species_group <- if_else(apparent_consumption$species_group=="Dolphin", "Dolphinfish", apparent_consumption$species_group)


apparent_consumption <- apparent_consumption %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & year>=2012 & year<=2017)
apparent_consumption <- apparent_consumption %>% group_by(species_group) %>% mutate(species_sum=sum(consumption)) %>% arrange(desc(species_sum))
apparent_consumption$species_group <- factor(apparent_consumption$species_group, levels = (unique(apparent_consumption$species_group)))


tot<- ggplot(apparent_consumption, aes(x=as.numeric(year), y=consumption, group=1)) + 
  geom_bar(stat="identity") +
  geom_vline(xintercept = 2016.5, linetype=2) +
  facet_wrap(~species_group, scales='free_y')+
  theme_bw()+
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x="Year", y="Apparent Consumption (metric tons)")+
  theme(plot.title = element_text(size=18,hjust = 0.5),
        axis.text.y=element_text(size=14),axis.text.x=element_text(size=14),
        axis.title=element_text(size=18), legend.text=element_text(size=18), legend.title=element_text(size=18), legend.position="bottom", strip.text.x = element_text(size = 14)) +
  ggtitle("Total U.S. Apparent Consumption by SIMP Species")
ggsave(tot, filename=paste(output_location,'Figures/apparent_consumption_byyr_species.png' ,sep=''),width = 15, height = 7, dpi = 200)

#######################################################################################################
# 6) Perc of Apparent consumption attributable to aquaculture 2016
apparent_consumption <- apparent_consumption %>% filter(year==2016)
apparent_consumption$aqua_consumption <- (apparent_consumption$prod_tons * apparent_consumption$perc_aqua_prod) + (apparent_consumption$imp_tons * apparent_consumption$perc_aqua_imp) - (apparent_consumption$exp_tons * apparent_consumption$perc_aqua_exp) - (apparent_consumption$rex_tons * apparent_consumption$perc_aqua_rex)

apparent_consumption$perc_aqua_consumption <- apparent_consumption$aqua_consumption / apparent_consumption$consumption
apparent_consumption$perc_aqua_consumption <- if_else(apparent_consumption$perc_aqua_consumption>=1, 1, apparent_consumption$perc_aqua_consumption)


aqua <- ggplot(apparent_consumption, aes(x=species_group, y=perc_aqua_consumption)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  theme(plot.title = element_text(size=24,hjust = 0.5),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18)) +
  coord_flip()+
  scale_x_discrete(limits=rev)+
  xlab("Species Group") + ylab("Percent of Consumption") + 
  labs(fill = "Species Group") + 
  ggtitle("Share of Apparent Consumption Estimated from Aquaculture in 2016")
ggsave(aqua, filename=paste(output_location,'Figures/perc_aqua_byspecies.png' ,sep=''),width = 15, height = 7, dpi = 200)

#######################################################################################################
# 7) Top production country by species group
prod_data <- read_dta("Data/02a_NOAA_FAO_linked_3A_bycountry.dta")

prod_data$species_group <- str_replace_all(prod_data$species_group, "_", " ")
prod_data$species_group <- str_to_title(prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Albacore Tuna", "Tuna: Albacore", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Bluefin Tuna", "Tuna: Bluefin", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Cod Groundfish", "Groundfish: Cod", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Skipjack Tuna", "Tuna: Skipjack", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Yellowfin Tuna", "Tuna: Yellowfin", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Bigeye Tuna", "Tuna: Bigeye", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Other Tuna", "Tuna: Other", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Other Crab", "Crab: Other", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="King Crab", "Crab: King", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Blue Crab", "Crab: Blue", prod_data$species_group)
prod_data$species_group <- if_else(prod_data$species_group=="Dolphin", "Dolphinfish", prod_data$species_group)


prod_data <- prod_data %>% filter(species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & year>=2012 & year<=2017)


top_country <- prod_data %>% filter( species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone") %>% group_by(species_group, country, year) %>% summarise(mtons_live_wgt=sum(mtons_live_wgt)) 

top_country <- prod_data %>% filter(year==2016) %>% group_by(subset, country, year) %>% summarise(mtons_live_wgt=sum(mtons_live_wgt)) %>% group_by(subset, year) %>% summarise(n=length(unique(country)))

# HHI of prod by FPI group
prod_data$subset <- "Other fish" 
prod_data$subset <- if_else(str_detect(prod_data$species_group, "Tuna"), "Tunas", prod_data$subset)
prod_data$subset <- if_else(str_detect(prod_data$species_group, "Groundfish: Cod"), "White fish", prod_data$subset)
prod_data$subset <- if_else(str_detect(prod_data$species_group, "Crab"), "Crustaceans", prod_data$subset)

concentrations <- prod_data %>% filter(year==2016) %>% group_by(country, subset) %>% summarise(mtons_live_wgt=sum(mtons_live_wgt))

concentrations <- concentrations %>% group_by(subset) %>% mutate(total_q = sum(mtons_live_wgt))
concentrations$share <- (concentrations$mtons_live_wgt / concentrations$total_q)
concentrations <- concentrations %>% group_by(subset) %>% summarise(hhi=sum(share^2)) %>% arrange(desc(hhi))
concentrations$subset <- factor(concentrations$subset, levels = (unique(concentrations$subset)))

ggplot(concentrations, aes(x=subset, y=hhi, group=1)) +
  geom_bar(stat="identity")+
  labs(x="FAO Fish Price Index Category", y="Herfindahl–Hirschman Index (HHI)") +
  theme_bw() +
  theme(plot.title = element_text(size=24,hjust = 0.5),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18)) +
  coord_flip()+
  scale_x_discrete(limits=rev)+
  ggtitle("Concentration in Production Countries by FPI Category in 2016")
ggsave( filename=paste(output_location,'Figures/hhi_byfpi.png' ,sep=''),width = 15, height = 7, dpi = 200)

# top prod country by species group
top_country <- prod_data %>% filter( species_group %in% simp_groups & species_group!="Shrimp" & species_group!="Abalone" & year==2016) %>% group_by(species_group, country) %>% summarise(mtons_live_wgt=sum(mtons_live_wgt)) %>% top_n(1,mtons_live_wgt)
top_country <- top_country %>% arrange(desc(mtons_live_wgt))
top_country$species_group <- factor(top_country$species_group, levels = (unique(top_country$species_group)))
top_country$country <- str_to_title(top_country$country)
top_country <- top_country %>% rename(`NMFS Species Group`=species_group, `Production Country`=country, `Total Production Tons`=mtons_live_wgt) %>% mutate(`Total Production Tons`=round(`Total Production Tons`,2))

print(xtable(top_country,  type = "latex", caption = 'Top production country for each NMFS species group in 2016. Total production is sourced from the FAO FishstatJ database and is measured in live weight metric tons.', label='table:top_country_prod2016'), 
      tabular.environment="longtable", floating=FALSE, include.rownames = FALSE, caption.placement = 'top',
      file = paste(output_location, 'Tables/top_country_prod2016.tex', sep=''))


