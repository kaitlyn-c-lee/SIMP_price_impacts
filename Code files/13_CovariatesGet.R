#####################################
### Extracts data from BLS, BEA, FRED ###
# 1) Load in registration key
# 2) BLS API Setup and Pull
# 3) FRED API Setup and Pull
# 4) Make variable for seafood prices
# 5) Combine all data, save to .R file for analysis
#####################################
# 1) Load in registration key
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
key='' # Request registration key from BLS

newDataYr <- 2023
cpi_only <- 1

# final set of covariates
# fuel oil, fish seafood cpi, disposable income, cold storage, exchange, containerized shipping, fed funds, soymeal, fishmeal, wheat

#####################################
# 2) BLS API Setup and Pull

# API calls 
bls_fun  <- function(X){
  result <- blsAPI(list('registrationkey'=key, 'startyear'=2005, 'endyear'=newDataYr,'seriesid'=X), 
                   2, return_data_frame=T)
  return(result)
}

bls_fun_early  <- function(X){
  result <- blsAPI(list('registrationkey'=key, 'startyear'=2000, 'endyear'=2004,'seriesid'=X), 
                   2, return_data_frame=T)
  return(result)
}

# Pull additional datasets from BLS that could influence fish retail price
addl_series <- c("APU000072511","CUUR0000SEFG")

data_addl_series1 <- bls_fun(addl_series) # API pull
data_addl_series2 <- bls_fun_early(addl_series) # API pull
data_addl_series <- rbind(data_addl_series1,data_addl_series2)

# Manually add names for these new series
data_addl_series$variable <- ""
data_addl_series$variable <- if_else(data_addl_series$seriesID=="APU000072511", "fuel_oil2", data_addl_series$variable) 
data_addl_series$variable <- if_else(data_addl_series$seriesID=="CUUR0000SEFG", "fishSeafood_cpi", data_addl_series$variable) 

data_addl_series <- as.data.frame(cbind(year=as.numeric(data_addl_series$year),Month=as.numeric(substring(data_addl_series$period,2,3)),as.numeric(data_addl_series$value),
                                        data_addl_series$variable))
data_addl_series$Month <- as.numeric(data_addl_series$Month)
data_addl_series      <- data_addl_series %>% 
  spread(V4,V3)
data_addl_series      <- data_addl_series[order(data_addl_series$year,data_addl_series$Month),]

bls_data_clean <- data_addl_series[order(data_addl_series$year,data_addl_series$Month),]

#####################################
# 3) FRED API Setup and Pull
fredr_set_key('b3ba93f0dbf23b4c3ee685f7ecdf59dc')

wheat_temp <- fredr(
  series_id = "PWHEAMTUSDM",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
wheat      <- cbind(year(wheat_temp$date),month(wheat_temp$date),wheat_temp$value)
colnames(wheat) <- c("year","Month","Wheat")

full_data <- merge(bls_data_clean, wheat, by=intersect(colnames(bls_data_clean), colnames(wheat)))

# disposable income
disp_income_temp <- fredr(
  series_id = "A229RX0",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
disp_income      <- cbind(year(disp_income_temp$date),month(disp_income_temp$date),disp_income_temp$value)
colnames(disp_income) <- c("year","Month","DispInc")

full_data <- merge(full_data, disp_income, by=intersect(colnames(full_data), colnames(disp_income)))

# fish meal
fishmeal_temp <- fredr(
  series_id = "PFISHUSDM",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
fishmeal     <- cbind(year(fishmeal_temp$date),month(fishmeal_temp$date),fishmeal_temp$value)
colnames(fishmeal) <- c("year","Month", "FishMeal")

full_data <- merge(full_data, fishmeal, by=intersect(colnames(full_data), colnames(fishmeal)))

# soybean meal
soymeal_temp <- fredr(
  series_id =  "PSMEAUSDM",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
soymeal      <- cbind(year(soymeal_temp$date),month(soymeal_temp$date),soymeal_temp$value)
colnames(soymeal) <- c("year","Month", "SoyMeal")

full_data <- merge(full_data, soymeal, by=intersect(colnames(full_data), colnames(soymeal)))

# containerized shipping PCU4883204883208
ship_temp <- fredr(
  series_id =  "PCU4883204883208",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
ship      <- cbind(year(ship_temp$date),month(ship_temp$date),ship_temp$value)
colnames(ship) <- c("year","Month", "ContainerShip")

full_data <- merge(full_data, ship, by=intersect(colnames(full_data), colnames(ship)))

# Exchange rates TWEXAFEGSMTH
exchange_temp <- fredr(
  series_id =  "TWEXAFEGSMTH",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
exchange      <- cbind(year(exchange_temp$date),month(exchange_temp$date),exchange_temp$value)
colnames(exchange) <- c("year","Month", "Exchange")

full_data <- merge(full_data, exchange, by=intersect(colnames(full_data), colnames(exchange)))

#federal funds rate FEDFUNDS
fedfunds_temp <- fredr(
  series_id =  "FEDFUNDS",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
fedfunds      <- cbind(year(fedfunds_temp$date),month(fedfunds_temp$date),fedfunds_temp$value)
colnames(fedfunds) <- c("year","Month", "FedFunds")

full_data <- merge(full_data, fedfunds, by=intersect(colnames(full_data), colnames(fedfunds)))


# cold storage rates PCU4931204931202
coldstor_temp <- fredr(
  series_id =  "PCU4931204931202",
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-07-01")
)
coldstor      <- cbind(year(coldstor_temp$date),month(coldstor_temp$date),coldstor_temp$value)
colnames(coldstor) <- c("year","Month", "ColdStorage")

full_data <- merge(full_data, coldstor, by=intersect(colnames(full_data), colnames(coldstor)))

#####################################
# 4) Make variable for seafood prices
# import price
trade_data_temp <- read_dta("Trade data/Output Data/03_HTS_trade_SIMP.dta")

# Calculate price
import_data_temp <- trade_data_temp %>% filter(trade_flow=="IMP" & year>=2000) %>% group_by(month, year) %>% summarise(import_ppt=sum(nominal_dollars) / sum(mtons_live_wgt)) %>% rename(Month=month)
import_data_temp$Month <- as.double(import_data_temp$Month)

temp_data <- merge(import_data_temp, full_data, by=c("Month", "year"))
temp_data <- temp_data %>% filter(year>=2000 & year<2023)

reg <- lm(fishSeafood_cpi ~ import_ppt, data=temp_data)
summary(reg)

full_data <- full_data %>% filter(year>=2000 & year<2023)

reg2 <- lm(fishSeafood_cpi ~ reg$residuals, data=temp_data)
summary(reg2)

full_data$exog_fishCPI <- (reg2$fitted.values)
full_data <- full_data %>% select(!fishSeafood_cpi)

#####################################
# 6) Combine all data, save to .R file for analysis
save(full_data, file = "Data/covariates.RData")


