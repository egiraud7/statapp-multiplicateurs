 library(dplyr)
 library(readxl)
library(magrittr)
library(tidyr)
 library(zoo)
 
 ### 1) handling monthly data
 # Germany

#d1 <- read_excel("2023-11-07 - OECD short term indicators.xlsx")
d_germany_short <- d1

# Germany
#keep German data
d_germany_short <- d_germany_short %>%  
              filter(d_germany_short$Country=="Germany")

# 

d_germany_short <- d_germany_short %>%  
  pivot_wider(names_from = "Subject", values_from = "Value") %>%
  group_by(TIME) %>%
  summarise_all(~max(., na.rm = TRUE))

d_germany_short[d_germany_short == -Inf] <- 0

# France

d_france_short <- d1


d_france_short <- d_france_short %>%  
  filter(d_france_short$Country=="France")

d_france_short <- d_france_short %>%  
  pivot_wider(names_from = "Subject", values_from = "Value") %>%
  group_by(TIME) %>%
  dplyr::summarise_all(~max(., na.rm = TRUE))

#United-States

d_United_States_short <- d1

d_United_States_short <- d_United_States_short %>%  
  filter(d_United_States_short$Country=="France")


d_United_States_short <- d_United_States_short %>%  
  pivot_wider(names_from = "Subject", values_from = "Value") %>%
  group_by(TIME) %>%
  dplyr::summarise_all(~max(., na.rm = TRUE))


### 2) handling quarterly data
#d2 <- read_excel("2023-11-20 - OECD quarterly national account.xlsx")
# Germany


d_germany_quarterly <- d2

# Germany
#keep German data
d_germany_quarterly <- d_germany_quarterly %>%  
  filter(d_germany_quarterly$Country=="Germany")

# 

d_germany_quarterly <- d_germany_quarterly %>%  
  pivot_wider(names_from = "Subject", values_from = "Value") %>%
  group_by(TIME) %>%
  dplyr::summarise_all(~max(., na.rm = TRUE))

# France

d_france_quarterly <- d2


d_france_quarterly <- d_france_quarterly %>%  
  filter(d_france_quarterly$Country=="France")

d_france_quarterly <- d_france_quarterly %>%  
  pivot_wider(names_from = "Subject", values_from = "Value") %>%
  group_by(TIME) %>%
  dplyr::summarise_all(~max(., na.rm = TRUE))

#United-States

d_United_States_quarterly <- d2

d_United_States_quarterly <- d_United_States_quarterly %>%  
  filter(d_United_States_quarterly$Country=="France")


d_United_States_quarterly <- d_United_States_quarterly %>%  
  pivot_wider(names_from = "Subject", values_from = "Value") %>%
  group_by(TIME) %>%
  dplyr::summarise_all(~max(., na.rm = TRUE))

colnames(d_france_short)


### 3) Use monthly data and build them as quarterly data


#sapply(d_germany_short$`Overnight interbank rate`, class)

d_germany_short_to_quarterly <- d_germany_short

d_germany_short_to_quarterly$trimestrial_overnight_interbank_rate <- 
  rollapply(d_germany_short_to_quarterly$"Overnight interbank rate", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_Long_term_interest_rate <- 
  rollapply(d_germany_short_to_quarterly$"Long-term interest rate", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_exchange_rates_monthly_averages_national_currency_per_US_dollar <- 
  rollapply(d_germany_short_to_quarterly$"Exchange rates,  monthly averages, National currency per US dollar", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_3_month_interbank_rate <- 
  rollapply(d_germany_short_to_quarterly$"3 month interbank rate", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_Leading_indicator_amplitude_adjusted <- 
  rollapply(d_germany_short_to_quarterly$"Leading indicator, amplitude adjusted", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_GDP_ratio_to_trend_smoothed <- 
  rollapply(d_germany_short_to_quarterly$"GDP (ratio to trend, smoothed)", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_monthly_unemployment_rate <- 
  rollapply(d_germany_short_to_quarterly$"Monthly unemployment rate: all persons, s,a,", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_producer_prices_manufacturing <- 
  rollapply(d_germany_short_to_quarterly$"Producer prices - Manufacturing", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_imports_in_goods <- 
  rollapply(d_germany_short_to_quarterly$"Imports in goods, s,a,", width = 3, FUN = sum, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_reatil_trade_volume <- 
  rollapply(d_germany_short_to_quarterly$"Retail trade (Volume), s,a,", width = 3, FUN = sum, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_exports_goods <- 
  rollapply(d_germany_short_to_quarterly$"Exports in goods, s,a,", width = 3, FUN = sum, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_indusdtrial_production<- 
  rollapply(d_germany_short_to_quarterly$"Industrial production, s,a,", width = 3, FUN = sum, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_manufacturing_confidence_indicator<- 
  rollapply(d_germany_short_to_quarterly$"Manufacturing - Confidence indicator, s,a,", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_consumer_confidence_indicator<- 
  rollapply(d_germany_short_to_quarterly$"Consumer confidence indicator, s,a,", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_consumer_prices_all_items<- 
  rollapply(d_germany_short_to_quarterly$"Consumer prices: all items", width = 3, FUN = mean, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_permits_issued_dwellings<- 
  rollapply(d_germany_short_to_quarterly$"Permits issued for dwellings, s,a,", width = 3, FUN = sum, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_passenger_car_registration<- 
  rollapply(d_germany_short_to_quarterly$"Passenger car registrations, s,a,", width = 3, FUN = sum, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_construction<- 
  rollapply(d_germany_short_to_quarterly$"Construction, s,a,", width = 3, FUN = sum, fill = NA, align = "right")

d_germany_short_to_quarterly$trimestrial_total_manufacturing<- 
  rollapply(d_germany_short_to_quarterly$"Total manufacturing, s,a,", width = 3, FUN = sum, fill = NA, align = "right")


## Keeping only quarterly rows (ends of trimesters)
d_germany_short_to_quarterly <- d_germany_short_to_quarterly %>% 
                                  filter(row_number() %% 3 ==0)



d_germany_quarterly <- d_germany_quarterly %>%
  mutate(
    # Extract year and quarter
    year = as.character(substring(TIME, 1, 4)),
    quarter = as.character(substring(TIME, 6, 7)),
    
    # Create a lookup table for quarter to month mapping
    quarter_to_month = case_when(
      quarter == 'Q1' ~ '03',
      quarter == 'Q2' ~ '06',
      quarter == 'Q3' ~ '09',
      quarter == 'Q4' ~ '12'
    ),
    
    # Combine year and mapped month to create a new column
    TIME = gsub(" ", "",paste(year,'-',quarter_to_month))
  )

d_germany_complete <- left_join(d_germany_quarterly, d_germany_short_to_quarterly, by="TIME")
colnames(d_germany_complete)
d_germany_complete <- d_germany_complete[, !names(d_germany_complete) %in% c("Unit Code.y","Unit.y", "PowerCode Code.y","PowerCode.y","Reference Period Code.y", "Reference Period.y",
                                              "Flag Codes.y", "Flags.y", "year","quarter", "quarter_to_month", "SUBJECT.y", "LOCATION.y","Country.y",
                                              "MEASURE.y", "Measure.y", "FREQUENCY.y", "Frequency.y", "Time", "FREQUENCY.x","Frequency.x", "Period",
                                              "Unit Code.x","Unit.x", "PowerCode Code.x", "PowerCode.x", "Reference Period Code.x", "Reference Period.x",
                                              "Flag Codes.x", "Flags.x", "Measure.x", "MEASURE.x", "Subject.x", "SUBJECT.x")
                                              ]

names(d_germany_complete)[names(d_germany_complete) == 'LOCATION.x'] <- 'Location'
names(d_germany_complete)[names(d_germany_complete) == 'Country.x'] <- 'Country'  

write.csv(d_germany_complete, "Germany_complete_quarterly_data")
write.csv(d_germany_short, "Germany_monthly_data")
