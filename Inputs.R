#07/06/2018
#Inputs to NRDC Tech Potential and Economic Analysis Model

# Import necessary packages ----------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(ggrepel)
library(xlsx)
library(readr)
library(tidyselect)
library(readbulk)
library(data.table)
library(RColorBrewer)

# Climate Zone Inputs ---------------------------------------------------------------------------------------
#Energy Efficiency Potential and Goals: Study for 2018 and Beyond
#CPUC MICS 2017 Database
pg_water_data <- tbl_df(read_excel("PG_Data+Reports/PG_WaterHeaterSubset.xlsx"))
pg_water_data <- select(pg_water_data,
                          "Climate Zone") %>% rename(climate_zone = "Climate Zone")

pg_water_data <- separate(pg_water_data, climate_zone, sep = '-' , into = c("climate_zone", "location")) 
pg_water_data <- separate(pg_water_data, climate_zone, 
                            sep = 'CZ' , 
                            into = c("other", "climate_zone"))%>%
  select(-other)

pg_water_data <- transform(pg_water_data, climate_zone = as.numeric(unlist(climate_zone)))

climate_zones <- select(pg_water_data, climate_zone, location) %>%
  distinct() %>% arrange(climate_zone)

write.xlsx(as.data.frame(climate_zones), 
           "Potential_Model_Input_Tables/climate_zone_list.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")

# Density Inputs ----------------------------------------------------------------------------------------------
#Density of gas and electric WH from RASS 2009
density_gas_wh <- read_xlsx("RASS/density_gas_WH_RASS.xlsx", sheet = "R_input")
density_elec_wh <- read_xlsx("RASS/density_elec_WH_RASS.xlsx", sheet = "R_input")
tech_group_cz_density <- cbind(density_elec_wh[,"climate_zone"],
                               density_elec_wh[,"fraction_Elec Water Heaters"], 
                               density_gas_wh[,"fraction_Gas Water Heaters"])

tech_group_cz_density <- gather(tech_group_cz_density, 
                                "fraction_Elec Water Heaters", 
                                "fraction_Gas Water Heaters", 
                                -climate_zone) %>% 
  rename(technology_group = "fraction_Elec Water Heaters",
         fraction_of_homes_ownership = "fraction_Gas Water Heaters") %>% 
  arrange(climate_zone) %>% 
  separate(technology_group, sep = "fraction_", into = c("other", "technology_group")) %>%
  select(-other)

write.xlsx(as.data.frame(tech_group_cz_density), 
           "Potential_Model_Input_Tables/tech_group_density_table.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")

# Housing Inputs ----------------------------------------------------------------------------------------------
#RASS Climate Zone Housing and Density Data

#defining single family home as 1-4 units, multi family home as 5+ units

housing_cz_data <- tbl_df(read_xlsx("RASS/RASS_households_climate_zone.xlsx", sheet = "R_input")) %>% 
  mutate(houses_single_family = single_family_detached + townhouse_duplex_rowhouse + apt_condo_2_to_4) %>%
  mutate(houses_multi_family = apt_condo_5_plus) %>%
  select(climate_zone, houses_single_family, houses_multi_family, mobile_home, total)

housing_cz_data <- gather(housing_cz_data,
                          key = building_type,
                          houses_single_family, 
                          houses_multi_family,
                          mobile_home,
                          total,
                          -climate_zone,
                          value = number_of_buildings) %>%
  arrange(climate_zone) 

housing_cz_data$building_type<- replace(housing_cz_data$building_type, 
                                        housing_cz_data$building_type == "houses_single_family", "Single Family") 
housing_cz_data$building_type<- replace(housing_cz_data$building_type, 
                                        housing_cz_data$building_type == "houses_multi_family", "Multi Family")
housing_cz_data$building_type<- replace(housing_cz_data$building_type, 
                                        housing_cz_data$building_type == "total", "Total")
housing_cz_data$building_type<- replace(housing_cz_data$building_type, 
                                        housing_cz_data$building_type == "mobile_home", "Mobile Home")

write.xlsx(as.data.frame(housing_cz_data), 
           "Potential_Model_Input_Tables/regional_population_data.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")


#HPWH Consumption Inputs ----------------------------------------------------------------------------------------------
#NRDC/Ecotope HPWH Consumption Study 2016
HPWH_data <- tbl_df(read_xlsx("Input_to_Input_Tables/HPWH_Pierre_data.xlsx", sheet = "R_Input")) %>%
  select(-Draw_profile,
         -State,
         -City,
         -Nb_persons,
         -HPWH_model) %>% 
  rename(climate_zone = CA_climate_zone) %>% 
  filter(HPWH_model_2 == "GE2014")

HPWH_data <- separate(HPWH_data, climate_zone, 
                      sep = 'CZ' , 
                      into = c("other", "climate_zone")) %>%
  select(-other) %>% 
  transform(climate_zone = as.numeric(unlist(climate_zone)))

write.xlsx(as.data.frame(HPWH_data), 
           "Input_to_Input_Tables/HPWH_R_input.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")



# Installation Location and Size Weighting Calculation----------------------------------------------------------------------------------------

# Gas Water Heater Size Inputs 
#CLASS 2012
#Climate Zones and proportion of gas water heaters 50< and 50> gallons


gas_wh_size_data <- tbl_df(read_xlsx("Input_to_Input_Tables/CLASS_wh_sizes.xlsx", sheet = "R_input"))
gas_wh_size_data <- rename(gas_wh_size_data, 
                           "50" = "0-59_Gallons",
                           "80" = "60+_Gallons")

gas_wh_size_data <- gather(gas_wh_size_data, size, weighting, -climate_zone)

write.xlsx(as.data.frame(gas_wh_size_data), 
           "Input_to_Input_Tables/gas_wh_size_table.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")

#installation location
wh_location_data <- tbl_df(read_xlsx("NEEA/wh_installation_location_NEEA.xlsx"))
wh_location_data <- rename(wh_location_data, 
                           location = "Water Heater Location",
                           weighting = Region)
wh_location_data$weighting[1] = wh_location_data$weighting[1] + wh_location_data$weighting[2]
wh_location_data$weighting[3] = wh_location_data$weighting[3] + wh_location_data$weighting[5]
wh_location_data$location[4] = "Vented closet"
wh_location_data <- filter(wh_location_data, location != "Crawlspace" & location != "Other")

write.xlsx(as.data.frame(wh_location_data), 
           "Input_to_Input_Tables/wh_location_data.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")


# Weighting HPWH Consumption by location and size -------------------------------------------------------------
# WH Location data
location_installation <- tbl_df(read_excel("Input_to_Input_Tables/wh_location_data.xlsx", sheet = "R_input"))
# Technology Size by CZ
tech_size <- tbl_df(read_excel("Input_to_Input_Tables/gas_wh_size_table.xlsx", sheet = "R_input"))
#Pierre Data
HPWH_consumption <- tbl_df(read_excel("Input_to_Input_Tables/HPWH_R_input.xlsx", sheet = "R_input"))

location_weighted_HPWH <- merge(HPWH_consumption, 
                                location_installation, 
                                by.x = "Location", 
                                by.y = "location")

location_weighted_HPWH <- location_weighted_HPWH %>%
  mutate(loc_wt_cop = Tmy_weighted_COP * weighting,
         loc_wt_input_energy = Tmy_weighted_Input_Energy * weighting) %>%  
  group_by(climate_zone, 
           HPWH_size,
           HPWH_model_2) %>%
  summarise(COP = sum(loc_wt_cop), 
            consumption = sum(loc_wt_input_energy))

final_weighted_HPWH <- merge(location_weighted_HPWH, 
                             tech_size, 
                             by.x = c("HPWH_size", "climate_zone"), 
                             by.y = c("size", "climate_zone"))

final_weighted_HPWH <- final_weighted_HPWH %>%  
  mutate(weighted_COP = COP * weighting,
         weighted_consumption = consumption * weighting) %>% 
  group_by(HPWH_model_2, climate_zone) %>%  
  summarise(weighted_COP = sum(weighted_COP), 
            weighted_consumption_kWh = sum(weighted_consumption))

final_weighted_HPWH <- final_weighted_HPWH %>% 
  ungroup() %>% 
  mutate(efficient_tech_name = ifelse(HPWH_model_2 == "GE2014", "GE 2014 Heat Pump Water Heater", HPWH_model_2)) %>%
  select(efficient_tech_name, climate_zone, weighted_COP, weighted_consumption_kWh)

write.xlsx(as.data.frame(final_weighted_HPWH), 
           "Input_to_Input_Tables/HPWH_weighted_consumption.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")



# Base Technology Consumption Calculation -------------------------
# Technology List
technology_list <- tbl_df(read_excel("Potential_Model_Input_Tables/independent_tech_list.xlsx", sheet = "R_input"))

technology_consumption <- tbl_df(NULL)

#creating a dataframe with 16 climate zones for each tech name
for(cz in final_weighted_HPWH$climate_zone){
  for(name in technology_list$tech_name){
    technology_consumption <- rbind(technology_consumption, 
                                    filter(technology_list, tech_name == name))
  }
}

technology_consumption <- technology_consumption %>% 
  arrange(tech_name) %>% 
  mutate(climate_zone = rep(1:16, times = length(technology_list$tech_name))) %>% 
  arrange(climate_zone)

technology_consumption <- merge(technology_consumption, 
                                final_weighted_HPWH, 
                                by = "climate_zone")

technology_consumption <- technology_consumption %>%
  rename(weighted_HPWH_consumption_kwh = weighted_consumption_kWh,
         weighted_HPWH_cop = weighted_COP)

#calculating kwh consumption of base technology by HPWH_consumption * HPWH_COP / Base_Tech_Efficiency_Factor
technology_consumption$efficiency_factor <- ifelse(technology_consumption$tech_name == "GE 2014 Heat Pump Water Heater- low cost" | 
                                                     technology_consumption$tech_name == "GE 2014 Heat Pump Water Heater- medium cost" |
                                                     technology_consumption$tech_name == "GE 2014 Heat Pump Water Heater- high cost" ,
                                                   1.1 * technology_consumption$weighted_HPWH_cop,
                                                   0.9 * technology_consumption$efficiency_factor)

technology_consumption <- technology_consumption %>% 
  mutate(base_consumption_kwh = weighted_HPWH_cop * weighted_HPWH_consumption_kwh / efficiency_factor,
         base_consumption_therms = 0.03412956 * base_consumption_kwh)

technology_consumption <- technology_consumption %>% 
  mutate(base_consumption_kwh = ifelse(tech_group == "Gas Water Heaters", 0, base_consumption_kwh)) %>%
  mutate(base_consumption_therms = ifelse(tech_group == "Elec Water Heaters", 0, base_consumption_therms)) %>%
  mutate(building_type = "Single Family")

input_consumption_table <- select(technology_consumption,
                                  tech_name,
                                  climate_zone,
                                  building_type,
                                  base_consumption_kwh,
                                  base_consumption_therms)

write.xlsx(as.data.frame(input_consumption_table), 
           "Potential_Model_Input_Tables/input_consumption_table.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")



# Saturation Input ----------------------------------------------------------------
#CLASS 2012 
# Technology Saturation by Uniform Efficiency Factor

#Gas

initial_tech_saturation_gas <- tbl_df(read_excel("Input_to_Input_Tables/CLASS_instantaneous_and_storage_EF_cz.xlsx", 
                                             sheet = "R_input"))
initial_tech_saturation_gas[is.na(initial_tech_saturation_gas)] <- 0


saturation_cz_year_wise <- list()
saturation_cz_year_wise[[2012]] <- initial_tech_saturation_gas



start_year <- 2013
current_year <- 2018

tech_saturation <- initial_tech_saturation_gas
tech_saturation <- tech_saturation %>% 
  mutate( "Small Gas Storage Water Heater (0.48 EF - 0.559 EF)" = rowSums(select(tech_saturation,
                                                                                 "0.48-0.519 EF":"0.52-0.559 EF")),
          "Small Gas Storage Water Heater (0.56 EF - 0.599 EF)" = rowSums(select(tech_saturation,
                                                                                 "0.56-0.599 EF")),
          "Small Gas Storage Water Heater (0.60 EF - 0.639 EF)" = tech_saturation$`0.60-0.639 EF`, 
          
          "GE 2014 Heat Pump Water Heater- low cost" = 0,
          "GE 2014 Heat Pump Water Heater- medium cost" = 0,
          "GE 2014 Heat Pump Water Heater- high cost" = 0) %>%
  select(climate_zone, 
         "Small Gas Storage Water Heater (0.48 EF - 0.559 EF)",
         "Small Gas Storage Water Heater (0.56 EF - 0.599 EF)",
         "Small Gas Storage Water Heater (0.60 EF - 0.639 EF)", 
         
         "GE 2014 Heat Pump Water Heater- low cost",
         "GE 2014 Heat Pump Water Heater- medium cost",
         "GE 2014 Heat Pump Water Heater- high cost")

tech_saturation <- arrange(gather(tech_saturation, tech_name, saturation, -climate_zone), climate_zone)

#projecting saturation values from 2013 to 2018
#replacement rate of 1/EUL, half replaced by code, half replaced by efficient
#instantaneous gas water heater remains the same
#Energy Star data for CA would make this assumption more accurate
for(year in start_year:current_year){
  EUL <- 15
  below_code <- filter(tech_saturation, tech_name == "Small Gas Storage Water Heater (0.48 EF - 0.559 EF)")
  code_before_2015 <- filter(tech_saturation, tech_name == "Small Gas Storage Water Heater (0.56 EF - 0.599 EF)")
  code_after_2015 <- filter(tech_saturation, tech_name == "Small Gas Storage Water Heater (0.60 EF - 0.639 EF)")
  #instant <- filter(tech_saturation, tech_name == "Instantaneous Gas Water Heater (0.80 EF - 0.879 EF)")
  HPWH_population <- filter(tech_saturation, tech_name == "GE 2014 Heat Pump Water Heater- low cost" | 
                              tech_name == "GE 2014 Heat Pump Water Heater- medium cost" |
                              tech_name == "GE 2014 Heat Pump Water Heater- high cost")
  if(year < 2015){
    code_after_2015$saturation <- code_after_2015$saturation + 
      (below_code$saturation * 0.5 * (1/EUL)) + 
      (code_before_2015$saturation * (1/EUL))
    
    
    code_before_2015$saturation <- code_before_2015$saturation * (1-(1/EUL))
    code_before_2015$saturation <- code_before_2015$saturation + below_code$saturation * 0.5 * (1/EUL)
    
    below_code$saturation <- below_code$saturation * (1-(1/EUL))
  }
  else{
    code_after_2015$saturation <- code_after_2015$saturation + 
      (code_before_2015$saturation * (1/EUL)) + 
      (below_code$saturation * (1/EUL))
    
    code_before_2015$saturation <- code_before_2015$saturation * (1-(1/EUL))
    
    below_code$saturation <- below_code$saturation * (1-(1/EUL))
  }
  
  tech_saturation <- arrange(rbind(below_code, code_before_2015, code_after_2015, HPWH_population), climate_zone)
  saturation_cz_year_wise[[year]] <- tech_saturation
} 


#Electric

initial_tech_saturation_elec <- tbl_df(read_excel("Input_to_Input_Tables/CLASS_Electric_Saturation.xlsx", 
                                                  sheet = "R_input"))
initial_tech_saturation_elec[is.na(initial_tech_saturation_elec)] <- 0


tech_saturation <- initial_tech_saturation_elec
tech_saturation <- tech_saturation %>% 
  mutate( "Small Electric Storage Water Heater (0.86 EF)" = tech_saturation$`0.84-0.879 EF`,
          "Small Electric Storage Water Heater (0.90 EF)" = tech_saturation$`0.88-0.919 EF`,
          "High Eff. Small Electric Storage Water Heater (0.945 EF)" = tech_saturation$`0.92-0.959 EF`) %>%
  select(climate_zone, 
         "Small Electric Storage Water Heater (0.86 EF)",
         "Small Electric Storage Water Heater (0.90 EF)",
         "High Eff. Small Electric Storage Water Heater (0.945 EF)")

tech_saturation <- arrange(gather(tech_saturation, tech_name, saturation, -climate_zone), climate_zone)

#projecting saturation values from 2013 to 2018
#replacement rate of 1/EUL, half replaced by code, half replaced by efficient
#instantaneous gas water heater remains the same
#Energy Star data for CA would make this assumption more accurate
for(year in start_year:current_year){
  EUL <- 13
  below_code <- filter(tech_saturation, tech_name == "Small Electric Storage Water Heater (0.86 EF)")
  code_before_2015 <- filter(tech_saturation, tech_name == "Small Electric Storage Water Heater (0.90 EF)")
  code_after_2015 <- filter(tech_saturation, tech_name == "High Eff. Small Electric Storage Water Heater (0.945 EF)")

  if(year < 2015){
    code_after_2015$saturation <- code_after_2015$saturation + 
      (below_code$saturation * 0.5 * (1/EUL)) + 
      (code_before_2015$saturation * (1/EUL))
    
    
    code_before_2015$saturation <- code_before_2015$saturation * (1-(1/EUL))
    code_before_2015$saturation <- code_before_2015$saturation + below_code$saturation * 0.5 * (1/EUL)
    
    below_code$saturation <- below_code$saturation * (1-(1/EUL))
  }
  else{
    code_after_2015$saturation <- code_after_2015$saturation + 
      (code_before_2015$saturation * (1/EUL)) + 
      (below_code$saturation * (1/EUL))
    
    code_before_2015$saturation <- code_before_2015$saturation * (1-(1/EUL))
    
    below_code$saturation <- below_code$saturation * (1-(1/EUL))
  }
  
  tech_saturation <- arrange(rbind(below_code, code_before_2015, code_after_2015), climate_zone)
  saturation_cz_year_wise[[year]] <- rbind(saturation_cz_year_wise[[year]], tech_saturation)
} 

#merging EUL from tech_list with saturation values 
saturation_cz_year_wise[[2018]] <- merge(saturation_cz_year_wise[[2018]], 
                                         select(technology_list, EUL, tech_group, tech_name), 
                                         by = "tech_name") %>%
  mutate(building_type = "Single Family")

saturation_cz_year_wise[[2018]] <- saturation_cz_year_wise[[2018]] %>% 
  arrange(tech_name, climate_zone)

write.xlsx(as.data.frame(saturation_cz_year_wise[[2018]]), 
           "Potential_Model_Input_Tables/saturation_input_data.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")

# Rates Input ----------------------------------------------------
# Pierre HPWH Flexibility Study, simulation using PGE values
# TOU
TOU_rates <- tbl_df(read_excel("Input_to_Input_Tables/Hourly price schedules v13.xlsx", sheet = "R_input"))

TOU_rates <- rename(TOU_rates,
                    "1" = CZ1,
                    "2" = CZ2,
                    "3" = CZ3,
                    "4" = CZ4,
                    "5" = CZ5,
                    "6" = CZ6,
                    "7" = CZ7,
                    "8" = CZ8,
                    "9" = CZ9,
                    "10" = CZ10,
                    "11" = CZ11,
                    "12" = CZ12,
                    "13" = CZ13,
                    "14" = CZ14,
                    "15" = CZ15,
                    "16" = CZ16)

TOU_rates <- gather(TOU_rates,
                    climate_zone,
                    NRDC_TOU_rate,
                    "1":"16",
                    -("8760":Hr))

TOU_rates <- mutate(TOU_rates,
                    hour = Hr + 1) 

TOU_rates <- select(TOU_rates,
                    climate_zone,
                    season = Season,
                    month = Month,
                    day = Day,
                    hour,
                    hour_of_year = "8760",
                    NRDC_TOU_rate)

months_days <- select(TOU_rates, month, day) %>% distinct() %>% mutate(day_of_year = row_number())
TOU_rates <- merge(TOU_rates, months_days, by = c("month", "day"))
rm(months_days)


TOU_rates <- select(TOU_rates, 
                    climate_zone,
                    day_of_year,
                    hour,
                    hour_of_year,
                    NRDC_TOU_rate) %>% arrange(climate_zone,day_of_year, hour)

#import gas rates for each climate zone
gas_and_non_TOU_rates <- tbl_df(read_excel("Input_to_Input_Tables/climate_zone_rate_mapping.xlsx", sheet = "rates"))

rates <- merge(TOU_rates, gas_and_non_TOU_rates, 
           by = "climate_zone")

#Adjusting TOU rates to adjust for PGE source of NRDC TOU calculations
PGE_rate <- select(gas_and_non_TOU_rates, electric_utility, electric_rate) %>% 
  filter(electric_utility== "PG&E") %>% 
  distinct()

#For some reason there are two identical PGE entries even after applying distinct(), so choosing first row
rates <- rates %>% 
  mutate(NRDC_TOU_rate = NRDC_TOU_rate * electric_rate / as.numeric(PGE_rate[1,2]))

rm(TOU_rates, gas_and_non_TOU_rates)

fwrite(as.data.frame(rates), 
       "Input_to_Input_Tables/rates.csv", 
       row.names = FALSE)

# Loadshape Inputs  --------------------------------------------------------------------------------------
# NRDC/ECOTOPE Flexibility Study

loadshapes <-  read_bulk(directory = "LoadShapes")

loadshapes <- select(loadshapes,
                     climate_zone,
                     day_of_year = dayOfYear,
                     hour,
                     vars_select(names(loadshapes), contains("input")),
                     standby_kWh,
                     File)

loadshapes <- loadshapes %>%
  rowwise() %>%
  mutate(input_kwh = sum(input_kWh1, 
                         input_kWh2, 
                         input_kWh3, 
                         standby_kWh, 
                         na.rm=TRUE)) %>%
  select(-input_kWh1,
         -input_kWh2,
         -input_kWh3,
         -standby_kWh)

loadshapes <- loadshapes %>%  
  mutate(loadshape_label = case_when(grepl("AO|Rheem", File) ~ "HP_water_heating",
                               grepl("res", File, ignore.case = TRUE) ~"elec_gas_water_heating")) %>%
  select(-File)

#Take average of AO Smith and Rheem to generate avg. HPWH consumption per hour per CZ
loadshapes <- group_by(loadshapes,
                       loadshape_label,
                       climate_zone,
                       day_of_year,
                       hour)

loadshapes <- summarise(loadshapes, input_kwh = mean(input_kwh))

annual_consumption_loadshape <- group_by(loadshapes,
                               loadshape_label,
                               climate_zone) %>% summarise(annual_input_kwh = sum(input_kwh))

loadshapes <- merge(loadshapes, annual_consumption_loadshape, by = c("loadshape_label", "climate_zone"))

loadshapes <- loadshapes %>% 
  mutate(loadshape = input_kwh/annual_input_kwh) %>% 
  select(-annual_input_kwh, 
         -input_kwh)
 
loadshapes <- loadshapes %>% 
  mutate(hour_8760 = rep(1:8760, times = 2 * length(climate_zones$climate_zone)))

loadshapes <- loadshapes %>%
  select(loadshape_label,
         climate_zone,
         day_of_year,
         hour,
         hour_8760,
         loadshape)

fwrite(as.data.frame(loadshapes), 
       "Input_to_Input_Tables/loadshapes.csv", 
       row.names = FALSE)

# Operational Costs Calculation  --------------------------------------------------------------------------------------
#merging loadshape with rates
operational_costs_8760 <- merge(select(loadshapes, -"hour_8760"), 
                                rates, 
                                by = c("climate_zone", "day_of_year", "hour")) %>%
  arrange(climate_zone, loadshape_label, hour_of_year)

#merging with tech_groups
operational_costs_8760 <- merge(operational_costs_8760, 
                           distinct(select(technology_list, tech_group, loadshape_label)),
                           by = "loadshape_label")


operational_costs_8760 <- operational_costs_8760 %>% 
  mutate(hourly_cost_TOU_multiplier = ifelse(tech_group == "Elec Water Heaters",
                                  NRDC_TOU_rate * loadshape,
                                  gas_rate * loadshape))

operational_costs_8760 <- operational_costs_8760 %>% 
  mutate(hourly_cost_non_TOU_multiplier = ifelse(tech_group == "Elec Water Heaters",
                                      electric_rate * loadshape,
                                      gas_rate * loadshape))

annual_operational_costs <- operational_costs_8760 %>% 
  group_by(tech_group, loadshape_label, climate_zone)

annual_operational_costs <- summarise(annual_operational_costs, 
                                      TOU_multiplier = sum(hourly_cost_TOU_multiplier), 
                                      non_TOU_multiplier = sum(hourly_cost_non_TOU_multiplier))


#merging with consumption
annual_operational_costs <- merge(annual_operational_costs,
                                  select(technology_list, tech_name, tech_group, loadshape_label),
                                  by = c("tech_group", "loadshape_label"))

annual_operational_costs <- merge(annual_operational_costs,
                                input_consumption_table,
                                by = c("tech_name", "climate_zone"))

annual_operational_costs <- annual_operational_costs %>%
  mutate(opr_costs_TOU = ifelse(tech_group == "Elec Water Heaters",
                                TOU_multiplier * base_consumption_kwh,
                                TOU_multiplier * base_consumption_therms))

annual_operational_costs <- annual_operational_costs %>%
  mutate(opr_costs_non_TOU = ifelse(tech_group == "Elec Water Heaters",
                                non_TOU_multiplier * base_consumption_kwh,
                                non_TOU_multiplier * base_consumption_therms))

annual_operational_costs <- annual_operational_costs %>%
  select(tech_name,
         climate_zone,
         building_type,
         opr_costs_TOU,
         opr_costs_non_TOU) %>%
  arrange(tech_name, climate_zone)

write.xlsx(as.data.frame(annual_operational_costs), 
           "Potential_Model_Input_Tables/annual_operational_costs.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")

# GHG Emissions Input   --------------------------------------------------------------------------------------
# converting tCO2/kWh = tCO2/MWh / 1000

# marginal greenhouse gas emissions
ghg_emissions_kwh <- tbl_df(read_excel("Input_to_Input_Tables/marginal_emissions.xlsx", 
                                       sheet = "R_input_kwh"))
ghg_emissions_therms <- tbl_df(read_excel("Input_to_Input_Tables/marginal_emissions.xlsx", 
                                          sheet = "R_input_therms"))

ghg_emissions_kwh <- mutate_at(ghg_emissions_kwh, 
                               vars(-hour),
                               funs(./1000))

ghg_emissions_kwh <- gather(ghg_emissions_kwh,
                            year,
                            tCO2_per_kwh,
                            "2018":"2030",
                            -hour)

ghg_emissions_therms <- gather(ghg_emissions_therms,
                            year,
                            tCO2_per_therm,
                            "2018":"2030",
                            -hour)

ghg_loadshape_kwh <- merge(ghg_emissions_kwh, 
                           select(loadshapes,
                                  -day_of_year,
                                  -hour),
                           by.x = "hour",
                           by.y = "hour_8760")

ghg_loadshape_kwh <- ghg_loadshape_kwh %>% 
  group_by(climate_zone, year)

ghg_loadshape_kwh <- summarise(ghg_loadshape_kwh,
                               weighted_tCO2_per_kwh = sum(loadshape * tCO2_per_kwh))

ghg_loadshape_therms <- merge(ghg_emissions_therms, 
                           select(loadshapes,
                                  -day_of_year,
                                  -hour),
                           by.x = "hour",
                           by.y = "hour_8760")

ghg_loadshape_therms <- ghg_loadshape_therms %>% 
  group_by(climate_zone, year)

ghg_loadshape_therms <- summarise(ghg_loadshape_therms,
                               weighted_tCO2_per_therm = sum(loadshape * tCO2_per_therm))

write.xlsx(as.data.frame(ghg_loadshape_kwh), 
           "Potential_Model_Input_Tables/ghg_loadshape.xlsx", 
           row.names = FALSE,
           sheetName = "electric")
write.xlsx(as.data.frame(ghg_loadshape_therms), 
           "Potential_Model_Input_Tables/ghg_loadshape.xlsx", 
           row.names = FALSE,
           append = TRUE,
           sheetName = "gas")


