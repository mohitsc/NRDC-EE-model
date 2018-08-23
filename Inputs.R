#07/06/2018
#Energy Efficiency Potential and Goals: Study for 2018 and Beyond
#CPUC MICS 2017 Database

# Import necessary packages ----------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(xlsx)
library(readr)



# PG Data Input -------------------------------------------------------------------------------------------------------
pg_water_data <- read_excel("PG_Data+Reports/PG_WaterHeaterSubset.xlsx")

working_PG_data <- tbl_df(pg_water_data)
working_PG_data <- select(pg_water_data,
                          -"Technology ID", 
                          -"Unique Technology  Name", 
                          -"Service Territory", 
                          -"Primary Utility Type",
                          -"Base Year Efficiency Level",
                          -"Year Technology Becomes Code",
                          -"Conv or Emerging",
                          -"End Use Category",
                          -"Sector",
                          -"Scaling Basis",
                          -"Unit Basis")

#renaming to enable dplyr function use
working_PG_data <- rename(working_PG_data,
                          utility = "Utility",
                          tech_description = "Technology Description",
                          tech_name = "Common Technology Name",
                          climate_zone = "Climate Zone",
                          building_type = "Building Type",
                          replacement_type = "Replacement Type",
                          accelerated_replacement = "Accelerated Replacement?",
                          retrofit_add_on = "Retrofit Add-on?",
                          lifetime_technology = "Technology Lifetime",
                          early_retirement_RUL = "Early Retirement RUL",
                          repair_EUL = "Repair EUL",
                          electric_energy_savings_loadshape = "Electric Energy Savings Loadshape",
                          electric_energy_consumption = "Electric Energy Consumption",
                          electric_coincident_peak_demand = "Electric Coincident Peak Demand",
                          gas_savings_loadshape = "Gas Savings Loadshape",
                          gas_consumption = "Gas Consumption",	
                          savings_source = "Savings Source(s)",
                          technology_cost = "Technology Cost",
                          technology_cost_data_year = "Technology Cost Data Year",
                          labor_cost = "Labor Cost",
                          laor_cost_data_year = "Labor Cost Data Year",
                          cost_sources = "Cost Source(s)",
                          technology_group = "Technology Group",
                          tech_group = "Common Technology Group",
                          total_tech_group_density = "Total Technology Group Density",
                          technical_suitability = "Technical Suitability",
                          tech_initial_saturation = "Technology Initial Saturation",	
                          density_sources = "Density/ Applicability Source(s)",
                          ntg_factor = "NTG Factor (1-FreeRiders)",
                          deer_id = "DEER ID(s)"
)

#Fix common tech name
working_PG_data <- separate(working_PG_data, tech_name, into = c("other", "tech_name"), extra = "merge")
working_PG_data <- separate(working_PG_data, building_type, into = c("other2", "building_type"), extra = "merge") %>% 
  distinct() %>% select(-other,-other2)

working_PG_data <- separate(working_PG_data, climate_zone, sep = '-' , into = c("climate_zone", "location")) 
working_PG_data <- separate(working_PG_data, climate_zone, 
                            sep = 'CZ' , 
                            into = c("other", "climate_zone"))%>%
  select(-other)

working_PG_data <- transform(working_PG_data, climate_zone = as.numeric(unlist(climate_zone)))



# Writing PG data -------------------------------------------------------------------------
groups_and_measures <- select(working_PG_data, 
                              tech_name, 
                              lifetime_technology,
                              tech_group) %>% distinct()
write.xlsx(as.data.frame(groups_and_measures), 
           "PG_Data+Reports/PG_technology_list.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")

# Writing PG consumption data 
consumption <- select(working_PG_data, 
                      climate_zone,
                      tech_name,
                      building_type,
                      electric_energy_consumption,
                      gas_consumption,
                      electric_coincident_peak_demand) %>% distinct() %>% arrange(climate_zone, tech_name)

write.xlsx(as.data.frame(consumption), 
           "PG_Data+Reports/PG_consumption_table.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")


# Writing saturation by climate zone data 
saturation_PG_data <- select(working_PG_data,
                              climate_zone,
                              tech_name,
                              tech_group,
                              tech_initial_saturation)%>% 
  arrange(climate_zone)

saturation_PG_data <- arrange(saturation_PG_data, tech_name) %>% 
  distinct() %>% 
  group_by(climate_zone, tech_name)

tech_saturation <- tbl_df(summarize(saturation_PG_data, 
                                    mean_saturation = mean(tech_initial_saturation),
                                    min_saturation = min(tech_initial_saturation),
                                    max_saturation = max(tech_initial_saturation))) %>%
  arrange(climate_zone, tech_name) %>% 
  mutate(check = (max_saturation - min_saturation)/mean_saturation)

write.xlsx(as.data.frame(tech_saturation), 
           "PG_Data+Reports/PG_tech_saturation_table.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")



#Climate Zone inputs ---------------------------------------------------------------------------------------
climate_zones <- select(working_PG_data, climate_zone, location) %>%
  distinct() %>% arrange(climate_zone)

write.xlsx(as.data.frame(climate_zones), 
           "Potential_Model_Input_Tables/climate_zone_list.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")




# Writing tech costs data ------------------------------------------------------------------------------------------
tech_costs <- select(working_PG_data, 
                     tech_name,
                     technology_cost,
                     labor_cost) %>% distinct()

write.xlsx(as.data.frame(tech_costs), 
           "Potential_Model_Input_Tables/tech_costs_table.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")



# Writing density data ----------------------------------------------------------------------------------------------
#Density of gas and electric WH
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

# Writing housing data ----------------------------------------------------------------------------------------------
#CEC Title 24 Climate Zone Housing and Density Data

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



# Writing gas water heater size data ----------------------------------------------------------------------------------------------
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

# #Inputting and cleaning Pierre's ECOTOPE HPWH data ----------------------------------------------------------------------------------------------

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



# Water heater installation location data ----------------------------------------------------------------------------------------
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



# Calculating consumption values of base measures -------------------------
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

#calculating kwh consumption of base measure by HPWH_consumption * HPWH_COP / Base_Tech_Efficiency_Factor
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



# Saturation data from CLASS 2012: modeling tech saturation till 2018 --------------------------------

# Technology Saturation by EF and CZ
initial_tech_saturation <- tbl_df(read_excel("Input_to_Input_Tables/CLASS_instantaneous_and_storage_EF_cz.xlsx", 
                                             sheet = "R_input"))
initial_tech_saturation[is.na(initial_tech_saturation)] <- 0

saturation_cz_year_wise <- list()
saturation_cz_year_wise[[2012]] <- initial_tech_saturation



start_year <- 2013
current_year <- 2018

tech_saturation <- initial_tech_saturation
tech_saturation <- tech_saturation %>% 
  mutate( "Small Gas Storage Water Heater (0.48 EF - 0.559 EF)" = rowSums(select(tech_saturation,
                                                                                 "0.48-0.519 EF":"0.52-0.559 EF")),
          "Small Gas Storage Water Heater (0.56 EF - 0.599 EF)" = rowSums(select(tech_saturation,
                                                                                 "0.56-0.599 EF")),
          "Small Gas Storage Water Heater (0.60 EF - 0.639 EF)" = tech_saturation$`0.60-0.639 EF`, 
          #"Instantaneous Gas Water Heater (0.80 EF - 0.879 EF)" = rowSums(select(tech_saturation, 
          #                                                                      "0.80-0.839 EF":"0.84-0.879 EF")),
          "GE 2014 Heat Pump Water Heater- low cost" = 0,
          "GE 2014 Heat Pump Water Heater- medium cost" = 0,
          "GE 2014 Heat Pump Water Heater- high cost" = 0) %>%
  select(climate_zone, 
         "Small Gas Storage Water Heater (0.48 EF - 0.559 EF)",
         "Small Gas Storage Water Heater (0.56 EF - 0.599 EF)",
         "Small Gas Storage Water Heater (0.60 EF - 0.639 EF)", 
         #"Instantaneous Gas Water Heater (0.80 EF - 0.879 EF)",
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

#merging EUL from tech_list with saturation values 
saturation_cz_year_wise[[2018]] <- merge(saturation_cz_year_wise[[2018]], 
                                         select(technology_list, EUL, tech_name), 
                                         by = "tech_name") %>%
  mutate(building_type = "Single Family",
         technology_group = ifelse(tech_name == "GE 2014 Heat Pump Water Heater- low cost" | 
                                  tech_name == "GE 2014 Heat Pump Water Heater- medium cost" |
                                  tech_name == "GE 2014 Heat Pump Water Heater- high cost" ,
                                   "Elec Water Heaters",
                                  "Gas Water Heaters"))

write.xlsx(as.data.frame(saturation_cz_year_wise[[2018]]), 
           "Potential_Model_Input_Tables/saturation_input_data.xlsx", 
           row.names = FALSE,
           sheetName = "R_input")
