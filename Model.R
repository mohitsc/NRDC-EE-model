# Technical Potential Model 1

# Import necessary packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(xlsx)
library(readr)
library(tidyselect)

# Inputs ------------------------------------------------------------------
# Climate Zones
climate_zone_list <- tbl_df(read_excel("Potential_Model_Input_Tables/climate_zone_list.xlsx", sheet = "R_input"))

# Houses per CZ 
regional_population <- tbl_df(read_excel("Potential_Model_Input_Tables/regional_population_data.xlsx", sheet = "R_input"))

# Technology List
technology_list <- tbl_df(read_excel("Potential_Model_Input_Tables/independent_tech_list.xlsx", sheet = "R_input"))

# Technology Group Density List
tech_density <- tbl_df(read_excel("Potential_Model_Input_Tables/tech_group_density_table.xlsx", sheet = "R_input"))

# Technology Lifetime Costs
tech_costs <- tbl_df(read_excel("Potential_Model_Input_Tables/tech_costs_table.xlsx", sheet = "R_input"))

# Measure Table
measure_table <- tbl_df(read_excel("Potential_Model_Input_Tables/sample_measure_table.xlsx", 
                                   sheet = "R_input"))
#Consumption Table
consumption_table <- tbl_df(read_excel("Potential_Model_Input_Tables/input_consumption_table.xlsx", sheet = "R_input"))

# 2018 Modeled saturation data
saturation_input <- tbl_df(read_excel("Potential_Model_Input_Tables/saturation_input_data.xlsx", 
                                         sheet = "R_input"))



# Per Unit Savings Table -----------------------------------------------------------

per_unit_savings_table <- merge(measure_table, 
                                consumption_table, 
                                by.x = c("base_tech_name", "building_type"), 
                                by.y = c("tech_name", "building_type"), 
                                all.y = FALSE)

per_unit_savings_table <- merge(per_unit_savings_table, 
                                consumption_table, 
                                by.x = c("efficient_tech_name", "climate_zone", "building_type"), 
                                by.y = c("tech_name", "climate_zone", "building_type")) %>%
  rename(base_consumption_kwh = base_consumption_kwh.x,
         base_consumption_therms = base_consumption_therms.x,
         efficient_consumption_kwh = base_consumption_kwh.y,
         efficient_consumption_therms = base_consumption_therms.y)

per_unit_savings_table <- merge(per_unit_savings_table, 
                                consumption_table, 
                                by.x = c("code_tech_name", "climate_zone", "building_type"), 
                                by.y = c("tech_name", "climate_zone", "building_type")) %>%
  rename(base_consumption_kwh = base_consumption_kwh.x,
         base_consumption_therms = base_consumption_therms.x,
         code_consumption_kwh = base_consumption_kwh.y,
         code_consumption_therms = base_consumption_therms.y)

per_unit_savings_table <- per_unit_savings_table %>% 
  mutate(savings_kwh = ifelse(delivery_type == "RET", 
                              base_consumption_kwh - efficient_consumption_kwh, 
                              code_consumption_kwh - efficient_consumption_kwh),
         savings_therms = ifelse(delivery_type == "RET", 
                                 base_consumption_therms - efficient_consumption_therms, 
                                 code_consumption_therms - efficient_consumption_therms)) %>%
  select(measure, 
         base_tech_name, 
         efficient_tech_name, 
         delivery_type, 
         climate_zone,
         building_type,
         measure_applicability, 
         population_applicability,
         delivery_type_proportion,
         savings_kwh, 
         savings_therms) %>% arrange(measure, climate_zone)


# Stock Turnover Modeling-------------------------------------------------

#Projecting forward
current_year <- 2018
project_until <- 2030

tech_population <- merge(regional_population,
                         tech_density,
                         by = "climate_zone") %>% 
  mutate(tech_group_population = number_of_buildings * fraction_of_homes_ownership) %>%
  select(climate_zone, 
         building_type, 
         technology_group, 
         tech_group_population)

#saturation to number of models
tech_population <- merge(saturation_input, 
                         tech_population,
                         by = c("climate_zone", "building_type", "technology_group"))
tech_population <- mutate(tech_population, 
                           number_of_models = tech_group_population * saturation) %>%
  select(-tech_group_population,
         -saturation)


savings_and_saturation_table <- merge(per_unit_savings_table, 
                                      tech_population, 
                                      by.x = c("base_tech_name", "climate_zone", "building_type"), 
                                      by.y = c("tech_name", "climate_zone", "building_type"), 
                                      all.y = FALSE) %>% 
  mutate(measure_limit = measure_applicability * population_applicability * delivery_type_proportion * number_of_models)

#divide into RET and ROB subsets to adjust installs for first year of measure adoption
#all RET measures adopted in first year
#ROB measures adopted at rate of measure limit divided by EUL each year
RET_subset <- filter(savings_and_saturation_table, delivery_type == "RET") %>% 
  mutate(installs_2019 = measure_limit)
ROB_subset <- filter(savings_and_saturation_table, delivery_type == "ROB") %>% 
  mutate(installs_2019 = measure_limit/EUL)

installs_per_year <- rbind(RET_subset, ROB_subset)
names(installs_per_year)[names(installs_per_year) == "number_of_models"] <- "base_population_start"
installs_per_year <- mutate(installs_per_year, cumulative_installs = installs_2019) 

for(year in (current_year+2):project_until){
  installs_per_year <- mutate(installs_per_year, 
                              cumulative_installs = ifelse((cumulative_installs + measure_limit/EUL) <= measure_limit, 
                                                           cumulative_installs + measure_limit/EUL,
                                                           measure_limit)) 
  installs_per_year <- bind_cols(installs_per_year, 
                                 temp = ifelse((installs_per_year$cumulative_installs + installs_per_year$measure_limit/installs_per_year$EUL) <= installs_per_year$measure_limit, 
                                               installs_per_year$measure_limit/installs_per_year$EUL,
                                               installs_per_year$measure_limit - installs_per_year$cumulative_installs))
  
  names(installs_per_year)[names(installs_per_year) == "temp"] <- paste0("installs_", year)
}

installs_per_year <- select(installs_per_year, 
                            base_tech_name,
                            efficient_tech_name,
                            climate_zone,
                            building_type,
                            delivery_type,
                            base_population_start,
                            measure_limit,
                            cumulative_installs,
                            installs_2019,
                            installs_2020:installs_2030) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone)

write.xlsx(as.data.frame(installs_per_year), 
           "Potential_Model_Output_Tables/tech_potential_installs.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

# Statewide Technical Potential-------------------------------------------
technical_potential <- merge(per_unit_savings_table,
                             installs_per_year,
                             by = c("base_tech_name", 
                                    "efficient_tech_name", 
                                    "climate_zone",
                                    "building_type",
                                    "delivery_type"))


kwh_savings <- function(installs) {
  savings <- installs * technical_potential$savings_kwh
  return(savings)
}

therms_savings <- function(installs) {
  savings <- installs * technical_potential$savings_therms
  return(savings)
}

technical_potential_kwh <- mutate_at(technical_potential, vars(contains("installs")), .funs = kwh_savings) %>% 
  select(-savings_kwh,
         -savings_therms,
         -base_population_start,
         -measure_limit,
         -measure_applicability,
         -delivery_type_proportion,
         -population_applicability) %>% 
  rename("cumulative_savings" = cumulative_installs) %>%
  rename_at(vars(contains("installs_")), funs(paste0("savings_kwh_", parse_number(.)))) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone)

write.xlsx(as.data.frame(technical_potential_kwh), 
           "Potential_Model_Output_Tables/tech_potential_kwh_savings.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

technical_potential_therms <- mutate_at(technical_potential, vars(contains("installs")), .funs = therms_savings) %>% 
  select(-savings_kwh,
         -savings_therms,
         -base_population_start,
         -measure_limit,
         -measure_applicability,
         -delivery_type_proportion,
         -population_applicability) %>% 
  rename("cumulative_savings" = cumulative_installs) %>%
  rename_at(vars(contains("installs_")), funs(paste0("savings_therms_", parse_number(.)))) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone)

write.xlsx(as.data.frame(technical_potential_therms), 
           "Potential_Model_Output_Tables/tech_potential_therms_savings.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

# Lifetime savings table
lifetime_savings_kwh <- technical_potential_kwh %>% 
  group_by(measure, delivery_type) %>%
  summarise_at(vars(savings_kwh_2019:savings_kwh_2030), sum)

lifetime_savings_kwh <- merge(select(measure_table, measure, base_tech_name), 
                              lifetime_savings_kwh, 
                              by = "measure") %>% distinct()

lifetime_savings_therms <- technical_potential_therms %>% 
  group_by(measure, delivery_type) %>%
  summarise_at(vars(savings_therms_2019:savings_therms_2030), sum)

lifetime_savings_therms <- merge(select(measure_table, measure, base_tech_name), 
                              lifetime_savings_therms, 
                              by = "measure") %>% distinct()
