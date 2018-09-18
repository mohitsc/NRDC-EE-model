# Technical Potential Model 1

# Import necessary packages ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(xlsx)
library(readr)
library(tidyselect)
library(readbulk)
library(data.table)

# Inputs ------------------------------------------------------------------
# Climate Zones
climate_zone_list <- tbl_df(read_excel("Potential_Model_Input_Tables/climate_zone_list.xlsx", sheet = "R_input"))

# Houses per CZ 
regional_population <- tbl_df(read_excel("Potential_Model_Input_Tables/regional_population_data.xlsx", sheet = "R_input"))

# Technology List
technology_list <- tbl_df(read_excel("Potential_Model_Input_Tables/independent_tech_list.xlsx", sheet = "R_input"))

# Technology Group Density List
tech_density <- tbl_df(read_excel("Potential_Model_Input_Tables/tech_group_density_table.xlsx", sheet = "R_input"))

# Measure Table
measure_table <- tbl_df(read_excel("Potential_Model_Input_Tables/sample_measure_table.xlsx", 
                                   sheet = "R_input"))
# Consumption Table
consumption_table <- tbl_df(read_excel("Potential_Model_Input_Tables/input_consumption_table.xlsx", sheet = "R_input"))

# 2018 Modeled saturation data
saturation_input <- tbl_df(read_excel("Potential_Model_Input_Tables/saturation_input_data.xlsx", 
                                         sheet = "R_input"))

# Operational Costs for base year
annual_operational_costs <- tbl_df(read_excel("Potential_Model_Input_Tables/annual_operational_costs.xlsx"))

# rate change projection by utility
rate_pct_change <- tbl_df(read_excel("Input_to_Input_Tables/climate_zone_rate_mapping.xlsx", sheet = "forecast"))

# rate change projection by utility
ghg_loadshapes_kwh <- tbl_df(read_excel("Potential_Model_Input_Tables/ghg_loadshape.xlsx", sheet = "electric"))
ghg_loadshapes_therms <- tbl_df(read_excel("Potential_Model_Input_Tables/ghg_loadshape.xlsx", sheet = "gas"))



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
  mutate(savings_over_base_kwh = base_consumption_kwh - efficient_consumption_kwh,
         savings_over_code_kwh = code_consumption_kwh - efficient_consumption_kwh,
         savings_over_base_therms = base_consumption_therms - efficient_consumption_therms,
         savings_over_code_therms = code_consumption_therms - efficient_consumption_therms) %>%
  select(measure, 
         base_tech_name, 
         efficient_tech_name, 
         climate_zone,
         building_type,
         measure_applicability, 
         population_applicability,
         savings_over_base_kwh, 
         savings_over_code_kwh,
         savings_over_base_therms,
         savings_over_code_therms) %>% arrange(base_tech_name, efficient_tech_name, climate_zone) %>% distinct()


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
         tech_group = technology_group, 
         tech_group_population)

#saturation to number of models
tech_population <- merge(saturation_input, 
                         tech_population,
                         by = c("climate_zone", "building_type", "tech_group"))
tech_population <- mutate(tech_population, 
                           number_of_models = tech_group_population * saturation) %>%
  select(-tech_group_population,
         -saturation)


savings_and_saturation_table <- merge(measure_table, 
                                      tech_population, 
                                      by.x = c("base_tech_name", "building_type"), 
                                      by.y = c("tech_name", "building_type"), 
                                      all.y = FALSE) %>% 
  mutate(measure_limit = measure_applicability * population_applicability * delivery_type_proportion * number_of_models)

#divide into RET and ROB subsets to adjust installs for first year of measure adoption
#all RET measures adopted in first year
#ROB measures adopted at rate of measure limit divided by EUL each year

RET_subset <- filter(savings_and_saturation_table, delivery_type == "RET") %>% 
  mutate(installs_first_year = measure_limit)
ROB_subset <- filter(savings_and_saturation_table, delivery_type == "ROB") %>% 
  mutate(installs_first_year = measure_limit/EUL)

installs_per_year <- rbind(RET_subset, ROB_subset)
names(installs_per_year)[names(installs_per_year) == "number_of_models"] <- "base_population_start"
installs_per_year <- mutate(installs_per_year, cumulative_installs = installs_first_year) 

names(installs_per_year)[names(installs_per_year) == "installs_first_year"] <- paste0("installs_", current_year+1)

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
                            EUL,
                            measure_limit,
                            cumulative_installs,
                            vars_select(names(installs_per_year), contains("installs"))) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

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
                                    "building_type")) %>% arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

RET_subset <- filter(technical_potential, delivery_type == "RET")
ROB_subset <- filter(technical_potential, delivery_type == "ROB")

over_code_kwh_savings <- function(installs) {
  savings <- installs * per_unit_savings_table$savings_over_code_kwh
  return(savings)
}

over_code_therms_savings <- function(installs) {
  savings <- installs * per_unit_savings_table$savings_over_code_therms
  return(savings)
}

over_base_kwh_savings <- function(installs) {
  savings <- installs * per_unit_savings_table$savings_over_base_kwh
  return(savings)
}

over_base_therms_savings <- function(installs) {
  savings <- installs * per_unit_savings_table$savings_over_base_therms
  return(savings)
}

annual_technical_potential_kwh <- rbind(mutate_at(ROB_subset, 
                                                  vars(contains("installs")),
                                                  .funs = over_code_kwh_savings),
                                        mutate_at(RET_subset, 
                                                  vars(contains("installs")), 
                                                  .funs = over_base_kwh_savings)) %>% 
  select(-savings_over_base_therms,
         -savings_over_base_kwh,
         -savings_over_code_therms,
         -savings_over_code_kwh,
         -base_population_start,
         -measure_limit,
         -measure_applicability,
         -population_applicability) %>% 
  rename("cumulative_savings" = cumulative_installs) %>%
  rename_at(vars(contains("installs_")), funs(paste0("savings_kwh_", parse_number(.)))) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

write.xlsx(as.data.frame(annual_technical_potential_kwh), 
           "Potential_Model_Output_Tables/tech_potential_kwh_savings.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

annual_technical_potential_therms <- rbind(mutate_at(ROB_subset, 
                                                  vars(contains("installs")),
                                                  .funs = over_code_therms_savings),
                                        mutate_at(RET_subset, 
                                                  vars(contains("installs")), 
                                                  .funs = over_base_therms_savings)) %>% 
  select(-savings_over_base_therms,
         -savings_over_base_kwh,
         -savings_over_code_therms,
         -savings_over_code_kwh,
         -base_population_start,
         -measure_limit,
         -measure_applicability,
         -population_applicability) %>% 
  rename("cumulative_savings" = cumulative_installs) %>%
  rename_at(vars(contains("installs_")), funs(paste0("savings_therms_", parse_number(.)))) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

write.xlsx(as.data.frame(annual_technical_potential_therms), 
           "Potential_Model_Output_Tables/tech_potential_therms_savings.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

########################################## Lifetime savings table for kwh ########################################## 
lifetime_savings_kwh <- select(annual_technical_potential_kwh,
                               base_tech_name:EUL,
                               "cumulative_savings" = vars_select(names(annual_technical_potential_kwh),
                                                                contains(as.character(current_year+1)))) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

cumulative_savings_kwh <- lifetime_savings_kwh

names(lifetime_savings_kwh)[names(lifetime_savings_kwh) == "cumulative_savings"] <- paste0("cumulative_savings_",
                                                                                           as.character(current_year+1) )



post_RUL <- select(annual_technical_potential_kwh, base_tech_name:delivery_type)
ROB_subset <- filter(post_RUL, delivery_type == "ROB")
RET_subset <- filter(post_RUL, delivery_type == "RET")

RET_subset <- bind_cols(RET_subset, 
                        "post_RUL_savings" = over_code_kwh_savings(installs_per_year$measure_limit[installs_per_year$delivery_type == "RET"])) 
 
ROB_subset <- bind_cols(ROB_subset, 
                        "post_RUL_savings" = rep(0, 
                                         nrow(filter(annual_technical_potential_kwh, 
                                                     delivery_type == "ROB"))))
post_RUL <- rbind(ROB_subset,
                  RET_subset) %>% arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

cumulative_savings_kwh <- merge(cumulative_savings_kwh, post_RUL, by = c("base_tech_name", 
                                                                         "efficient_tech_name",
                                                                         "climate_zone",
                                                                         "delivery_type",
                                                                         "building_type",
                                                                         "measure"))  %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)
                                    

for(year in (current_year+2):project_until){
  
  year_column <- select(annual_technical_potential_kwh,
                        base_tech_name:delivery_type,
                        vars_select(names(annual_technical_potential_kwh),
                                    contains(as.character(year))))
  
  cumulative_savings_kwh <- merge(cumulative_savings_kwh,
                                  year_column,
                                  by = c("base_tech_name", 
                                         "efficient_tech_name",
                                         "climate_zone",
                                         "delivery_type",
                                         "building_type",
                                         "measure")) %>% 
    arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)
  
  names(cumulative_savings_kwh)[names(cumulative_savings_kwh) == names(cumulative_savings_kwh)[ncol(cumulative_savings_kwh)]] <- "temp"
  
  cumulative_savings_kwh <- cumulative_savings_kwh %>% rowwise() %>%
    mutate(cumulative_savings = ifelse(delivery_type == "ROB",
                                       cumulative_savings + temp,
                                       ifelse(year > (current_year + (EUL/3)),
                                              post_RUL_savings,
                                              cumulative_savings)))
  
  cumulative_savings_kwh <- select(cumulative_savings_kwh, -temp)
  
  lifetime_savings_kwh <- bind_cols(lifetime_savings_kwh,
                                    "savings" = cumulative_savings_kwh$cumulative_savings)
  
  names(lifetime_savings_kwh)[names(lifetime_savings_kwh) == "savings"] <- paste0("cumulative_savings_", year)
  
}

lifetime_savings_kwh <- lifetime_savings_kwh %>% select(-EUL, -measure)


########################################## Lifetime savings table for therms ########################################## 
lifetime_savings_therms <- select(annual_technical_potential_therms,
                               base_tech_name:EUL,
                               "cumulative_savings" = vars_select(names(annual_technical_potential_therms),
                                                                  contains(as.character(current_year+1)))) %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

cumulative_savings_therms <- lifetime_savings_therms

names(lifetime_savings_therms)[names(lifetime_savings_therms) == "cumulative_savings"] <- paste0("cumulative_savings_",
                                                                                           as.character(current_year+1) )


post_RUL <- select(annual_technical_potential_therms, base_tech_name:delivery_type)
ROB_subset <- filter(post_RUL, delivery_type == "ROB")
RET_subset <- filter(post_RUL, delivery_type == "RET")

RET_subset <- bind_cols(RET_subset, 
                        "post_RUL_savings" = over_code_therms_savings(installs_per_year$measure_limit[installs_per_year$delivery_type == "RET"])) 

ROB_subset <- bind_cols(ROB_subset, 
                        "post_RUL_savings" = rep(0, 
                                                 nrow(filter(annual_technical_potential_therms, 
                                                             delivery_type == "ROB"))))
post_RUL <- rbind(ROB_subset,
                  RET_subset) %>% arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

cumulative_savings_therms <- merge(cumulative_savings_therms, post_RUL, by = c("base_tech_name", 
                                                                         "efficient_tech_name",
                                                                         "climate_zone",
                                                                         "delivery_type",
                                                                         "building_type",
                                                                         "measure"))  %>% 
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)


for(year in (current_year+2):project_until){
  
  year_column <- select(annual_technical_potential_therms,
                        base_tech_name:delivery_type,
                        vars_select(names(annual_technical_potential_therms),
                                    contains(as.character(year))))
  
  cumulative_savings_therms <- merge(cumulative_savings_therms,
                                  year_column,
                                  by = c("base_tech_name", 
                                         "efficient_tech_name",
                                         "climate_zone",
                                         "delivery_type",
                                         "building_type",
                                         "measure")) %>% 
    arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)
  
  names(cumulative_savings_therms)[names(cumulative_savings_therms) == names(cumulative_savings_therms)[ncol(cumulative_savings_therms)]] <- "temp"
  
  cumulative_savings_therms <- cumulative_savings_therms %>% rowwise() %>%
    mutate(cumulative_savings = ifelse(delivery_type == "ROB",
                                       cumulative_savings + temp,
                                       ifelse(year > (current_year + (EUL/3)),
                                              post_RUL_savings,
                                              cumulative_savings)))
  
  cumulative_savings_therms <- select(cumulative_savings_therms, -temp)
  
  lifetime_savings_therms <- bind_cols(lifetime_savings_therms,
                                    "savings" = cumulative_savings_therms$cumulative_savings)
  
  names(lifetime_savings_therms)[names(lifetime_savings_therms) == "savings"] <- paste0("cumulative_savings_", year)
  
}

lifetime_savings_therms <- lifetime_savings_therms %>% select(-EUL, -measure)

write.xlsx(as.data.frame(lifetime_savings_kwh), 
           "Potential_Model_Output_Tables/lifetime_savings_kwh.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

write.xlsx(as.data.frame(lifetime_savings_therms), 
           "Potential_Model_Output_Tables/lifetime_savings_therms.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")


# incremental first year costs -------------------------------------------------------------------------------
# RET costs = efficient costs
first_year_costs_RET <- merge(select(technology_list, tech_name, costs), 
                                        filter(measure_table, delivery_type == "RET"),
                                        by.x = "tech_name",
                                        by.y = "efficient_tech_name") %>% 
  rename("efficient_tech_name" = tech_name,
         "first_year_incremental_cost" = costs)

first_year_costs_RET <- first_year_costs_RET %>% 
  select(base_tech_name,
         code_tech_name,
         efficient_tech_name,
         delivery_type,
         building_type,
         first_year_incremental_cost)

# ROB costs = efficient costs - code costs

first_year_costs_ROB <- merge(select(technology_list, tech_name, costs), 
                                                                   filter(measure_table, delivery_type == "ROB"),
                                                                   by.x = "tech_name",
                                                                   by.y = "efficient_tech_name") %>% 
  rename("efficient_costs" = costs,
         "efficient_tech_name" = tech_name)

first_year_costs_ROB <- bind_cols(first_year_costs_ROB,
                                             select(merge(select(technology_list, tech_name, costs), 
                                                          filter(measure_table, delivery_type == "ROB"),
                                                          by.x = "tech_name",
                                                          by.y = "code_tech_name"), 
                                                    "costs")) %>% 
  rename("code_costs" = costs)

first_year_costs_ROB <- first_year_costs_ROB %>% 
  mutate(costs = efficient_costs - code_costs) %>%
  select(base_tech_name,
         code_tech_name,
         efficient_tech_name,
         delivery_type,
         building_type,
         first_year_incremental_cost = costs)

first_year_costs <- rbind(first_year_costs_ROB, first_year_costs_RET)
rm(first_year_costs_RET, first_year_costs_ROB)

first_year_costs <- first_year_costs %>%
  arrange(base_tech_name, efficient_tech_name, delivery_type)

write.xlsx(as.data.frame(first_year_costs), 
           "Potential_Model_Output_Tables/first_year_costs.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")
# 
# #Total first year costs including installs
# RET_subset <- filter(technical_potential, delivery_type == "RET")
# ROB_subset <- filter(technical_potential, delivery_type == "ROB")
# 
# #adding ROB costs column
# total_first_year_costs_ROB <- merge(select(ROB_subset, 
#                                        base_tech_name, 
#                                        efficient_tech_name, 
#                                        climate_zone, 
#                                        delivery_type, 
#                                        building_type,
#                                        vars_select(names(installs_per_year), contains("installs"))),
#                                 first_year_costs_ROB,
#                                 by = c("base_tech_name", 
#                                        "efficient_tech_name", 
#                                        "delivery_type", 
#                                        "building_type"))
#                                 
# total_first_year_costs_ROB <- mutate_at(total_first_year_costs_ROB, 
#                                     vars(contains("installs")), 
#                                     funs(. * incremental_first_year_cost)) 
# 
# total_first_year_costs_ROB <- rename_at(total_first_year_costs_ROB,
#                                     vars(contains("installs_")), 
#                                     funs(paste0("costs_", parse_number(.)))) %>%
#   rename("cumulative_costs" = cumulative_installs)
# 
# total_first_year_costs <- total_first_year_costs %>% 
#   mutate(cumulative_costs = rowSums(select(., contains("costs_"))))
# 

# Operational cost savings for years between 2019 and 2030-------------------------------------------------------------------------------

#discounted future costs = costs/(1+r)^n
discount_rate <- 0.03
# rate increases 
avg_increase = summarise_all(rate_pct_change, mean, na.rm = TRUE)

operational_cost_savings <- merge(select(measure_table, base_tech_name:delivery_type, building_type), 
                                  annual_operational_costs, 
                                  by.x = c("base_tech_name", "building_type"), 
                                  by.y = c("tech_name", "building_type")) %>%
  rename("base_opr_costs_TOU" = opr_costs_TOU,
         "base_opr_costs_non_TOU" = opr_costs_non_TOU)

operational_cost_savings <- merge(operational_cost_savings, 
                                  annual_operational_costs, 
                                  by.x = c("efficient_tech_name", "building_type", "climate_zone"), 
                                  by.y = c("tech_name", "building_type", "climate_zone")) %>%
  rename("efficient_opr_costs_TOU" = opr_costs_TOU,
         "efficient_opr_costs_non_TOU" = opr_costs_non_TOU)

operational_cost_savings <- merge(operational_cost_savings, 
                                  annual_operational_costs, 
                                  by.x = c("code_tech_name", "building_type", "climate_zone"), 
                                  by.y = c("tech_name", "building_type", "climate_zone")) %>%
  rename("code_opr_costs_TOU" = opr_costs_TOU,
         "code_opr_costs_non_TOU" = opr_costs_non_TOU)

operational_cost_savings <- merge(operational_cost_savings, 
                                  select(technology_list, tech_name, EUL),
                                  by.x = "base_tech_name", 
                                  by.y = "tech_name")

operational_cost_savings <- operational_cost_savings %>%
  select(base_tech_name,
         code_tech_name,
         efficient_tech_name,
         climate_zone,
         delivery_type,
         building_type,
         base_EUL = EUL,
         base_opr_costs_TOU,
         code_opr_costs_TOU,
         efficient_opr_costs_TOU,
         base_opr_costs_non_TOU,
         code_opr_costs_non_TOU,
         efficient_opr_costs_non_TOU)

apply_rate <- function(tech_name_input){
  group = technology_list$tech_group[technology_list$tech_name==tech_name_input]
  return (ifelse(group == "Elec Water Heaters",
                 avg_increase$electricity_rates_increase, 
                 avg_increase$gas_rates_increase))
}


#YEARWISE OPR COST SAVINGS

for(year in (current_year+1):project_until){
  #Calling function to increase rates each year
  operational_cost_savings <- operational_cost_savings %>% rowwise() %>%
    mutate(base_opr_costs_TOU = (1+apply_rate(base_tech_name)) * base_opr_costs_TOU)
  operational_cost_savings <- operational_cost_savings %>% rowwise() %>%
    mutate(code_opr_costs_TOU = (1+apply_rate(code_tech_name)) * code_opr_costs_TOU)
  operational_cost_savings <- operational_cost_savings %>% rowwise() %>%
    mutate(efficient_opr_costs_TOU = (1+apply_rate(efficient_tech_name)) * efficient_opr_costs_TOU)
  operational_cost_savings <- operational_cost_savings %>% rowwise() %>%
    mutate(base_opr_costs_non_TOU = (1+apply_rate(base_tech_name)) * base_opr_costs_non_TOU)
  operational_cost_savings <- operational_cost_savings %>% rowwise() %>%
    mutate(code_opr_costs_non_TOU = (1+apply_rate(code_tech_name)) * code_opr_costs_non_TOU)
  operational_cost_savings <- operational_cost_savings %>% rowwise() %>%
    mutate(efficient_opr_costs_non_TOU = (1+apply_rate(efficient_tech_name)) * efficient_opr_costs_non_TOU)
  
  #calculating savings each measure
  operational_cost_savings <- bind_cols(operational_cost_savings, 
                                 temp = ifelse(operational_cost_savings$delivery_type == "ROB",
                                               operational_cost_savings$code_opr_costs_non_TOU - operational_cost_savings$efficient_opr_costs_non_TOU,
                                               ifelse(year<(current_year + (operational_cost_savings$base_EUL/3)), 
                                                      operational_cost_savings$base_opr_costs_non_TOU - operational_cost_savings$efficient_opr_costs_non_TOU,
                                                      operational_cost_savings$code_opr_costs_non_TOU - operational_cost_savings$efficient_opr_costs_non_TOU)
                                               ))
  
  names(operational_cost_savings)[names(operational_cost_savings) == "temp"] <- paste0("non_TOU_savings_", year)
  
  operational_cost_savings <- bind_cols(operational_cost_savings, 
                                        temp = ifelse(operational_cost_savings$delivery_type == "ROB",
                                                      operational_cost_savings$code_opr_costs_TOU - operational_cost_savings$efficient_opr_costs_TOU,
                                                      ifelse(year<(current_year + (operational_cost_savings$base_EUL/3)), 
                                                             operational_cost_savings$base_opr_costs_TOU - operational_cost_savings$efficient_opr_costs_TOU,
                                                             operational_cost_savings$code_opr_costs_TOU - operational_cost_savings$efficient_opr_costs_TOU)
                                        ))

  names(operational_cost_savings)[names(operational_cost_savings) == "temp"] <- paste0("TOU_savings_", year)
}

 operational_cost_savings <- operational_cost_savings %>% 
   select(-(base_EUL:efficient_opr_costs_non_TOU))

#applying discount rate of base_year_savings/(1+discount_rate)^(year- current_year)

original_names <- names(operational_cost_savings)

for(savings_column in names(operational_cost_savings)){
  if(grepl("savings", savings_column)){
    temp1 = operational_cost_savings[savings_column]/(1 + discount_rate)^(parse_number(savings_column) - current_year)
    operational_cost_savings <- bind_cols(operational_cost_savings, temp1)
    operational_cost_savings <- operational_cost_savings[ , !(names(operational_cost_savings) == savings_column)]
  }
}
names(operational_cost_savings) <- original_names

operational_cost_savings <- operational_cost_savings %>%
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

write.xlsx(as.data.frame(operational_cost_savings), 
           "Potential_Model_Output_Tables/operational_cost_savings.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

# Cashflow table
cumulative_gain <- -1 * (first_year_costs$first_year_incremental_cost)
cashflow <- function(savings){
  cumulative_gain <<- cumulative_gain + savings
  return(cumulative_gain)
}

cashflow_tables <- operational_cost_savings %>%
  mutate_at(vars(contains("non_TOU_savings")), cashflow)

cumulative_gain <- -1 * (first_year_costs$first_year_incremental_cost)

cashflow_tables <- cashflow_tables %>%
  mutate_at(vars(starts_with("TOU_savings")), cashflow)

cashflow_tables <- merge(cashflow_tables, first_year_costs, 
                        by = c("base_tech_name", 
                               "code_tech_name", 
                               "efficient_tech_name", 
                               "delivery_type", 
                               "building_type"))

cashflow_tables <- cashflow_tables %>%
  mutate(first_year_incremental_cost = -1 * first_year_incremental_cost)

cashflow_tables <- cashflow_tables %>%
  select(base_tech_name:climate_zone,
         first_year_incremental_cost,
         non_TOU_savings_2019:TOU_savings_2030) %>%
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

non_TOU_cashflow <- select(cashflow_tables, 
                           base_tech_name:first_year_incremental_cost, 
                           vars_select(names(cashflow_tables),
                                       contains("non_TOU")))
TOU_cash_flow <- select(cashflow_tables, 
                        base_tech_name:first_year_incremental_cost, 
                        vars_select(names(cashflow_tables),
                                    starts_with("TOU_savings")))

write.xlsx(as.data.frame(cashflow_tables), 
           "Potential_Model_Output_Tables/cashflow_tables.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

############################################## GHG Analysis #########################################################

net_emissions <- select(lifetime_savings_kwh,
                        base_tech_name:delivery_type)

for(loop_year in (current_year+1):project_until){
  kwh_column <- select(lifetime_savings_kwh,
                        base_tech_name:delivery_type,
                        vars_select(names(lifetime_savings_kwh),
                                    contains(as.character(loop_year))))
  year_ghg_loadshape <- filter(ghg_loadshapes_kwh,
                               year == loop_year)
  
  newcol_kwh <- merge(kwh_column, year_ghg_loadshape, by = "climate_zone")
  newcol_kwh <- newcol_kwh %>% 
    mutate_at(vars(contains("savings")), funs(.*weighted_tCO2_per_kwh)) %>%
    rename_at(vars(contains("savings")), funs(paste0("ghg_kwh"))) %>%
    select(-year, -weighted_tCO2_per_kwh)
  
  therm_column <- select(lifetime_savings_therms,
                       base_tech_name:delivery_type,
                       vars_select(names(lifetime_savings_therms),
                                   contains(as.character(loop_year))))
  year_ghg_loadshape <- filter(ghg_loadshapes_therms,
                               year == loop_year)
  newcol_therms <- merge(therm_column, year_ghg_loadshape, by = "climate_zone")
  newcol_therms <- newcol_therms %>% 
    mutate_at(vars(contains("savings")), funs(.*weighted_tCO2_per_therm)) %>%
    rename_at(vars(contains("savings")), funs(paste0("ghg_therms"))) %>%
    select(-year, -weighted_tCO2_per_therm)
  
  new_col <- merge(newcol_kwh, 
                   newcol_therms, 
                   by = c("base_tech_name", 
                          "efficient_tech_name",
                          "climate_zone", 
                          "delivery_type", 
                          "building_type"))
  new_col <- new_col %>% 
    mutate(ghg_savings = ghg_kwh + ghg_therms) %>% 
    select(-ghg_kwh, -ghg_therms)
  
  net_emissions <- merge(net_emissions,
                         new_col,
                         by = c("base_tech_name", 
                                       "efficient_tech_name",
                                       "climate_zone", 
                                       "delivery_type", 
                                       "building_type"))
  names(net_emissions)[names(net_emissions) == "ghg_savings"] <- paste0("ghg_savings_", loop_year)
}                          

write.xlsx(as.data.frame(net_emissions), 
           "Potential_Model_Output_Tables/net_emissions.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

####################################### Graphing Emissions and costs ##############################################

graphing_emissions<- gather(net_emissions,
                            year,
                            ghg_savings,
                            -(base_tech_name:building_type)) %>% 
  mutate(year = parse_number(year)) %>% 
  group_by(base_tech_name, efficient_tech_name, year)

#Emissions savings by measure, taking sum of climate zones and looking only at ROB
filter(graphing_emissions, delivery_type == "ROB") %>%
  summarise(ghg_savings_tCO2 = sum(ghg_savings)) %>% 
  ggplot(aes(x = year, y = ghg_savings_tCO2)) +
  geom_line(size = 1) + 
  theme_bw() + 
  geom_line() +
  facet_wrap(~ base_tech_name) + 
  theme(text = element_text(size=9.5),
        axis.text.x = element_text(angle=15, hjust=1))


  