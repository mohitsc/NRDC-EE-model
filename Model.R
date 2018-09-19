# Technical Potential Model 1

# Import necessary packages ---------------------------------------------------------------
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

names(cumulative_savings_kwh)[names(cumulative_savings_kwh) == "cumulative_savings"] <- "cumulative_savings_first_year"

names(lifetime_savings_kwh)[names(lifetime_savings_kwh) == "cumulative_savings"] <- paste0("cumulative_savings_",
                                                                                           as.character(current_year+1) )
                                                                                           
cumulative_savings_kwh <- cumulative_savings_kwh %>% mutate("cumulative_savings" = cumulative_savings_first_year)

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
                                              cumulative_savings + post_RUL_savings,
                                              cumulative_savings + cumulative_savings_first_year)))
  
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

names(cumulative_savings_therms)[names(cumulative_savings_therms) == "cumulative_savings"] <- "cumulative_savings_first_year"

names(lifetime_savings_therms)[names(lifetime_savings_therms) == "cumulative_savings"] <- paste0("cumulative_savings_",
                                                                                           as.character(current_year+1) )

cumulative_savings_therms <- cumulative_savings_therms %>% mutate("cumulative_savings" = cumulative_savings_first_year)


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
                                              cumulative_savings + post_RUL_savings,
                                              cumulative_savings + cumulative_savings_first_year)))
  
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
cashflow_tables <- merge(operational_cost_savings, first_year_costs, 
                         by = c("base_tech_name", 
                                "code_tech_name", 
                                "efficient_tech_name", 
                                "delivery_type", 
                                "building_type"))

cashflow_tables <- cashflow_tables %>%
  mutate(first_year_incremental_cost = -1 * first_year_incremental_cost)

cumulative_gain <- cashflow_tables$first_year_incremental_cost

#run this function only when savings = 0 in the environment 
cashflow <- function(savings){
  cumulative_gain <<- cumulative_gain + savings
  return(cumulative_gain)
}

non_TOU_cashflow_tables <- cashflow_tables %>%
  mutate_at(vars(contains("non_TOU_savings")), cashflow) %>% 
  select(base_tech_name:climate_zone,
         first_year_incremental_cost, 
         vars_select(names(cashflow_tables),
                     contains("non_TOU"))) %>%
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

cumulative_gain <- cashflow_tables$first_year_incremental_cost

TOU_cashflow_tables <- cashflow_tables %>%
  mutate_at(vars(starts_with("TOU_savings")), cashflow) %>% 
  select(base_tech_name:climate_zone,
         first_year_incremental_cost, 
         vars_select(names(cashflow_tables),
                     starts_with("TOU_savings"))) %>%
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

TOU_cashflow_tables <- merge(TOU_cashflow_tables, 
                             select(technology_list, tech_name, EUL), 
                             by.x = "base_tech_name",
                             by.y = "tech_name")

non_TOU_cashflow_tables <- merge(non_TOU_cashflow_tables, 
                             select(technology_list, tech_name, EUL), 
                             by.x = "base_tech_name",
                             by.y = "tech_name")

TOU_cashflow_tables <- merge(TOU_cashflow_tables, 
                             select(technology_list, tech_name, costs), 
                             by.x = "code_tech_name",
                             by.y = "tech_name")

non_TOU_cashflow_tables <- merge(non_TOU_cashflow_tables, 
                                 select(technology_list, tech_name, costs), 
                                 by.x = "code_tech_name",
                                 by.y = "tech_name")


for(year in (current_year+1):project_until){
  year_column <- select(TOU_cashflow_tables,
                        base_tech_name:first_year_incremental_cost,
                        code_costs = costs,
                        EUL,
                        temp = vars_select(names(TOU_cashflow_tables),
                                    contains(as.character(year))))
  
  name <- vars_select(names(TOU_cashflow_tables),
              contains(as.character(year))) 
  
  year_column <- year_column %>% mutate(temp = ifelse((delivery_type == "RET") & (year > (current_year + EUL/3)),
                                           temp + code_costs,
                                           temp))
  
  TOU_cashflow_tables[name] <- year_column$temp
  
  year_column <- select(non_TOU_cashflow_tables,
                        base_tech_name:first_year_incremental_cost,
                        code_costs = costs,
                        EUL,
                        temp = vars_select(names(non_TOU_cashflow_tables),
                                           contains(as.character(year))))
  
  name <- vars_select(names(non_TOU_cashflow_tables),
                      contains(as.character(year))) 
  
  year_column <- year_column %>% mutate(temp = ifelse((delivery_type == "RET") & (year > (current_year + EUL/3)),
                                                      temp + code_costs,
                                                      temp))
  
  non_TOU_cashflow_tables[name] <- year_column$temp
  
}

TOU_cashflow_tables <- select(TOU_cashflow_tables, 
                              base_tech_name,
                              code_tech_name,
                              efficient_tech_name,
                              climate_zone,
                              building_type,
                              delivery_type,
                              first_year_incremental_cost,
                              vars_select(names(TOU_cashflow_tables), contains("savings")))

non_TOU_cashflow_tables <- select(non_TOU_cashflow_tables, 
                              base_tech_name,
                              code_tech_name,
                              efficient_tech_name,
                              climate_zone,
                              building_type,
                              delivery_type,
                              first_year_incremental_cost,
                              vars_select(names(non_TOU_cashflow_tables), contains("savings")))

write.xlsx(as.data.frame(non_TOU_cashflow_tables), 
           "Potential_Model_Output_Tables/non_TOU_cashflow_tables.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

write.xlsx(as.data.frame(TOU_cashflow_tables), 
           "Potential_Model_Output_Tables/TOU_cashflow_tables.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

################### Estimating total non TOU societal spending by applying installs in each CZ to per unit savings ###################
non_TOU_cashflow_2022 <- gather(non_TOU_cashflow_tables,
                                    year,
                                    cashflow,
                                    -(base_tech_name:first_year_incremental_cost)) %>% 
  mutate(year = parse_number(year)) %>% filter(year == 2022)

non_TOU_cashflow_2022 <- non_TOU_cashflow_2022 %>% rowwise() %>% 
  mutate(unit_spending = ifelse(cashflow < 0, 0 - cashflow, 0)) %>% 
  select(-code_tech_name, -first_year_incremental_cost, -year, -cashflow)

non_TOU_spending <- merge(non_TOU_cashflow_2022, select(installs_per_year, 
                                                -base_population_start, 
                                                -EUL, 
                                                -measure_limit, 
                                                -cumulative_installs),
                  by = c("base_tech_name", 
                         "efficient_tech_name",
                         "climate_zone",
                         "delivery_type", 
                         "building_type"))

non_TOU_spending <- non_TOU_spending %>% mutate_at(vars(contains("installs")), funs(.*unit_spending)) %>%
  rename_at(vars(contains("installs")), funs(paste0("spending_", parse_number(.))))

original_names <- names(non_TOU_spending)

for(spending_column in names(non_TOU_spending)){
  if(grepl("spending_", spending_column)){
    temp1 = non_TOU_spending[spending_column]/(1 + discount_rate)^(parse_number(spending_column) - current_year)
    non_TOU_spending <- bind_cols(non_TOU_spending, temp1)
    non_TOU_spending <- non_TOU_spending[ , !(names(non_TOU_spending) == spending_column)]
  }
}
names(non_TOU_spending) <- original_names

non_TOU_spending <- non_TOU_spending  %>%
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

non_TOU_spending <- non_TOU_spending %>% 
  group_by(base_tech_name, efficient_tech_name, delivery_type) %>% 
  summarise_at(vars(contains("spending_")), sum) 

total_non_TOU_spending <- non_TOU_spending %>% 
  rowwise() %>% 
  mutate(total_spending = sum(spending_2019:spending_2030)) %>% 
  select(base_tech_name:delivery_type, total_spending)

################### Estimating total non TOU societal spending by applying installs in each CZ to per unit savings ###################
TOU_cashflow_2022 <- gather(TOU_cashflow_tables,
                                year,
                                cashflow,
                                -(base_tech_name:first_year_incremental_cost)) %>% 
  mutate(year = parse_number(year)) %>% filter(year == 2022)

TOU_cashflow_2022 <- TOU_cashflow_2022 %>% rowwise() %>% 
  mutate(unit_spending = ifelse(cashflow < 0, 0 - cashflow, 0)) %>% 
  select(-code_tech_name, -first_year_incremental_cost, -year, -cashflow)

TOU_spending <- merge(TOU_cashflow_2022, select(installs_per_year, 
                                                        -base_population_start, 
                                                        -EUL, 
                                                        -measure_limit, 
                                                        -cumulative_installs),
                          by = c("base_tech_name", 
                                 "efficient_tech_name",
                                 "climate_zone",
                                 "delivery_type", 
                                 "building_type"))

TOU_spending <- TOU_spending %>% mutate_at(vars(contains("installs")), funs(.*unit_spending)) %>%
  rename_at(vars(contains("installs")), funs(paste0("spending_", parse_number(.))))

original_names <- names(TOU_spending)

for(spending_column in names(TOU_spending)){
  if(grepl("spending_", spending_column)){
    temp1 = TOU_spending[spending_column]/(1 + discount_rate)^(parse_number(spending_column) - current_year)
    TOU_spending <- bind_cols(TOU_spending, temp1)
    TOU_spending <- TOU_spending[ , !(names(TOU_spending) == spending_column)]
  }
}
names(TOU_spending) <- original_names

TOU_spending <- TOU_spending  %>%
  arrange(base_tech_name, efficient_tech_name, climate_zone, delivery_type)

TOU_spending <- TOU_spending %>% 
  group_by(base_tech_name, efficient_tech_name, delivery_type) %>% 
  summarise_at(vars(contains("spending_")), sum) 

total_TOU_spending <- TOU_spending %>% 
  rowwise() %>% 
  mutate(total_spending = sum(spending_2019:spending_2030)) %>% 
  select(base_tech_name:delivery_type, total_spending)

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

######################################################### Graphs ###################################################

#Graphing cashflow ----------------------------------------------------
graphing_emissions<- gather(net_emissions,
                            year,
                            ghg_savings,
                            -(base_tech_name:building_type)) %>% 
  mutate(year = parse_number(year))

graphing_emissions <- merge(graphing_emissions, 
                            select(technology_list, tech_name, tech_group),
                            by.x = "base_tech_name",
                            by.y = "tech_name") %>%
  group_by(tech_group, base_tech_name, year)



#Emissions savings by measure, taking sum of climate zones and looking only at ROB
graphing_emissions %>%
  summarise(ghg_savings_mmtCO2 = as.integer(sum(ghg_savings))/(10^6)) %>% 
  ggplot(aes(x = year, 
             y = ghg_savings_mmtCO2, 
             color = base_tech_name,
             label=ifelse(year== 2019 | year == 2030, ghg_savings_mmtCO2,''))) +
  geom_line(size = 1) + 
  scale_color_brewer(palette="Dark2") +
  geom_point() +
  facet_wrap(~ tech_group) + 
  theme(text = element_text(size=9.5),
        axis.text.x = element_text(angle=15, hjust=1)) +
  geom_text_repel(point.padding = 1) +
  ggtitle("Statewide Lifetime GHG savings") +
  labs(x="Year",y="GHG Savings (MMT CO2)") + 
  theme(plot.title = element_text(size= 26, hjust=0)) +
  theme(axis.title = element_text(size=18)) + 
  scale_x_continuous(breaks=c(2018, 2020,2022,2024,2026,2028, 2030))

#Graphing cashflow ----------------------------------------------------

graphing_non_TOU_cashflow <- gather(non_TOU_cashflow_tables,
                                    year,
                                    cashflow,
                                    -(base_tech_name:first_year_incremental_cost)) %>% 
  mutate(year = parse_number(year)) %>% 
  group_by(base_tech_name, efficient_tech_name, year)

#average of all climate zones
filter(graphing_non_TOU_cashflow, delivery_type == "RET") %>%
  summarise(cashflow = mean(cashflow)) %>% 
  ggplot(aes(x = year, y = cashflow, color = efficient_tech_name)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2022, linetype = "dashed") + 
  geom_text(aes(x=2022, label="3 year Payback", y = 1200), color= "black", size = 4, angle = 90) + 
  theme_bw() + 
  scale_color_brewer(palette="Dark2") +
  geom_line() +
  facet_wrap(~ base_tech_name) +
  theme(text = element_text(size=9.5),
        axis.text.x = element_text(angle=15, hjust= 1)) + 
  ggtitle("Non-TOU Cashflow for each Base Model") +
  labs(x="Year",y="Net Savings ($)") + 
  theme(plot.title = element_text(size= 26, hjust=0)) +
  theme(axis.title = element_text(size=18)) + 
  scale_x_continuous(breaks=c(2018, 2020,2022,2024,2026,2028, 2030))
  
graphing_TOU_cashflow <- gather(TOU_cashflow_tables,
                                    year,
                                    cashflow,
                                    -(base_tech_name:first_year_incremental_cost)) %>% 
  mutate(year = parse_number(year)) %>% 
  group_by(base_tech_name, efficient_tech_name, year)

filter(graphing_TOU_cashflow, delivery_type == "ROB") %>%
  summarise(cashflow = mean(cashflow)) %>% 
  ggplot(aes(x = year, y = cashflow, color = efficient_tech_name)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 2022, linetype = "dashed") +
  geom_text(aes(x=2022, label="3 year Payback", y = 1200), color= "black", size = 4, angle = 90) +
  theme_bw() +
  scale_color_brewer(palette="Dark2") +
  geom_line() +
  facet_wrap(~ base_tech_name) +
  theme(text = element_text(size=9.5),
        axis.text.x = element_text(angle=15, hjust=1)) + 
  ggtitle("TOU Cashflow for each Base Model") +
  labs(x="Year",y="Net Savings ($)") + 
  theme(plot.title = element_text(size= 26, hjust=0)) +
  theme(axis.title = element_text(size=18)) + 
  scale_x_continuous(breaks=c(2018, 2020,2022,2024,2026,2028, 2030))

#Graphing spending ----------------------------------------------------
total_spending <- merge(total_non_TOU_spending, total_TOU_spending,
                        by = c("base_tech_name", "efficient_tech_name", "delivery_type")) %>%
  rename("total_non_TOU_spending" = "total_spending.x",
         "total_TOU_spending" = "total_spending.y")

total_spending <- merge(total_spending, 
                        select(technology_list, tech_name, tech_group),
                        by.x = "base_tech_name",
                        by.y = "tech_name")

total_spending <- total_spending %>%
  group_by(base_tech_name, tech_group, delivery_type)

total_spending %>%
  summarise("non_TOU_spending" = mean(total_non_TOU_spending),
            "TOU_spending" = mean(total_TOU_spending)) %>% 
  ggplot(aes(x = base_tech_name, y = non_TOU_spending)) +
  theme_bw() +
  facet_wrap(~ tech_group) +
  geom_bar(stat = "identity") +
  theme(text = element_text(size=9.5),
        axis.text.x = element_text(angle=15, hjust=1)) + 
  ggtitle("TOU Spending for 3 Year Payback") +
  labs(x="Base Technology",y="Total Spending ($)") + 
  theme(plot.title = element_text(size= 26, hjust=0)) +
  theme(axis.title = element_text(size=18))
