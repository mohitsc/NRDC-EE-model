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

# Technology Lifetime Costs
tech_costs <- tbl_df(read_excel("Potential_Model_Input_Tables/tech_costs_table.xlsx", sheet = "R_input"))

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
         savings_over_code_therms) %>% arrange(measure, climate_zone) %>% distinct()


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
                                    "building_type")) %>% arrange(measure, climate_zone)

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
  arrange(base_tech_name, efficient_tech_name, climate_zone)

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
  arrange(base_tech_name, efficient_tech_name, climate_zone)

write.xlsx(as.data.frame(annual_technical_potential_therms), 
           "Potential_Model_Output_Tables/tech_potential_therms_savings.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

# Lifetime savings table by making list of lifetime savings dataframes
cumulative_lifetime_kwh_savings <- select(installs_per_year,
                                      base_tech_name:delivery_type,
                                      EUL)
for(year in (current_year+1):project_until){
  cumulative_lifetime_kwh_savings <- bind_cols(cumulative_lifetime_kwh_savings,
                                                          temp = rep.int(0, times = nrow(cumulative_lifetime_kwh_savings)))
  names(cumulative_lifetime_kwh_savings)[names(cumulative_lifetime_kwh_savings) == "temp"] <- paste0("savings_kwh_",
                                                                                             year)
}


lifetime_kwh_calculations <- list()

for(installs_year in (current_year+1):project_until){
  
  lifetime_kwh_calculations[[installs_year]] <- select(installs_per_year,
                                                   base_tech_name:delivery_type,
                                                   EUL)
  
  installs_column <- select_at(installs_per_year, .vars = vars(contains(as.character(installs_year))))
  
  for(savings_year in installs_year:project_until){
    savings_column <- over_base_kwh_savings(installs_column)
    names(savings_column) <- paste0("savings_kwh_", savings_year)
    
    lifetime_kwh_calculations[[installs_year]] <- bind_cols(lifetime_kwh_calculations[[installs_year]], 
                                                            savings_column)
    cumulative_adder <- function(column) {
      return (column + savings_column[,1]) 
    }
    
    cumulative_lifetime_kwh_savings <- mutate_at(cumulative_lifetime_kwh_savings, 
                                                 .vars = vars(contains(as.character(savings_year))), 
                                                 cumulative_adder)
  }
  
}
#ADAPT THIS TO LIFETIME SAVINGS
# #adding pre and post RUL cost columns
# 
# first_year_costs_RET_pre_RUL <- merge(select(RET_subset, 
#                                              base_tech_name, 
#                                              efficient_tech_name, 
#                                              climate_zone, 
#                                              delivery_type, 
#                                              building_type,
#                                              vars_select(names(installs_per_year), contains("installs"))),
#                                       first_year_costs_RET_pre_RUL,
#                                       by = c("base_tech_name", 
#                                              "efficient_tech_name", 
#                                              "delivery_type", 
#                                              "building_type")) %>%
#   rename("pre_RUL_costs" = costs)
# 
# first_year_costs_RET <- merge(first_year_costs_RET_pre_RUL,
#                               first_year_costs_RET_post_RUL,
#                               by = c("base_tech_name",
#                                      "code_tech_name",
#                                      "efficient_tech_name", 
#                                      "delivery_type", 
#                                      "building_type")) %>%
#   rename("post_RUL_costs" = costs)
# 
# rm(first_year_costs_RET_post_RUL, first_year_costs_RET_pre_RUL)
# 
# 
# first_year_costs_RET <- mutate_at(first_year_costs_RET, 
#                                   vars(matches("installs_[2019:2022]")), 
#                                   funs(. * pre_RUL_costs))
# first_year_costs_RET <- mutate_at(first_year_costs_RET, 
#                                   vars(matches("installs_[2023:2030]")), 
#                                   funs(. * post_RUL_costs))
# 
# first_year_costs_RET <- rename_at(first_year_costs_RET,
#                                   vars(contains("installs_")), 
#                                   funs(paste0("costs_", parse_number(.)))) %>%
#   rename("cumulative_costs" = cumulative_installs)


# incremental first year costs -------------------------------------------------------------------------------
# RET costs pre RUL = efficient costs
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

write.xlsx(as.data.frame(first_year_costs), 
           "Potential_Model_Output_Tables/first_year_costs.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")

#Total first year costs including installs
RET_subset <- filter(technical_potential, delivery_type == "RET")
ROB_subset <- filter(technical_potential, delivery_type == "ROB")

#adding ROB costs column
total_first_year_costs_ROB <- merge(select(ROB_subset, 
                                       base_tech_name, 
                                       efficient_tech_name, 
                                       climate_zone, 
                                       delivery_type, 
                                       building_type,
                                       vars_select(names(installs_per_year), contains("installs"))),
                                first_year_costs_ROB,
                                by = c("base_tech_name", 
                                       "efficient_tech_name", 
                                       "delivery_type", 
                                       "building_type"))
                                
total_first_year_costs_ROB <- mutate_at(total_first_year_costs_ROB, 
                                    vars(contains("installs")), 
                                    funs(. * incremental_first_year_cost)) 

total_first_year_costs_ROB <- rename_at(total_first_year_costs_ROB,
                                    vars(contains("installs_")), 
                                    funs(paste0("costs_", parse_number(.)))) %>%
  rename("cumulative_costs" = cumulative_installs)

total_first_year_costs <- total_first_year_costs %>% 
  mutate(cumulative_costs = rowSums(select(., contains("costs_"))))


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


#YEARWISE OPR COST SAVINGS

for(year in (current_year+1):project_until){
  
  operational_cost_savings <- operational_cost_savings %>% mutate()
  
  operational_cost_savings <- bind_cols(operational_cost_savings, 
                                 temp = ifelse(operational_cost_savings$delivery_type == "ROB",
                                               operational_cost_savings$code_opr_costs_non_TOU - operational_cost_savings$efficient_opr_costs_non_TOU,
                                               ifelse(year<(current_year + (operational_cost_savings$base_EUL/3)), 
                                                      operational_cost_savings$base_opr_costs_non_TOU - operational_cost_savings$efficient_opr_costs_non_TOU,
                                                      operational_cost_savings$code_opr_costs_non_TOU - operational_cost_savings$efficient_opr_costs_non_TOU)
                                               ))
  
  #applying discount rate of base_year_savings/(1+discount_rate)^(year- current_year)
  operational_cost_savings <- operational_cost_savings %>% 
    mutate(temp = temp / (1 + discount_rate)^(year - current_year))
  names(operational_cost_savings)[names(operational_cost_savings) == "temp"] <- paste0("non_TOU_savings_", year)
  
  operational_cost_savings <- bind_cols(operational_cost_savings, 
                                        temp = ifelse(operational_cost_savings$delivery_type == "ROB",
                                                      operational_cost_savings$code_opr_costs_TOU - operational_cost_savings$efficient_opr_costs_TOU,
                                                      ifelse(year<(current_year + (operational_cost_savings$base_EUL/3)), 
                                                             operational_cost_savings$base_opr_costs_TOU - operational_cost_savings$efficient_opr_costs_TOU,
                                                             operational_cost_savings$code_opr_costs_TOU - operational_cost_savings$efficient_opr_costs_TOU)
                                        ))
  operational_cost_savings <- operational_cost_savings %>% 
    mutate(temp = temp / (1 + discount_rate)^(year - current_year))
  names(operational_cost_savings)[names(operational_cost_savings) == "temp"] <- paste0("TOU_savings_", year)
}





write.xlsx(as.data.frame(operational_cost_savings), 
           "Potential_Model_Output_Tables/operational_costs_savings.xlsx", 
           row.names = FALSE,
           sheetName = "R_output")






