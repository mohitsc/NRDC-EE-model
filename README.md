# NRDC-EE-model
Mohit Chhabra and Vivan Malkani Technical/Economic Potential Model

2017 CPUC PG Study for technology names  
Data sources within PG Study:  
California Lighting & Appl. Saturation Survey (CLASS), Residential baseline study of 1,987 homes across California, DNV GL 2012  
California Energy Commission: Residential Appliance Saturation Survey 2009 Dataset for housing numbers and gas/electric WH density data

--------------------------------------------------------------------------------------------------------------------------------------
Github system and planning to review the model:  
Enables version control  
Easy to share code   
Allows for collaboration, review and feedback  

--------------------------------------------------------------------------------------------------------------------------------------
Regional Population data from RASS 2009  
By Climate Zone: Single Family, Multi Family, Mobile Homes and Other  
 
RASS density questions:   
Have a gas water heater (data collected by climate zone)  
Have an electric gas water heater (data collected by climate zone)  
Selection of “Single Family”, “Townhouse, Duplex, Row House” and “Apt Condo 2-4 Units”  

--------------------------------------------------------------------------------------------------------------------------------------
Saturation data
CLASS 2012: Efficiency Factor distribution of each CZ
Selection of 02. Energy Factor - Bins , Report Year: 2012 , Weighting Scheme: Census Weights, Group By: [Climate Zone] , Filters: [Type of Residence] IN ('01 - Single Family Detached','02 - Apt 2-4 Units','06 - Townhouse/Rowhouse (2-4 Unit Multi-Story)') AND [Water Heater Fuel Type] IN ('Electric') AND [Size of Water Heater] IN ('40-49 Gallons','50-59 Gallons')

For Electric: CZ 4 and 8 and 15 are average of the rest

--------------------------------------------------------------------------------------------------------------------------------------

Pierre HPWH data for HPWH performance by CZ, location  
Using GE2014 model, only looking at households with 3 people  (avg household size for CA = 2.9)  
Selecting GE2014 because most efficient non-CO2 powered HPWH  

HPWH consumption = 0.9 * Pierre data (efficiency improvements in past 5 years)  
Calculating per unit kwh consumption of base measure by 1.1 * HPWH_consumption * HPWH_COP / Base_Tech_Efficiency_Factor   
1.1 because of difference in COP and UEF (actual vs. rated performance)      
Technical Potential = per unit savings * installable measures    
base_consumption_therms = 0.03412956 * base_consumption_kwh  

Weighting by location from NEEA:    
NEEA Residential Building Stock Assessment 2016/2017  
Basement: Basement + Crawlspace  
Garage: Garage + Other  
Vented Closet: Main House  

Weighting by size: <60 gallons and >60 gallons as proxies for 50 gallons and 80 gallons (sizes in Pierre’s dataset), CLASS 2012 data  
Selection of “Single Family”, “Townhouse, Duplex, Row House” and “Apt Condo 2-4 Units”  

--------------------------------------------------------------------------------------------------------------------------------------
A measure is defined as the combination of base technology, efficient technology, building type (SF/ MF/ Other), RET/ROB  

Gas WH Code UEF 2015: 0.60
Elec WH Code UEF 2015: .945

Input to start stock turnover model:   
[1/EUL * [All efficiency categories]] are retired every year   
[1/EUL * lowest_EF]: half replaced by 2015_code, half replaced by post_2015_code  
[1/EUL * 2015_code]: replaced by post_2015_code  
[1/EUL * post_2015_code]: intra-category replacement   

Start year to end of projected period stock turnover model:  
Stock turnover model is independent for each measure  
Total Install Limit for each measure is [technical applicability * population applicability * ROB_RET_ratio * population of base
measure in 2018]  
Install limit = RET installs in first year  
[Install limit/ EUL] gives ROB installs per year  

Eg:  
CZ	tech		number		retire-2019  
1	.48ef		1000		100  
Install limit for HIGH cost HPWH= 1000 * .9 (tech applicability) * .333 (RETROFIT)     
Number of installs in 2020 is 300/EUL (ROB)    
Then look to measure table for savings for this base-efficient combination for technical potential for that base-efficient-CZ combo (divided into HML and ROB/RET)  

--------------------------------------------------------------------------------------------------------------------------------------
Per Unit Savings:  
ROB per unit savings = [code technology consumption – efficient technology consumption]  
RET per unit savings = [base technology consumption – efficient technology consumption] for RUL period 1 and then [code technology consumption – efficient technology consumption] for period 2  

--------------------------------------------------------------------------------------------------------------------------------------
Technical potential estimates:  
 Potential installs for each year  
 First year savings in kWh = per unit savings * installs that year  
 First year savings in therms = per unit savings * installs that year  
 Lifetime savings for each measure  

--------------------------------------------------------------------------------------------------------------------------------------
Adding new measures:  
Saturation table  
Density table  
Consumption table  
Regional Population table (if different from CA Title 24 climate zones)  
EUL  

--------------------------------------------------------------------------------------------------------------------------------------
 Payback Model
Merrian: referred to in Synapse Study
 "The most comparable model I found was a Rheem 50-gallon tank with three times the efficiency of a standard water heater priced at $1,199 from Home Depot. I polled several utilities, contractors, and energy efficiency program implementers in California and the Pacific Northwest, and the average cost they gave was $2000 to $2600 for equipment, installation, and mark-up"
https://www.nrdc.org/experts/merrian-borgeson/electric-home-study-biased-shows-ca-wants-clean-energy  


Using IEPR 2017 forecast tables for gas and electricity prices of each utility, 2016 to 2030 estimated growth rates: http://www.energy.ca.gov/2017_energypolicy/documents/2018-02-21_business_meeting/2018-02-21_middemandcase_forecst.php  

growth rate for gas estimated by using 2018 to 2030 estimation : future = present (1 + r)^n  

check excel file "climate_zone_rate_mapping"  

mapping utilities to climate zones:
http://www.energy.ca.gov/maps/renewable/BuildingClimateZonesMap.pdf  
https://www.energy.ca.gov/maps/serviceareas/natural_gas_service_areas.pdf  
https://www.energy.ca.gov/maps/serviceareas/CA_Electric_Investor_Owned_Utilities_IOUs.pdf  
https://www.energy.ca.gov/maps/serviceareas/Electric_Utility_Service_Areas.pdf  

TOU rates from Pierre Ecotope Data for climate zones 1:5, 11:13
Using CZ 5 data for CZ 6:10, 14:15,
CZ 1 data for CZ 16
Approximation uses heating degree days ranges from PGE   https://www.pge.com/includes/docs/pdfs/about/edusafety/training/pec/toolbox/arch/climate/california_climate_zones_01-16.pdf

Loadshape data from Pierre HPWH Flexibility Study data for all 16 climate zones  

GHG Marginal Emissions from CPUC Avoided Cost Calculator 2018 prepared by E3  
marginal_emissions.xlsx doc in Input_to_Input_Tables  
8760 hours for 2018-2030 avg. grid tXO2 per MWh  
For natural gas: Natural Gas Carbon Content (tons/therm): 0.00585  

avg. carbon intensity (over both loadshape labels) for kwh



--------------------------------------------------------------------------------------------------------------------------------------

Appliance Efficiency Appendix
Water heaters CEC minimum UEF requirement
http://www.energy.ca.gov/title24/2016standards/documents/2016_water_heater_efficiency_guide.pdf

CEC 2014 Appliance Efficiency Regulations
http://www.energy.ca.gov/2014publications/CEC-400-2014-009/CEC-400-2014-009-CMF.pdf
 

CEC 2005, 2008
http://www.energy.ca.gov/2008publications/CEC-400-2008-016/rev1_chapters/RCM_Chapter_5_WH.pdf
 
CEC 2001
http://www.energy.ca.gov/title24/archive/2001standards/2001-10-04_400-01-024.PDF

Source of 1999-2003 and 2004-2009
https://www.eia.gov/analysis/studies/residential/pdf/appendix-a.pdf

