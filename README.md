# NRDC-EE-model
Mohit Chhabra and Vivan Malkani Technical/Economic Potential Model

2017 CPUC PG Study for technology names
Data sources within PG Study:
California Lighting & Appl. Saturation Survey (CLASS), Residential baseline study of 1,987 homes across California, DNV GL 2012
California Energy Commission: Residential Appliance Saturation Survey 2009 Dataset for housing numbers and gas/electric WH density data

RASS density questions: 
Have a gas water heater (data collected by climate zone)
Have an electric gas water heater (data collected by climate zone)
Selection of “Single Family”, “Townhouse, Duplex, Row House” and “Apt Condo 2-4 Units”

Pierre HPWH data for HPWH performance by CZ, location
Using GE2014 model, only looking at households with 3 people  (avg household size for CA = 2.9)
Selecting GE2014 because most efficient non-CO2 powered HPWH

Weighting by location from NEEA:  
NEEA Residential Building Stock Assessment 2016/2017
Basement: Basement + Crawlspace
Garage: Garage + Other
Vented Closet: Main House

Weighting by size: <60 gallons and >60 gallons as proxies for 50 gallons and 80 gallons (sizes in Pierre’s dataset), CLASS 2012 data
Selection of “Single Family”, “Townhouse, Duplex, Row House” and “Apt Condo 2-4 Units”


HPWH consumption = 0.9 * Pierre data (efficiency improvements in past 5 years)
Calculating per unit kwh consumption of base measure by 1.1 * HPWH_consumption * HPWH_COP / Base_Tech_Efficiency_Factor 
1.1 because of difference in COP and UEF (actual vs. rated performance)
Technical Potential = per unit savings * installable measures
base_consumption_therms = 0.03412956 * base_consumption_kwh


A measure is defined as the combination of base technology, efficient technology, building type (SF/ MF/ Other), RET/ROB
Process for model saturation:
CZ 1 is average of other 15
Division of models into 
"Small Gas Storage Water Heater (0.48 EF - 0.559 EF)" =  0.48-0.519 EF : 0.52-0.559 EF,
2015 code: "Small Gas Storage Water Heater (0.56 EF - 0.599 EF)" = 0.56-0.599 EF
Post 2015 code: “Small Gas Storage Water Heater (0.60 EF - 0.639 EF)" = 0.60-0.639 EF
"Instantaneous Gas Water Heater (0.80 EF - 0.879 EF)" = 0.80-0.839 EF : 0.84-0.879 EF
From 2013 to 2018
1/EUL * [All categories] are retired every year
1/EUL * lowest_EF : half replaced by 2015_code, half replaced by post_2015_code  
1/EUL * 2015_code : replaced by post_2015_code  
1/EUL * post_2015_code: intra-category replacement
1/EUL * Instantaneous : intra-category replacement (NOT A MEASURE, THEREFORE NOT CONSIDERED)

From 2015 to 2018
Retired_Population = 1/EUL * [All below + code categories] are retired every year
post_2015_code: Retired_population + post_2015 code
Update dying populations
1/EUL * Instantaneous : intra-category replacement (NOT A MEASURE, THEREFORE NOT CONSIDERED)
PROJECTING FORWARD (THE MODEL)
From 2018 to 2030
Retrofit/ROB tag column
Competition group value (fractions of 100%, eg. 33% each for high, medium and low costs of HPWH, potentially divided by 2 if doing 50% ROB and 50% RET
Technical Applicability depends on each measure (base measure to HML efficient-ROB & HML-efficient-RET)
Stock turnover model is unique for each [base-efficient-HML-ROB-RET]-CZ
Install Limit for each base-efficient measure is technical applicability * Population of base measure in 2018 (could also * Competition group for specific group limit)
Install limit / EUL gives ROB installs per year
Eg:
CZ	tech		number		retire-2019
1	.48ef		1000		100
Install limit for HIGH cost HPWH= 1000 * .9 (tech applicability) * .333  RETROFIT 
Number of installs in 2020 is 300/EUL  ROB
Then look to measure table for savings for this base-efficient combination for technical potential for that base-efficient-CZ combo (divided into HML and ROB/RET)
 
INCORPORATE SINGLE/MULTI FAMILY: CHECK DENSITY, SATURATION, LOCATION AND SIZE TABLES



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

