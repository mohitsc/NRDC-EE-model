# NRDC-EE-model
Mohit Chhabra and Vivan Malkani Technical/Economic Potential Model

2017 CPUC PG Study for technology names
Data sources within PG Study:
California Lighting & Appl. Saturation Survey (CLASS), Residential baseline study of 1,987 homes across California, DNV GL 2012
California Energy Commission: Residential Appliance Saturation Survey 2009 Dataset for housing numbers and gas/electric WH density data

--------------------------------------------------------------------------------------------------------------------------------------
Regional Population data from RASS 2009
By Climate Zone: Single Family, Multi Family, Mobile Homes and Other



RASS density questions: 
Have a gas water heater (data collected by climate zone)
Have an electric gas water heater (data collected by climate zone)
Selection of “Single Family”, “Townhouse, Duplex, Row House” and “Apt Condo 2-4 Units”
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
•	A measure is defined as the combination of base technology, efficient technology, building type (SF/ MF/ Other), RET/ROB

•	Input to start : 
o	[1/EUL * [All efficiency categories]] are retired every year 
o	[1/EUL * lowest_EF]: half replaced by 2015_code, half replaced by post_2015_code
o	[1/EUL * 2015_code]: replaced by post_2015_code
o	[1/EUL * post_2015_code]: intra-category replacement 
•	Start year to end of projected period:
o	Stock turnover model is independent for each measure
o	Total Install Limit for each measure is [technical applicability * population applicability * ROB_RET_ratio * population of base measure in 2018]
o	Install limit = RET installs in first year
o	[Install limit/ EUL] gives ROB installs per year

•	Per Unit Savings:
o	ROB per unit savings = [code technology consumption – efficient technology consumption]
o	RET per unit savings = [base technology consumption – efficient technology consumption] for RUL period 1 and then [code technology consumption – efficient technology consumption] for period 2

•	Technical potential estimates
o	First year savings in kWh
o	First year savings in therms
o	Potential installs for each year
o	Lifetime savings for each measure

•	Adding new measures
o	Saturation table
o	Density table
o	Consumption table
o	Regional Population table (if different from CA Title 24 climate zones)
o	EUL

•	Github system and planning to review the model
o	Enables version control
o	Easy to share code 
o	Allows for collaboration, review and feedback

Eg:
CZ	tech		number		retire-2019
1	.48ef		1000		100
Install limit for HIGH cost HPWH= 1000 * .9 (tech applicability) * .333  RETROFIT 
Number of installs in 2020 is 300/EUL  ROB
Then look to measure table for savings for this base-efficient combination for technical potential for that base-efficient-CZ combo (divided into HML and ROB/RET)
 
INCORPORATE SINGLE/MULTI FAMILY: CHECK DENSITY, SATURATION, LOCATION AND SIZE TABLES
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

