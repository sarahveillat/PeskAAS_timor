# Peskaas - Timor
## GUIDE TO SCRIPTS

### Go through and run the scripts in this order:
1. Trips_script.R
2. Species_script.R
3. Boattype_script.R
4. gear_script.R
5. effort_script.R
6. PDS_script.R
7. landings_script.R
8. foodsale_script.R
9. catchvalue_script.R
10. Models.R
Extra: appendix_script.R

##

### Contents of scripts:

#### TRIPS
In this script:
   - setting directory
   - downloading data
   - add stratification
   - cleaning trip effort data
<br />

#### SPECIES
In this script:
   - adding functional groups
   - cleaning species and landing data
   - adding missing catch weight data
   - joining trips and landings df
   - making graphs for species and functional group composition (also in relation to gear type)
<br />

#### BOATS
In this script:
   - boat composition (canoe/motorboat)
   - boat composition over time (canoe/motorboat/shore-based)
   - length of different boat types
   - composition of motor types
   - composition of material of boats
   - number of fishers active on different boat types
   - duration of trip of different boat types
<br />

#### GEAR
In this script:
   - gear composition (strata)
   - gear use over time (strata + municipalities)
<br />

#### EFFORT AND CPUE
In this script:
   - trip effort over time (strata together and in grid)
   - seasonality of trip effort
   - trip duration and active fishers over time (separately)
   - calculating CPUE (standardise effort to 1 fisher and 1 trip hour)
   - CPUE over time
   - seasonality CPUE
<br />

#### PDS TRACKERS
In this script:
   - downloading data from pds trackers 
   - adding strata to pds df using boats df
   - plotting operation time
   - plotting number and percentage of boats being tracked over time
   - estimating and plotting number of TOTAL trips taken per week
   - plotting seasonality of number of trips taken per week
<br />

#### LANDINGS
In this script:
   - calculating VAC (average number of trips per month per boat)
   - calculating total monthly landings using slightly modified equation from peskaas paper. 
   - plotting total monthly landings per area (together and grids)
   - plotting seasonality of total monthly landings
   - calculating landings per capita per year
   - calculating average monthly landings per year
<br />

#### FOOD OR SALE
In this script:
   - plot of destination of the catch (kg) (food/sale/both) over time 
   - plot of catch used for own consumption over time
   - plot of destination of catch for different size groups
   - plot of destination of catch of different species and functional groups
<br />

#### MARKET VALUE OF CATCH
In this script:
   - calculate market value per kg catch
   - plot value per area over time
   - plot value per gear over time
   - calculate how many trips have catches which consist 95% or more of one functional group/species
   - plot value per functional group over time
   - plot value per species over time
<br />

#### MODELS TO TEST COVID19 EFFECT
In this script:
   - model with dependent variable "number of trips taken per week per boat"
   - model with dependent variable "total monthly landings"
   - model with dependent variable "market value per kg catch"
