#start fresh
rm(list=ls()) #clear environment
if(!is.null(dev.list())) dev.off() #clear plots
cat("\014") #clear console

#TRIPS
#IN THIS SCRIPT:
#   - setting directory
#   - downloading data
#   - add stratification
#   - cleaning trip effort data


library(RMySQL)
library(DBI)
library(tidyverse)
library(lubridate)

##Setting directory

setwd("~/Documents/Documents – Sarah's MacBook Pro/Studies/Master UiB/Master Marine Biology UiB/Master Thesis/R-scripts/R-project")


#download the datasets from peskaas
peskaDAT = RMySQL::dbConnect(RMySQL::MySQL(), user="wildrlab_guest", password="aGD9h8zt74pEMzk", dbname='wildrlab_peskaDB', host='johnny.heliohost.org', port=3306)
RMySQL::dbListTables(peskaDAT)
stns <- RMySQL::dbReadTable(peskaDAT, "stations")
trips <- RMySQL::dbReadTable(peskaDAT, "trips")
landings <- RMySQL::dbReadTable(peskaDAT, "landings")
habitat <- RMySQL::dbReadTable(peskaDAT, "habitat_types")
gear <- RMySQL::dbReadTable(peskaDAT, "gear_types")
boats <- RMySQL::dbReadTable(peskaDAT, "boats")
boat_types <- RMySQL::dbReadTable(peskaDAT, "boat_types")
species <- RMySQL::dbReadTable(peskaDAT, "species")
flags <- RMySQL::dbReadTable(peskaDAT, "flags")
rank <- RMySQL::dbReadTable(peskaDAT, "rankings")
VAC <- RMySQL::dbReadTable(peskaDAT, "VACs")
municipalities <- RMySQL::dbReadTable(peskaDAT, "municipalities")
habitat_types <- RMySQL::dbReadTable(peskaDAT, "habitat_types")

RMySQL::dbDisconnect(peskaDAT)

## ADD STRATA

#Cluster the stations to 3 strata:
#1 --> Autora island
#2 --> North coast
#3 --> South coast

#1.	Atauro island 
#   Municipalities: Atauro (12)
#2.	North coast
#   Municipalities: Bacau (9), Lautem (2), Bobonaro (5), Liquica (4), Oe-Cusse (11), Manatutu (3), Dili
#3.	South coast
#   Municipalities: Viqueque (1), Covalima (6), Manufahi (7), Ainaro (8)

stns <- stns %>% 
  mutate(strata = case_when(
    municipality_code %in%  c(12) ~ "Atauro island",
    municipality_code %in% c(2, 3, 4, 5, 9, 11) ~ "North coast",
    municipality_code %in% c(1, 6, 7, 8) ~ "South coast"))


#join trips dataset with the info about the stations
trips_stns <- left_join(trips, stns, by = "station_code") %>% 
  dplyr::rename(municipality = municipality_name) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(year = factor(year(date)),
         trip_id = factor(trip_id),
         habitat_code = factor(habitat_code),
         boat_code = factor(boat_code),
         gear_code = factor(gear_code)) 


## CLEANING TRIP EFFORT DATA

#change the 1922 date to 2018-12-07, as the tripID matches with trips from that exact date. 
#TRIP DURATION:
#   - change trip durations of above 13 hours to median trip duration of area + boat type
#   - make negative trip hours positive (assuming that is the mistake..)
#   - make 0 hour trips 1 hour (assuming they rounded down the number)
#NUMBER OF FISHERS (relative effort):
#   - change relative effort (number of fishers active) of above 10 to median of area + boat type
#   - make 0 fishers into 1 fisher (assumption)
#CATCH VALUE:
#   - round down catch value of above 2000 on a trip to 2000 (decided together with Jeppe, only affects 3 trips with 1 huge outliers)

####       --> check appendix_script.R for the histograms and number of trips altered


trps_stns <- trips_stns %>%
  mutate(date = replace(date, date == "1922-11-04", "2018-12-07")) %>% 
  mutate(trip_hours = abs(trip_hours), 
         trip_hours = replace(trip_hours, trip_hours <= 0, 1), 
         catch_value = replace(catch_value, catch_value > 2000, NA_real_),
         rel_effort = replace(rel_effort, rel_effort <= 0, 1)) %>% 
  mutate(trip_hours = if_else(boat_code == 1 & strata == "South coast" & 
                                trip_hours >= 14, 4, trip_hours),
         trip_hours = if_else(boat_code == 1 &strata != "South coast" & 
                                trip_hours >= 14, 2, trip_hours),
         trip_hours = if_else(boat_code == 2 & strata == "Atauro Island" & 
                                trip_hours >= 14, 3, trip_hours),
         trip_hours = if_else(boat_code == 2 &strata != "Atauro Island" & 
                                trip_hours >= 14, 5, trip_hours),
         trip_hours = if_else(boat_code == 3 & strata == "North coast" & 
                                trip_hours >= 14, 3, trip_hours),
         trip_hours = if_else(boat_code == 3 &strata != "North coast" & 
                                trip_hours >= 14, 1, trip_hours),
         rel_effort = if_else(boat_code == 1 & strata == "Atauro Island" & 
                                rel_effort > 10, 2, rel_effort),
         rel_effort = if_else(boat_code == 1 & strata != "Atauro Island" & 
                                rel_effort > 10, 1, rel_effort),
         rel_effort = if_else(boat_code == 2 & strata == "Atauro Island" & 
                                rel_effort > 10, 3, rel_effort),
         rel_effort = if_else(boat_code == 2 & strata != "Atauro Island" & 
                                rel_effort > 10, 2, rel_effort)) %>% 
  mutate(trip_effort2 = trip_hours * rel_effort, .before = trip_effort) #make own trip effort (fisher-hours)


