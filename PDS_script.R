#RUN TRIPS_SCRIPT.R FOR DIRECTORY AND PACKAGES

##PDS TRACKERS
#IN THIS SCRIPT
#   - downloading data from pds trackers 
#   - adding strata to pds df using boats df
#   - plotting operation time
#   - plotting number and percentage of boats being tracked over time
#   - estimating and plotting number of TOTAL trips taken per week
#   - plotting seasonality of number of trips taken per week


library(readr)

#data received from Fer
PDS <- read_csv("Data used/pds_trips.csv") %>% 
  rename(IMEI = imei, 
         boat_id = boat_id_pds) %>% 
  mutate(IMEI = as.factor(IMEI))

#Problem, PDS df has no info that gives me an option to allocate strata. However, it does have IMEI.
#the boats df has IMEI, however a lot of IMEI in PDS df do not have a match with IMEI in boats df.
#add other boats df downloaded from peskaas interface:
boats_CSV <- read_csv2("Data used/boats_CSV.csv")

#use both these dfs to add strata to PDS df:
boats_CSV <- boats_CSV %>%
  mutate(Region = as.character(Region)) %>% 
  mutate(strata = case_when(
    Region %in% c("Atauro") ~ "Atauro island",
    Region %in% c("Baucau", "Dili", "Lautem", "Liquica", "Manatuto", "Oe-Cusse", "Bobonaro") ~ "North coast",
    Region %in% c("--") ~ NA_character_,
    TRUE ~ "South coast")) #check .before or command relocate

boats <- boats %>% #not necessary to do this if already run boattype_script.R
  mutate(strata = case_when(
    municipality_name %in% c("Atauro") ~ "Atauro island",
    municipality_name %in% c("Baucau", "Lautem", "Bobonaro", "Liquica", "Oe-Cusse", "Manatuto", "Dili") ~ "North coast",
    municipality_name %in% c("Viqueque", "Covalima","Manufahi", "Ainaro") ~ "South coast")) %>% 
  mutate(boat_description = case_when(
    str_detect(boat_description, "wood") ~ "wood",
    str_detect(boat_description, "fiberglass")  ~ "fiberglass",
    TRUE ~ NA_character_))

#first join pds df with boats_csv
PDS <- left_join(PDS, boats_CSV, by = "IMEI") %>% 
  dplyr::select(!c("Boat", "Captain", "Battery", "Last Seen", "Entity", "Community", 
            "Gross Tonnage", "Device Owner", "Registration Authority", "Contact Name", "Owner Name", 
            "Gear Type", "Registration Number", "Registration Expiration", "Contract expiration", 
            "Flag", "Contact Phone"))

#then join that one with boats df as well, and add strata if it was missing after the first merging
PDS2 <- left_join(PDS, boats, by = "IMEI") %>% 
  dplyr::select(!c("motor_make", "motor_HP", "VTS_location", "VTS_covered_when_parked",
            "installation_date", "primary_gear", "secondary_gear", "network_provider",
            "boat_reg_no", "notes")) %>% 
  mutate(strata = if_else(is.na(strata.x), true = strata.y, false = strata.x)) %>% 
  rename(boat_id = boat_id.x)

length(which(is.na(PDS2$strata))) #only 1326 have no strata


#plotting operation time of PDS trackers, using  script from Fer modified to strata
PDS2 %>% 
  filter(!is.na(strata)) %>% 
group_by(IMEI, boat_id, strata) %>%
  summarise(installation = min(trip_start_date_pds), 
            last_heard = max(trip_end_date_pds), .groups = "drop") %>%
  mutate(boat_id = as.factor(boat_id),
         boat_id = fct_reorder(boat_id, installation, .fun = min, .desc = TRUE)) %>%
  ggplot(aes(y = boat_id, colour = strata)) +
  geom_segment(aes(x = installation, xend = last_heard, yend = boat_id)) +
  facet_grid(strata ~ ., scales = "free", space = "free", margins = FALSE) +
  scale_color_brewer(palette = "Paired", name = "Strata") +
  theme_light() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  theme(axis.text.y = element_blank(), 
        panel.grid.major.y = element_blank(), 
        legend.position = "none", 
        #strip.text.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(title = " ", 
       colour = " ",
       y = "Individual boat")

ggsave(path = "figures/Samples", filename = "operationtime_PDS.png", width = 18, height = 12, units = "cm")



#calculate the percentage of total boats being tracked

#making df of total boats in strata
total_boats <- municipalities %>% 
  mutate(strata = case_when(
    municipality_name %in% c("Atauro") ~ "Atauro island",
    municipality_name %in% c("Baucau", "Lautem", "Bobonaro", "Liquica", "Oe-Cusse", "Manatuto", "Dili") ~ "North coast",
    municipality_name %in% c("Viqueque", "Covalima","Manufahi", "Ainaro") ~ "South coast")) %>% 
  group_by(strata) %>% 
  summarise(canoes = sum(canoes),
            motors = sum(motors),
            PDS_units = sum(total_PDS_units)) %>% 
  mutate(boats = canoes + motors) %>% 
  mutate(simple_tracked = (PDS_units/boats) * 100)


#making a plot of number of boats being tracked over time
#first making df with start and end date of unique IMEI
Unique_IMEI <- PDS2 %>% 
  group_by(IMEI, strata) %>% 
  summarize(startdate = min(trip_start_date_pds), enddate = max(trip_start_date_pds), n = n(), n_dis = n_distinct(start)) %>% 
  # arrange(enddate) %>% 
  mutate(IMEI_id = row_number())

#make a df with all the dates
all_dates <- tibble(date = seq(min(Unique_IMEI$startdate), max(Unique_IMEI$enddate), 1))

#reframing df
Unique_IMEI_2 <- Unique_IMEI %>% 
  pivot_longer(cols = ends_with("date"),
               names_to = "start_end",values_to = "date") %>% 
  mutate(start_end = str_remove_all(start_end, "date"))

#making a df which shows how many boats were tracked every day. 
boats_tracked <- Unique_IMEI %>% 
  crossing(all_dates) %>% 
  group_by(strata) %>% 
  filter(date >= startdate, date <= enddate,
         !is.na(strata)) %>% 
  count(date) 

#plot this
ggplot(boats_tracked, aes(x= date, y = n, fill = strata)) + geom_area() +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y %b", guide = guide_axis(angle = 70))+
  #geom_vline(xintercept = as.numeric(as.Date("2020-03-28")), linetype=4, col = "blue")+
  labs(title = "Number of boats tracked", x = "", y = "Count", fill = "Strata") +
  theme_light() + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Samples", filename = "No_PDSactive.png", width = 18, height = 11, units = "cm")

#make plot of the percentage of boats being tracked over time
boats_tracked2 <- left_join(boats_tracked, total_boats, by = "strata") %>% 
  mutate(per_tracked = (n/boats) * 100) %>% 
  filter(!is.na(strata))

ggplot(boats_tracked2, aes(x= date, y = per_tracked, col = strata)) + geom_line() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", guide = guide_axis(angle = 70)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  #geom_vline(xintercept = as.numeric(as.Date("2020-03-28")), linetype=4, col = "blue")+
  labs(title = "", x = "", y = "Percentage boats tracked", col = "Strata") +
  theme_light() + scale_color_brewer(palette = "Paired") + theme(legend.position = "right")


ggsave(path = "figures/Samples", filename = "percentage_PDSactive.png", width = 18, height = 10, units = "cm")


#using the percentage of boats being tracked to estimate the TOTAL number of trips being taken per week
boats_tracked2_week <- boats_tracked2 %>% 
  filter(!is.na(strata)) %>% 
  group_by(week = floor_date(date, "1 week"), strata) %>% 
  summarise(per_tracked = mean(per_tracked, na.rm = TRUE))

PDS_trips <- PDS2 %>% 
  filter(!is.na(strata)) %>% 
  mutate(trip_start_date_pds = ymd(trip_start_date_pds)) %>%
  group_by(week = floor_date(trip_start_date_pds, "1 week"), strata) %>% 
  summarize(n = n())

PDS_trips2 <- left_join(PDS_trips, boats_tracked2_week, by = c("week" = "week", "strata" = "strata")) %>% 
  mutate(total_trips = (n*100/per_tracked)) %>%
  mutate(lockdown_stage = case_when(
    week >= "2020-03-28" ~ 1,
    TRUE ~ 0)) %>% 
  mutate(year = format(week, "%Y"),
         month = format(week, "%b"))

ggplot(PDS_trips2, aes(x=week, y=total_trips, col = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) +
  geom_smooth(aes(x = week, y = total_trips, group = strata, fill = strata), method = "lm", col ="black", linetype = "twodash", size = .6) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  theme_light() + scale_x_date(date_breaks = "3 months", 
                               date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "No. trips per week", col = "Strata") +
  theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Paired") + scale_fill_brewer(palette = "Paired") + guides(fill = FALSE)

ggsave(path = "figures/PDS", filename = "Tripsperweek_PDS.png", width = 18, height = 11, units = "cm")


#show seasonality of number of trips taken per week
PDS_trips2 <- PDS_trips2 %>% 
  mutate(month = month(week),
         month_name = month(ymd(week), label = TRUE, abbr = TRUE)) %>% 
  mutate(season = case_when(
    month(ymd(week)) >= "12" & month(ymd(week)) <= "4" ~ "Wet season",
    TRUE ~ "Dry season"),
    lockdown_stage = case_when(
      week >= "2020-03-28" & week <= "2020-06-25" ~ 1,
      week >= "2020-06-26" & week <= "2020-08-05" ~ 2,
      week >= "2020-08-06" ~ 3,
      TRUE ~ 0))

ggplot(PDS_trips2) +
  geom_boxplot(aes(x = month_name, y = total_trips, group = month)) +
  geom_smooth(aes(x = as.numeric(month_name), y = total_trips), method = "loess", linetype = "twodash", size = .6) +
  facet_wrap(~strata, scale = "free_y") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_light() + labs(y = "No. of trips per week", x = "Month")

ggsave(path = "figures/PDS", filename = "Tripsperweek_season.png", width = 18, height = 11, units = "cm")

