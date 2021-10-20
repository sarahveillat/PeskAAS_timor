#DATA FROM TRIPS_SCRIPT.R AND SPECIES_SCRIPT.R AND PDS_SCRIPT.R (need to run those before starting this one)

#LANDINGS
#IN THIS SCRIPT
#   - calculating VAC (average number of trips per month per boat)
#   - calculating total monthly landings using slightly modified equation from peskaas paper. 
#   - plotting total monthly landings per area (together and grids)
#   - plotting seasonality of total monthly landings
#   - calculating landings per capita per year
#   - calculating average monthly landings per year


#total landings equation:
#Estimated total monthly catch = sum(CPUE * EPT * VAC * N * 0.001) (per boat type)
#CPUE = catch (kg) per fisher-hours (1 fisherman per 1 hour)
#EPT = effort per trip (fisher-hours)
#VAC =  vessel activity coefficient (average number of trips per month per boat)
# N = total number of boats


#calculate VAC
#includes N (boats)
VAC <- boats_tracked2 %>% 
  filter(!is.na(strata)) %>% 
  group_by(month = floor_date(date, "1 month"), strata, boats) %>% 
  mutate(per_tracked2 = mean(per_tracked, na.rm = TRUE))
  
VAC2 <- PDS2 %>% 
  filter(!is.na(strata)) %>% 
  mutate(trip_start_date_pds = ymd(trip_start_date_pds)) %>%
  group_by(month = floor_date(trip_start_date_pds, "1 month"), strata) %>% 
  mutate(n = n())

VAC3 <- left_join(VAC2, VAC, by = c("month" = "month", "strata" = "strata")) %>% 
  dplyr::select("date", "strata", "per_tracked2", "n.x", "boats") %>% 
  mutate(total_trips = (n.x*100/per_tracked2)) %>% 
  mutate(VAC = total_trips/boats)

#CPUE and EPT
library(plotrix)
CPUE_landings <- all_data %>%
  filter(date > "2018-02-06") %>% 
  group_by(trip_id, date, strata, municipality, boat_code, trip_hours, rel_effort, trip_effort2) %>% 
  summarize(catch_weight = sum(weight_calc, na.rm = TRUE)/1000) %>%
  mutate(CPUE_weight =  catch_weight * (1/trip_hours) * (1/rel_effort)) %>% 
  group_by(month = floor_date(date, "1 month"), strata) %>% #add boat_code here if seperated by boat type
  summarize(monthly_CPUE = sum(CPUE_weight, na.rm = TRUE),
            ntrips = n(),
            median_effort = median(trip_effort2, na.rm = TRUE),
            se_effort = std.error(trip_effort2, na.rm = TRUE)) %>% 
  mutate(CPUE = monthly_CPUE/ntrips)
  

#add VAC to df
monthly_landings <- left_join(CPUE_landings, VAC3, by = c("month" = "month", "strata" = "strata")) %>% 
  distinct(CPUE, .keep_all = TRUE) 

#Estimated total monthly catch = CPUE * EPT * VAC * N * 0.001 

monthly_landings <- monthly_landings %>% 
  filter(!is.na(VAC)) %>% 
  mutate(catch_tons = (CPUE * median_effort * VAC * boats * 0.001)) %>%
  dplyr::select(!"date") %>% 
  rename(date = month)

#plotting monthly landings per area
ggplot(monthly_landings, aes(x=date, y=catch_tons, col = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  theme_light() + scale_x_date(date_breaks = "3 months", 
                               date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Catch (tons) per month", col = "Strata") + theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Paired") + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Landings", filename = "Total_landings.png", width = 18, height = 11, units = "cm")

#and gridded
ggplot(monthly_landings, aes(x=date, y=catch_tons, col = strata)) +
  facet_wrap(~strata, scales = "free_y") +
  #facet_grid(rows = vars(strata), scales = "free_y") +
  geom_line(size=.5, position = position_dodge(0.1)) +
  geom_smooth(aes(x = date, y = catch_tons), method = "lm", col ="black", linetype = "twodash", size = .6) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  theme_light() + scale_x_date(date_breaks = "3 months", 
                               date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Catch (tons) per month", col = "Strata") + theme(axis.title.x = element_blank(), legend.position = "none") +
  scale_color_brewer(palette = "Paired")

ggsave(path = "figures/Landings", filename = "Total_landings_facet.png", width = 18, height = 8, units = "cm")


#show seasonality of monthly landings
monthly_landings <- monthly_landings %>% 
  mutate(month = month(date),
         month_name = month((date), label = TRUE, abbr = TRUE))

ggplot(monthly_landings) +
  theme_light() + 
  geom_boxplot(aes(x = month_name, y = catch_tons, group = month)) +
  geom_smooth(aes(x = as.numeric(month_name), y = catch_tons), 
              method = "loess", linetype = "twodash", size = .6) +
  facet_wrap(~strata, scale = "free_y") +
  scale_x_discrete(guide = guide_axis(angle = 45)) + theme(axis.title.x = element_blank()) +
  labs(y = "Catch (tons) per month")

ggsave(path = "figures/landings", filename = "Landings_season.png", width = 18, height = 11, units = "cm")
  

#calcula per capita catch per year
#population number Timor per year:
#2018 --> 1,267,797 
#2019 --> 1,293,120
#2020 --> 1,318,442
#using same as 2020 for 2021 as there is no information available yet. 

landing_capita <- monthly_landings %>% 
  mutate(catch_kg = catch_tons*1000) %>% 
  group_by(month = floor_date(date, "1 month")) %>% 
  summarise(catch_total = sum(catch_kg)) %>% 
  mutate(population = case_when(year(month) == "2018" ~ 1267797,
                                year(month) == "2019" ~ 1293120,
                                year(month) == "2020" ~ 1318442,
                                year(month) == "2021" ~ 1318442),
         catch_capita = catch_total/population) %>% 
  group_by(year = floor_date(month, "1 year")) %>% 
  summarise(mean_catch_capita = mean(catch_capita),
            se_catch_capita = std.error(catch_capita))

#calculating average monthly landings per year
landing_mm <- monthly_landings %>% 
  group_by(month = floor_date(date, "1 month")) %>% 
  summarise(catch_total = sum(catch_tons)) %>% 
  group_by(year = floor_date(month, "1 year")) %>% 
  summarise(mean_catch = mean(catch_total),
            se_catch = std.error(catch_total))
