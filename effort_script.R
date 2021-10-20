#DATA FROM TRIPS_SCRIPT.R AND SPECIES_SCRIPT.R (need to run those before starting this one)

#EFFORT AND CPUE
#IN THIS SCRIPT
#   - trip effort over time (strata together and in grid)
#   - seasonality of trip effort
#   - trip duration and active fishers over time (separately)
#   - calculating CPUE (standardise effort to 1 fisher and 1 trip hour)
#   - CPUE over time
#   - seasonality CPUE

library(plotrix)

#trip effort over time (fishers x hours)
effort_month <- trps_stns %>% 
  group_by(month = floor_date(date, "1 month"),  strata) %>%
  summarize(n = n_distinct(trip_id),
            effort_mean = (1/n)* sum(trip_effort2),
            effort_se = std.error(trip_effort2),
            triphours_mean = (1/n)* sum(trip_hours),
            triphours_se = std.error(trip_hours),
            fishermen_mean = (1/n)* sum(rel_effort),
            fishermen_se = std.error(rel_effort))  

#plot with areas together
ggplot(effort_month, aes(x=month, y=effort_mean, col = strata, group = strata)) +
  theme_light() + 
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = effort_mean - effort_se, ymax = effort_mean + effort_se), 
                size=.3, position = position_dodge(0.1)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Fishermen x trip hours", col = "Strata") +
  theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Paired") 

ggsave(path = "figures/Effort", filename = "effort_time.png", width = 18, height = 11, units = "cm")

#plots with area in grid + linear regression
ggplot(effort_month, aes(x=month, y=effort_mean, col = strata)) +
  theme_light() + 
  facet_grid(cols = vars(strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = effort_mean - effort_se, ymax = effort_mean + effort_se), 
                size=.3, position = position_dodge(0.1)) +
  geom_smooth(aes(x = month, y = effort_mean, group = strata), method = "lm", col ="black", linetype = "twodash", size = .6) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Fishermen x trip hours", col = "Strata") +
  theme(axis.title.x = element_blank(), legend.position = "none") +
  scale_color_brewer(palette = "Paired")

ggsave(path = "figures/Effort", filename = "effort_time_facet.png", width = 18, height = 11, units = "cm")

#seasonality of trip effort
effort_season <- trps_stns %>% 
  mutate(month = month(date),
         month_name = month(ymd(date), label = TRUE, abbr = TRUE))

ggplot(effort_season) +
  theme_light() +
  geom_boxplot(aes(x = month_name, y = trip_effort2, group = month)) +
  facet_wrap(~strata) +
  coord_cartesian(ylim = c(0, 30)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(y = "Fishermen x trip hours") + theme(axis.title.x = element_blank())

ggsave(path = "figures/effort", filename = "effort_season.png", width = 18, height = 11, units = "cm")



#trip duration over time
ggplot(effort_month, aes(x=month, y=triphours_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = triphours_mean - triphours_se, ymax = triphours_mean + triphours_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(title = "Mean duration of a fishing trip", 
       x = "Date", y = "Hours", col = "Strata") +
  scale_color_brewer(palette = "Paired") 

#active fishers on a trip over time
ggplot(effort_month, aes(x=month, y=fishermen_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = fishermen_mean - fishermen_se, ymax = fishermen_mean + fishermen_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(title = "Number of fishermen per trip", 
       x = "Date", y = "Number of fishermen", col = "Strata") +
  scale_color_brewer(palette = "Paired")

##CPUE from Jeppe:
#CPUE = (1/number of trips in one day) * sumofalltrips[(catch * (standard hours fishing trip/trip_hours) * (standard number of fishermen/fishermen))]
#standardise effort to 1 fisher and 1 trip hour.


CPUE_trip <- all_data %>%
  group_by(trip_id, date, strata, trip_hours, rel_effort, gear_code, habitat_code, boat_code, station_code) %>% 
  summarize(catch_weight = sum(weight_calc, na.rm = TRUE)/1000,
            catch_abundance = sum(nfish, na.rm = TRUE)) %>% 
  mutate(CPUE_weight =  catch_weight * (1/trip_hours) * (1/rel_effort),
         CPUE_abundance = catch_abundance * (1/trip_hours) * (1/rel_effort)) 

CPUE_month <- CPUE_trip %>% 
  group_by(month = floor_date(date, "1 month"),  strata) %>%
  summarize(n = n_distinct(trip_id),
            CPUE_weight_month = (1/n)* sum(CPUE_weight),
            weight_sd = sd(CPUE_weight),
            weight_se = weight_sd/sqrt(n()),
            weight_se2 = std.error(CPUE_weight),
            CPUE_abundance_month = (1/n)* sum(CPUE_abundance),
            nfish_se = std.error(CPUE_abundance))  

#plot CPUE areas together
ggplot(CPUE_month, aes(x=month, y=CPUE_weight_month, col = strata, group = strata)) +
  theme_light() + 
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = CPUE_weight_month - weight_se, ymax = CPUE_weight_month + weight_se), 
                size=.3, position = position_dodge(0.1)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-28")), linetype="dotted", 
             color = "red", size=0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Weight (kg) / fisher-hour", col = "Strata") + theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Paired") 

ggsave(path = "figures/Effort", filename = "CPUE_time.png", width = 18, height = 11, units = "cm")

#plot CPUE areas in grid
ggplot(CPUE_month, aes(x=month, y=CPUE_weight_month, col = strata)) +
  theme_light() + 
  facet_grid(cols = vars(strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = CPUE_weight_month - weight_se, ymax = CPUE_weight_month + weight_se), 
                size=.3, position = position_dodge(0.1)) +
  geom_smooth(aes(x = month, y = CPUE_weight_month, group = strata), method = "lm", col ="black", linetype = "twodash", size = .6) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Weight (kg) / fisher-hour", col = "Strata") + 
  theme(axis.title.x = element_blank(), legend.position = "none") +
  scale_color_brewer(palette = "Paired") 

ggsave(path = "figures/Effort", filename = "CPUE_time_facet.png", width = 18, height = 11, units = "cm")

#CPUE using number of fish (instead of weight as above)
ggplot(CPUE_month, aes(x=month, y=CPUE_abundance_month, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = CPUE_abundance_month - nfish_se, ymax = CPUE_abundance_month + nfish_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(title = "Catch per unit effort", 
       x = "", y = "Number of fish per fisher-hour", col = "Strata") +
  scale_color_brewer(palette = "Paired") 

ggsave(path = "figures/Effort", filename = "CPUE_time_nfish.png", width = 18, height = 11, units = "cm")


#seasonality of CPUE
CPUE_trip <- CPUE_trip %>% 
  mutate(month = month(date),
         month_name = month((date), label = TRUE, abbr = TRUE))

ggplot(CPUE_trip) +
  theme_light() +
  geom_boxplot(aes(x = month_name, y = CPUE_weight, group = month)) +
  # geom_smooth(aes(x = as.numeric(month_name), y = CPUE_weight), method = "loess", 
  #             linetype = "twodash", size = .6) +
  facet_wrap(~strata) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(y = "Weight (kg) / fisher-hour") + theme(axis.title.x = element_blank())

ggsave(path = "figures/effort", filename = "CPUE_season.png", width = 18, height = 11, units = "cm")


