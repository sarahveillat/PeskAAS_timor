#DATA FROM TRIPS_SCRIPT.R (need to run that one before starting this one)

#BOATS
#IN THIS SCRIPT
#   - boat composition (canoe/motorboat)
#   - boat composition over time (canoe/motorboat/shore-based)
#   - length of different boat types
#   - composition of motor types
#   - composition of material of boats
#   - number of fishers active on different boat types
#   - duration of trip of different boat types

#adding strata to boats df, and making the boat description uniform
boats <- boats %>%
  mutate(strata = case_when(
    municipality_name %in% c("Atauro") ~ "Atauro island",
    municipality_name %in% c("Baucau", "Lautem", "Bobonaro", "Liquica", "Oe-Cusse", "Manatuto", "Dili") ~ "North coast",
    municipality_name %in% c("Viqueque", "Covalima","Manufahi", "Ainaro") ~ "South coast")) %>% 
  mutate(boat_description = case_when(
    str_detect(boat_description, "wood") ~ "wood",
    str_detect(boat_description, "fiberglass")  ~ "fiberglass",
    TRUE ~ NA_character_))

#some checks
boats %>% 
  group_by(strata) %>% 
  tally() %>% view

boats %>% 
  group_by(strata, boat_code, boat_description) %>% 
  summarize(n = n()) %>% 
  group_by(strata, boat_code) %>% 
  mutate(tot = sum(n),
         perc = n/tot *100) %>% view

boats %>% 
  group_by(strata, boat_code, motor_type) %>% 
  summarize(n = n()) %>% 
  group_by(strata, boat_code) %>% 
  mutate(tot = sum(n),
         perc = n/tot *100) %>% view

boats %>% 
  group_by(strata, boat_code) %>% 
  summarize(n = n()) %>% 
  group_by(strata) %>% 
  mutate(tot = sum(n),
         perc = n/tot) %>% view

anti_join(trips, boats, by = "boat_id") %>% view #a lot of trips don't have a boat_id.


#plots boat composition and other information
c26 <- c(
  "#A6CEE3", "#1F78B4",
  "#58181F" 
)

#boat composition in numbers
ggplot(subset(boats, !is.na(strata))) +
  geom_bar(aes(x = strata, fill = as.factor(boat_code)), colour = "black", 
           position = "stack", size = 0.2) +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(title = "No. of registered boats per strata", 
       x = "Strata", y = "No. of boats", fill = "Boat type") +
  theme_light() + scale_fill_manual(labels = c("Canoe", "Motorboat"), values = c26) 

ggsave(path = "figures/Boat", filename = "RegBoat_use.png", width = 20, height = 11, units = "cm")

#boat composition in percentage
ggplot(subset(boats, !is.na(strata))) +
  geom_bar(aes(x = strata, fill = as.factor(boat_code)), colour = "black", 
           position = "fill", size = 0.2) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(y = "% of sampled boats", fill = "Boat type") + 
  theme_light() + theme(axis.title.x = element_blank()) + 
  scale_fill_manual(labels = c("Canoe", "Motorboat"), values = c26) 

ggsave(path = "figures/Boat", filename = "RegBoat_use_percent.png", width = 18, height = 11, units = "cm")

#boat use over time
boat_time <- trps_stns %>%
  group_by(date = floor_date(date, "1 month"), strata, boat_code) %>%
  summarize(count = n()) %>% 
  mutate(percent = (count/sum(count))*100, totalpercent = sum(percent), totalcount = sum(count))

c27 <- c(
  "#00BFC4", "#1F78B4",
  "#7CAE00"
)

ggplot(boat_time) +
  theme_light() + 
  geom_line(aes(x = date, col = as.factor(boat_code), y = count)) + facet_wrap(~strata) +
  geom_smooth(aes(x = date, y = count, group = as.factor(boat_code), fill = as.factor(boat_code)), method = "lm", col ="black", linetype = "twodash", size = .6) +
  labs(x = "", y = "Number of sampled trips", fill = "Boat type") +
  scale_color_manual(labels = c("Canoe", "Motorboat", "Shore-based"), 
                                      values = c27) + 
  scale_fill_manual(labels = c("Canoe", "Motorboat", "Shore-based"), 
                     values = c28) + 
  scale_y_continuous(limits= c(0,500)) +
  coord_cartesian(ylim = c(0, 500)) +
  guides(col = FALSE) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) 

ggsave(path = "figures/Boat", filename = "samples_boat_time_count.png", width = 20, height = 11, units = "cm")

#length of boats
median_IQR <- function(x) {
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4])  # 3rd quartile
}

ggplot(subset(boats, !is.na(strata))) + theme_light() +
  geom_violin(aes(y = length_m, x = strata, group = strata, fill = as.factor(boat_code))) +
  facet_wrap(~boat_code) +
  stat_summary(aes(x = strata, y = length_m), fun.data = median_IQR, #cut of at zero
               geom="point", size = 1.4) +
  stat_summary(aes(x = strata, y = length_m),
               fun.data = median_IQR, geom = "errorbar", width = .4) +
  scale_y_continuous(breaks = seq(0,15,1)) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  labs(x = "", y = "Length (m)", fill = "Boat type") + 
  scale_fill_manual(labels = c("Canoe", "Motorboat"), values = c26)

ggsave(path = "figures/Boat", filename = "length_boats_violinplots.png", width = 18, height = 12, units = "cm")

#composition of motor type
motorboats <- subset(boats, boat_code == 2)

ggplot(subset(motorboats, !is.na(strata))) +
  geom_bar(aes(x = strata, fill = motor_type), colour = "black", 
           position = "fill", size = 0.2) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(y = "% of sampled motorboats", fill = "Motor type") + 
  theme_light() + theme(axis.title.x = element_blank()) + 
  scale_fill_manual(labels = c("Inboard", "Outboard", "Unknown"), values = c26, na.value = "#FAA0A0") 

ggsave(path = "figures/Boat", filename = "Motor_type.png", width = 18, height = 12, units = "cm")

#composition of material of the boats
boat.labs <- c("Canoe", "Motorboat")
names(boat.labs) <- c("1", "2")

ggplot(subset(boats, !is.na(strata))) +
  theme_light() +
  geom_bar(aes(x = strata, fill = boat_description), colour = "black", 
           position = "fill", size = 0.2) +
  facet_wrap(~boat_code, labeller = labeller(boat_code = boat.labs)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(y = "% of sampled boats", fill = "Boat material") +
  theme(axis.title.x = element_blank()) + 
  scale_fill_manual(labels = c("Fiberglass", "Wood", "Unknown"), values = c26, na.value = "#FAA0A0") 

ggsave(path = "figures/Boat", filename = "boat_material.png", width = 18, height = 12, units = "cm")


#number of people on the different boat types per strata

median_IQR <- function(x) {
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4])  # 3rd quartile
}

ggplot(subset(trps_stns, boat_code != "3")) + theme_light() +
  geom_violin(aes(y = rel_effort, x = strata, group = strata, fill = as.factor(boat_code))) +
  facet_wrap(~boat_code) +
  stat_summary(aes(x = strata, y = rel_effort), fun.data = median_IQR, #cut of at zero
               geom="point", size = 1.4) +
  stat_summary(aes(x = strata, y = rel_effort),
               fun.data = median_IQR, geom = "errorbar", width = .7) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  labs(x = "", y = "Number of people per trip", fill = "Boat type") + 
  scale_fill_manual(labels = c("Canoe", "Motorboat"), values = c26)

ggsave(path = "figures/Boat", filename = "releff_boats_violinplots.png", width = 18, height = 12, units = "cm")

#duration of trip per boat type and strata

median_IQR <- function(x) {
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4])  # 3rd quartile
}

ggplot(subset(trps_stns, boat_code != "3")) + theme_light() +
  geom_violin(aes(y = trip_hours, x = strata, group = strata, fill = as.factor(boat_code))) +
  facet_wrap(~boat_code) +
  stat_summary(aes(x = strata, y = trip_hours), fun.data = median_IQR, #cut of at zero
               geom="point", size = 1.4) +
  stat_summary(aes(x = strata, y = trip_hours),
               fun.data = median_IQR, geom = "errorbar", width = .7) +
  scale_y_continuous(breaks = seq(0,13,1)) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  labs(x = "", y = "Trip duration (hours)", fill = "Boat type") + 
  scale_fill_manual(labels = c("Canoe", "Motorboat"), values = c26)

ggsave(path = "figures/Boat", filename = "triphours_boats_violinplots.png", width = 18, height = 12, units = "cm")

