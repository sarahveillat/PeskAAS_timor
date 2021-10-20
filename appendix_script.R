###check the number of entries that will be changed/removed (and also overview of dataset in count information)
###check median of different variables


##TRIP DURATION
#count
trips %>% 
  group_by(trip_hours) %>%
  mutate(trip_hours = replace(trip_hours, trip_hours >= 14, 14)) %>% #lump all trips above 13 hours together
  tally() %>% view

#median per strata and boat code
trips_stns %>% 
  filter(boat_code ==1) %>% 
  group_by(strata) %>% 
  summarize(median = median(trip_hours)) %>% view

trips_stns %>% 
  filter(boat_code ==2) %>% 
  group_by(strata) %>% 
  summarize(median = median(trip_hours)) %>% view

trips_stns %>% 
  filter(boat_code ==3) %>% 
  group_by(strata) %>% 
  summarize(median = median(trip_hours)) %>% view

#histogram
boat.labs <- c("Canoe", "Motorboat", "Shore-based")
names(boat.labs) <- c(1, 2, 3)
ggplot(trips_stns) +
  geom_histogram(aes(x = trip_hours), binwidth = 1, col = "black", fill = "grey") +
  facet_grid(boat_code ~ strata, scales = "free", labeller = labeller(boat_code = boat.labs)) + theme_light() + scale_x_continuous(breaks = seq(0, 20, 1), limits = c(0,15)) +
  labs(x = "Trip duration (hours)") + theme(strip.text.x = element_text(size = 12, face = "bold"),
                                            strip.text.y = element_text(size = 12, face = "bold"))

#ggsave(path = "figures/appendix/effort", filename = "histogram_tripduration.png", width = 22, height = 22, units = "cm")

##NUMBER OF FISHERS
#count
trips %>% 
  group_by(rel_effort) %>%
  mutate(rel_effort = replace(rel_effort, rel_effort >= 11, 11)) %>% #lump all trips with more than 11 fishers together
  tally() %>% view

#median per strata and boat_code
trips_stns %>% 
  filter(boat_code ==1) %>% 
  group_by(strata) %>% 
  summarize(median = median(rel_effort)) %>% view

trips_stns %>% 
  filter(boat_code ==2) %>% 
  group_by(strata) %>% 
  summarize(median = median(rel_effort)) %>% view

trips_stns %>% 
  filter(boat_code ==3) %>% 
  group_by(strata) %>% 
  summarize(median = median(rel_effort)) %>% view

#histogram
ggplot(trips_stns) +
  geom_histogram(aes(x = rel_effort), binwidth = 1, col = "black", fill = "grey") +
  facet_grid(boat_code ~ strata, scales = "free", labeller = labeller(boat_code = boat.labs)) + theme_light() + scale_x_continuous(breaks = seq(0, 20, 1), limits = c(0,18)) + labs(x = "Number of fishermen") + theme(
    strip.text.x = element_text(
      size = 12, face = "bold"
    ),
    strip.text.y = element_text(
      size = 12, face = "bold"
    )
  )

#ggsave(path = "figures/appendix/effort", filename = "histogram_fishermen.png", width = 22, height = 22, units = "cm")

##MARKET VALUE--> a lot of NA's
trips %>% 
  group_by(catch_value) %>%
  tally() %>% view

ggplot(trips) +
  geom_histogram(aes(x = catch_value), binwidth = 20, col = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 4500, 500), limits = c(0,4000)) +
  theme_light() + labs(x = "Market value (USD)") + theme(strip.text.x = element_text(size = 12, face = "bold"),
                                            strip.text.y = element_text(size = 12, face = "bold"))

##LANDINGS
##number of fish
#count
landings %>% 
  group_by(nfish) %>%
  mutate(nfish = replace(nfish, nfish >= 10001, 10001)) %>% 
  tally() %>% view

#histogram
ggplot(landings) +
  geom_histogram(aes(x = nfish), bins = 100, col = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 10500, 500), limits = c(0,1000)) +
  theme_light() + labs(x = "Number of fish") + theme(strip.text.x = element_text(size = 12, face = "bold"),
                                                         strip.text.y = element_text(size = 12, face = "bold"))

##weight of fish
#count
landings %>% 
  group_by(weight_g) %>%
  tally() %>% view

#histogram
ggplot(landings) +
  geom_histogram(aes(x = weight_g), bins = 100, col = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 105000, 5000), limits = c(0,100000)) +
  theme_light() + labs(x = "Weight of fish") + theme(strip.text.x = element_text(size = 12, face = "bold"),
                                                     strip.text.y = element_text(size = 12, face = "bold"))


##SPECIES
#number of entries without a and b constants:
lndngs_spcies %>% 
  filter(is.na(a)) %>%
  tally() %>% view

#number of entries per fish species
lndngs_spcies %>% 
  filter(nfish > 0) %>% 
  group_by(category) %>% 
  tally() %>% view



