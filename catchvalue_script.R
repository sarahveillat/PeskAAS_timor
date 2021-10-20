#DATA FROM TRIPS_SCRIPT.R AND SPECIES_SCRIPT.R (need to run those before starting this one)

#MARKET VALUE OF CATCH
#IN THIS SCRIPT
#   - calculate market value per kg catch
#   - plot value per area over time
#   - plot value per gear over time
#   - calculate how many trips have catches which consist 95% or more of one functional group/species
#   - plot value per functional group over time
#   - plot value per species over time


#VALUE 

#calculate value per kg catch
catchvalue <- all_data %>% 
  filter(food_sale == "sale") %>% 
  group_by(trip_id, date, strata, catch_value, municipality, boat_code, gear_code, habitat_code, trip_effort2, station_code) %>% 
  summarize(landings_g = sum(weight_calc, na.rm = TRUE)) %>% 
  filter(landings_g != 0) %>% 
  mutate(catch_value = replace(catch_value, landings_g == 0, 0)) %>% 
  mutate(landings_kg = landings_g/1000,
         value_per_kg = catch_value/landings_kg)

sum(is.na(catchvalue$catch_value))

#remove na's
catchvalue <- catchvalue %>% 
  filter(!is.na(catch_value))

#make plot of catch value over time per area
library(plotrix) #std.error function
catchvalue_month <- catchvalue %>% 
  group_by(month = floor_date(date, "1 month"),  strata) %>%
  summarize(n = n_distinct(trip_id),
            value_kg_mean = (1/n)* sum(value_per_kg),
            value_kg_se = std.error(value_per_kg)) %>% 
  filter(month >= "2019-07-01")

ggplot(catchvalue_month, aes(x=month, y=value_kg_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = value_kg_mean - value_kg_se, ymax = value_kg_mean + value_kg_se), 
                size=.3, position = position_dodge(0.1)) +
  geom_smooth(aes(x = month, y = value_kg_mean, group = strata, fill = strata), method = "lm", col ="black", linetype = "twodash", size = .6) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  theme_light() + scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Price (USD) per kg catch", col = "Strata") + theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Paired") + scale_fill_brewer(palette = "Paired") + guides(fill = FALSE)

ggsave(path = "figures/market/value", filename = "catchvalue_kg.png", width = 18, height = 11, units = "cm")

### CATCH VALUE PER GEAR TYPE
catchvalue_gear <- catchvalue %>% 
  filter(gear_code == "1" | gear_code == "2" | gear_code == "3" | gear_code == "4") %>% 
  group_by(month = floor_date(date, "1 month"),  gear_code) %>%
  summarize(n = n_distinct(trip_id),
            value_kg_mean = (1/n)* sum(value_per_kg),
            value_kg_se = std.error(value_per_kg)) %>% 
  filter(month >= "2019-07-01")

gear.labs <- c("Gill net", "Hand line", "Long line", "Spear gun")
names(gear.labs) <- c("1", "2","3", "4")

ggplot(catchvalue_gear, aes(x=month, y=value_kg_mean)) +
  facet_wrap(~gear_code, scales = "free_y", labeller = labeller(gear_code = gear.labs)) +
  geom_line(size=.5, position = position_dodge(0.1)) + 
  geom_errorbar(aes(ymin = value_kg_mean - value_kg_se, ymax = value_kg_mean + value_kg_se), 
                size=.3, position = position_dodge(0.1)) +
  # geom_smooth(aes(x = month, y = value_kg_mean), method = "lm", col ="blue", fil = "blue", linetype = "twodash", size = .6) + 
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  theme_light() + scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Price (USD) per kg catch") + theme(axis.title.x = element_blank()) 

ggsave(path = "figures/market/value", filename = "catchvalue_kg_gear.png", width = 15, height = 10, units = "cm")


#VALUE PER SPECIES/FUNCTIONAL GROUP
#as the catch value is given for an entire trip, and not per species caught, some estimations have to be made
#if a catch of a trip consists 95% of one species/fg, we treat it as 100%, and thus the value is linked to that species/fg

#functional groups
all_data_fg <- all_data %>%
  filter(weight_calc != 0,
         food_sale == "sale",
         !is.na(catch_value)) %>% 
  group_by(strata, date, trip_id, functional_group) %>% 
  summarize(weight_species = (sum(weight_calc, na.rm = TRUE))/1000) %>% 
  group_by(strata, trip_id) %>% 
  mutate(weight_trip = (sum(weight_species)),
         perc_weight_trip = (weight_species/weight_trip) * 100,
         test = sum(perc_weight_trip)) 

length(unique(all_data_fg$trip_id)) #19829 trips

#show histogram of percentage of catch (kg) of one trip consisting of one fg
all_data_fg %>% 
  summarise(perc_weight_trip_max = max(perc_weight_trip)) %>% 
  ggplot() +
  geom_histogram(aes(x = perc_weight_trip_max), binwidth = 5) +
  scale_x_reverse(breaks = seq(0, 105, 10), guide = guide_axis(angle = 45),
                  labels = function(x) paste0(x, "%")) +
  theme_light() + coord_cartesian(xlim = c(100, 30)) +
  labs(x = "Percentage of catch (kg) consisting of one functional group", y = "Trip count") +
  facet_wrap(~strata)

ggsave(path = "figures/samples", filename = "perc_one_fg.png", width = 18, height = 11, units = "cm")


#show value per FG over time
trips_95FG <- all_data %>%
  filter(weight_calc != 0,
         food_sale == "sale",
         !is.na(catch_value)) %>% 
  group_by(strata, trip_id, functional_group) %>% 
  summarize(weight_fg = (sum(weight_calc))/1000) %>% 
  group_by(strata, trip_id) %>% 
  mutate(weight_trip = (sum(weight_fg)),
         perc_weight_trip = weight_fg/weight_trip,
         test = sum(perc_weight_trip)) %>% #19829 trips
  filter(perc_weight_trip >= 0.95)  #15786 trips after filter --> 15786 / 19829 * 100 = 79,6 % of the trips

length(unique(trips_95FG$trip_id))

valuefg <- right_join(trps_stns, trips_95FG, by = c("strata" = "strata", "trip_id" = "trip_id")) %>%
  filter(!is.na(catch_value)) %>% 
  mutate(value_per_kg = catch_value/weight_trip) %>% 
  group_by(strata, functional_group) %>%
  mutate(total_observations = n()) %>% 
  filter(total_observations >= 10, #filter out species that are seen less than 10 times
         date >= "2019-07-01") %>% 
  filter(!(functional_group == "Small demersal" & value_per_kg > 100 & strata == "North coast")) #filter out extreme outliers that are very likely sampling errors

valuefg_month <- valuefg %>% 
  group_by(month = floor_date(date, "1 month"),  strata, functional_group) %>%
  summarize(value_mean = mean(value_per_kg),
            value_se = std.error(value_per_kg))

valuefg_month %>% 
  mutate(functional_group = fct_relevel(functional_group, c("Small pelagics", "Large pelagics", "Small demersal", "Large demersal", "Cephalopods", "Shark and rays", "Unknown"))) %>% 
  ggplot(aes(x=month, y=value_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + #geom_point() + 
  geom_errorbar(aes(ymin = value_mean - value_se, ymax = value_mean + value_se), 
                size=.3, position = position_dodge(0.1)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  theme_light() + scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  facet_wrap(~functional_group, scales="free_y") +
  labs(y = "Price (USD) per kg catch", col = "Strata") +
  scale_color_brewer(palette = "Paired") + theme(legend.position="top", axis.title.x = element_blank())

ggsave(path = "figures/market/value", filename = "catchvalue_fg_kg.png", width = 20, height = 14, units = "cm")


#species
all_data_sp <- all_data %>%
  filter(weight_calc != 0,
         food_sale == "sale",
         !is.na(catch_value)) %>%
  group_by(strata, date, trip_id, functional_group, category, species_code) %>% 
  summarize(weight_species = (sum(weight_calc, na.rm = TRUE))/1000) %>% 
  group_by(strata, trip_id) %>% 
  mutate(weight_trip = (sum(weight_species)),
         perc_weight_trip = (weight_species/weight_trip) * 100,
         test = sum(perc_weight_trip)) 

length(unique(all_data_sp$trip_id)) #19829 trips
19829/37470 * 100

#show histogram of percentage of catch (kg) of one trip consisting of one species
all_data_sp %>% 
  summarise(perc_weight_trip_max = max(perc_weight_trip)) %>% 
  ggplot() +
  geom_histogram(aes(x = perc_weight_trip_max), binwidth = 5) +
  scale_x_reverse(breaks = seq(0, 105, 10), guide = guide_axis(angle = 45),
                     labels = function(x) paste0(x, "%")) +
  theme_light() +
  labs(x = "Percentage of catch (kg) consisting of one species", y = "Trip count") +
  facet_wrap(~strata)

ggsave(path = "figures/samples", filename = "perc_one_sp.png", width = 18, height = 11, units = "cm")


#choose all trips where 95% of the catch consists of one species
trips_95species <- all_data_sp %>% 
  filter(perc_weight_trip >= 95)  #14330 trips after filter --> 14330 / 19829 * 100 = 72,26789 % of the trips

length(unique(trips_95species$trip_id))

#show value of species over time
valuespecies <- right_join(trps_stns, trips_95species, by = c("strata" = "strata", "trip_id" = "trip_id", "date" = "date")) %>%
  filter(!is.na(catch_value)) %>% 
  mutate(value_per_kg = catch_value/weight_trip) %>% 
  group_by(strata, category, species_code) %>%
  mutate(total_observations = n()) %>% 
  filter(total_observations >= 10,
         date >= "2019-07-01") #filter out species that are seen less than 10 times

valuespecies_month <- valuespecies %>% 
  group_by(month = floor_date(date, "1 month"),  strata, functional_group, category) %>%
  summarize(value_mean = mean(value_per_kg),
            value_se = std.error(value_per_kg))  

#all species together
ggplot(valuespecies_month, aes(x=month, y=value_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + #geom_point() + 
  geom_errorbar(aes(ymin = value_mean - value_se, ymax = value_mean + value_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + 
  scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  facet_wrap(~category, scales="free") +
  labs(title = "Catch value per kg", 
       x = "Date", y = "Price ($) per kg catch", col = "Strata") +
  scale_color_brewer(palette = "Paired") + 
  theme(legend.position="top")

ggsave(path = "figures/Landings", filename = "catchvalue_species_kg.png", width = 20, height = 14, units = "cm")


#show value of species over time but per functional group
ggplot(subset(valuespecies_month, functional_group == "Small pelagics"), aes(x=month, y=value_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + #geom_point() + 
  geom_errorbar(aes(ymin = value_mean - value_se, ymax = value_mean + value_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + 
  scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  facet_wrap(~category, scales="free", ncol = 4) +
  labs(title = "Small pelagics",y = "Price ($) per kg catch", col = "Strata") +
  scale_color_brewer(palette = "Paired") + 
  theme(legend.position="top", axis.title.x = element_blank())

ggsave(path = "figures/Landings", filename = "catchvalue_species_SP.png", width = 20, height = 12, units = "cm")


ggplot(subset(valuespecies_month, functional_group == "Large pelagics"), aes(x=month, y=value_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + #geom_point() + 
  geom_errorbar(aes(ymin = value_mean - value_se, ymax = value_mean + value_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + 
  scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  facet_wrap(~category, scales="free") +
  labs(title = "Large pelagics", y = "Price ($) per kg catch", col = "Strata") +
  scale_color_brewer(palette = "Paired") + 
  theme(legend.position="top", axis.title.x = element_blank())

ggsave(path = "figures/Landings", filename = "catchvalue_species_LP.png", width = 20, height = 14, units = "cm")

ggplot(subset(valuespecies_month, functional_group == "Small demersal"), aes(x=month, y=value_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + #geom_point() + 
  geom_errorbar(aes(ymin = value_mean - value_se, ymax = value_mean + value_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + 
  scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  facet_wrap(~category, scales="free") +
  labs(title = "Small demersal", y = "Price ($) per kg catch", col = "Strata") +
  scale_color_brewer(palette = "Paired") + 
  theme(legend.position="top", axis.title.x = element_blank())

ggsave(path = "figures/Landings", filename = "catchvalue_species_SD.png", width = 20, height = 14, units = "cm")


ggplot(subset(valuespecies_month, functional_group == "Large demersal"), aes(x=month, y=value_mean, col = strata, group = strata)) +
  geom_line(size=.5, position = position_dodge(0.1)) + #geom_point() + 
  geom_errorbar(aes(ymin = value_mean - value_se, ymax = value_mean + value_se), 
                size=.3, position = position_dodge(0.1)) +
  theme_light() + 
  scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  facet_wrap(~category, scales="free") +
  labs(title = "Large demersal", y = "Price ($) per kg catch", col = "Strata") +
  scale_color_brewer(palette = "Paired") + 
  theme(legend.position="top", axis.title.x = element_blank())

ggsave(path = "figures/Landings", filename = "catchvalue_species_LD.png", width = 20, height = 11, units = "cm")


