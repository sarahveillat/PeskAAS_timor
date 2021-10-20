#DATA FROM TRIPS_SCRIPT.R AND SPECIES_SCRIPT.R (need to run those before starting this one)

#GEAR
#IN THIS SCRIPT
#   - gear composition (strata)
#   - gear use over time (strata + municipalities)


gear <- gear %>% 
  mutate(gear_code = as.factor(gear_code))


#GEAR COMPOSITION (using IRI)
#calculate how often a gear is used at every strata
ngear <- left_join(trps_stns, gear, by = "gear_code") %>% 
  group_by(strata, gear_name) %>% 
  summarise(ngear = n())

#calculating total catch (abundance and weight) of each gear type at every strata
gear_composition <- left_join(all_data, gear, by = "gear_code") %>% 
  group_by(strata) %>% 
  mutate(ntrip = n_distinct(trip_id)) %>% 
  group_by(strata, gear_name, ntrip) %>% 
  summarize(sumabundance = sum(nfish, na.rm = TRUE), sumweight = sum(weight_calc, na.rm = TRUE), .groups = "drop_last")  

gear_composition <- left_join(gear_composition, ngear, 
                              by = c("strata" = "strata", "gear_name" = "gear_name")) %>% 
  mutate(n = (ngear/ntrip[1]*100)) %>% 
  group_by(strata) %>% 
  mutate(per_abundance = (sumabundance/sum(sumabundance, na.rm = TRUE) * 100), per_weight = (sumweight/sum(sumweight, na.rm = TRUE)*100)) %>% 
  arrange(sumweight) %>% 
  mutate(x_end = cumsum(n), x_start = cumsum(c(0,n)[-(n()+1)])) %>% 
  ungroup() %>% 
  #mutate(gear_code = fct_reorder(.f = gear_code, .x = sumabundance, .fun = median)) %>% 
  pivot_longer(c(per_abundance, per_weight)) %>% 
  mutate(value = if_else(name == "per_weight",
                         true = value,
                         false = -1 * value)) %>% 
  mutate(gear_name = fct_relevel(gear_name, c("Gill net", "Long line", "Spear gun", "Hand line", "Beach seine", "Cast net", "Manual collection", "Seine net", "Trap")))

breaks <- seq(20, 100, 20)
library(grid)

gear_composition_AI <- gear_composition %>% 
  filter((strata == "Atauro island")) 
gear_composition_SC <- gear_composition %>% 
  filter((strata == "South coast")) 
gear_composition_NC <- gear_composition %>% 
  filter((strata == "North coast")) 
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

ggplot(gear_composition_AI, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = gear_name)) +
  geom_rect(colour = "black") +
  #geom_text(aes(label = gear_name, y = 0, x = (x_start + x_end)/2)) +
  coord_flip(clip = "off") +
  scale_x_continuous(breaks = (gear_composition_AI$x_start + gear_composition_AI$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(gear_composition_AI$n, 0)), '%')) +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-15, xmax=-15,ymin=-30, ymax=-30) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-15, xmax=-15,ymin=30, ymax=30) +
  labs(title = "Atauro Island", x = "FRQ of occurrence", fill = "Gear", y = " ") +
  scale_fill_brewer(palette = "Paired") 


ggsave(path = "figures/Gear", filename = "Gear_IRI_AI.png", width = 17, height = 9.5, units = "cm")


ggplot(gear_composition_NC, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = gear_name)) +
  geom_rect(colour = "black") +
  #geom_text(aes(label = gear_name, y = 0, x = (x_start + x_end)/2)) +
  coord_flip(clip = "off") +
  scale_x_continuous(breaks = (gear_composition_NC$x_start + gear_composition_NC$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(gear_composition_NC$n, 0)), '%')) +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-14, xmax=-14,ymin=-50, ymax=-50) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-14, xmax=-14,ymin=50, ymax=50) +
  labs(title = "North coast", x = "FRQ of occurrence", fill = "Gear", y = " ") +
  scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Gear", filename = "Gear_IRI_NC.png", width = 17, height = 9.5, units = "cm")


ggplot(gear_composition_SC, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = gear_name)) +
  geom_rect(colour = "black") +
  #geom_text(aes(label = gear_name, y = 0, x = (x_start + x_end)/2)) +
  coord_flip(clip = "off") +
  scale_x_continuous(breaks = (gear_composition_SC$x_start + gear_composition_SC$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(gear_composition_SC$n, 0)), '%')) +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-14.5, xmax=-14.5,ymin=-30, ymax=-30) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-14.5, xmax=-14.5,ymin=50, ymax=50) +
  labs(title = "South coast", x = "FRQ of occurrence", fill = "Gear", y = " ") +
  scale_fill_brewer(palette = "Paired") 

ggsave(path = "figures/Gear", filename = "Gear_IRI_SC.png", width = 17, height = 9.5, units = "cm")


#GEAR USE OVER TIME
gear_time <- left_join(trps_stns, gear, by = "gear_code") %>%
  group_by(date = floor_date(date, "6 month"), strata, gear_name) %>%
  summarize(count = n()) %>% 
  mutate(percent = (count/sum(count))*100, totalpercent = sum(percent), totalcount = sum(count)) %>% 
  mutate(gear_name = fct_reorder(gear_name, count, .fun = median, .desc = TRUE))


ggplot(gear_time) +
  theme_light() + 
  geom_col(aes(x = date, fill = as.factor(gear_name), y = percent), colour = "black", 
           position = "stack", size = 0.2) + facet_wrap(~strata) +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(y = "% use of gear", fill = "Gear") + 
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_brewer(palette="Paired") +
  scale_x_date(date_labels = "%b %Y", guide = guide_axis(angle = 45))

ggsave(path = "figures/Gear", filename = "gear_time.png", width = 20, height = 11, units = "cm")
#above plot shows drastic shift in gear type from hand to long line at south coast, check every municipality:

gear_time_SC <- left_join(trps_stns, gear, by = "gear_code") %>%
  filter(strata == "South coast") %>% 
  group_by(date = floor_date(date, "6 month"), municipality, gear_name) %>%
  summarize(count = n()) %>% 
  mutate(percent = (count/sum(count))*100, totalpercent = sum(percent), totalcount = sum(count)) %>% 
  mutate(gear_name = fct_relevel(gear_name, c("Gill net", "Long line", "Spear gun", "Hand line", "Beach seine", "Cast net", "Manual collection", "Seine net", "Trap")))


gear_time_SC %>% 
  mutate(gear_name = fct_relevel(gear_name, c("Gill net", "Long line", "Spear gun", "Hand line", "Beach seine", "Cast net", "Manual collection", "Seine net", "Trap"))) %>% 
ggplot(data = .) +
  geom_col(aes(x = date, fill = gear_name, y = percent), colour = "black", 
           position = "stack", size = 0.2) + facet_wrap(~municipality) +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(title = "South coast", x = " ", y = "% use of gear", fill = "Gear") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light() + scale_fill_brewer(palette="Paired") +
  scale_x_date(date_labels = "'%y")

ggsave(path = "figures/Gear", filename = "gear_time_SC.png", width = 20, height = 16, units = "cm")
#above plot shows that shift happens at viqueque


gear_time_NC <- left_join(trps_stns, gear, by = "gear_code") %>%
  filter(strata == "North coast") %>% 
  group_by(date = floor_date(date, "6 month"), municipality, gear_name) %>%
  summarize(count = n()) %>% 
  mutate(percent = (count/sum(count))*100, totalpercent = sum(percent), totalcount = sum(count)) %>% 
  mutate(gear_name = fct_relevel(gear_name, c("Gill net", "Long line", "Spear gun", "Hand line", "Beach seine", "Cast net", "Manual collection", "Seine net", "Trap")))

gear_time_NC %>% 
  mutate(gear_name = fct_relevel(gear_name, c("Gill net", "Long line", "Spear gun", "Hand line", "Beach seine", "Cast net", "Manual collection", "Seine net", "Trap"))) %>% 
  ggplot(data = .) +
  geom_col(aes(x = date, fill = as.factor(gear_name), y = percent), colour = "black", 
           position = "stack", size = 0.2) + facet_wrap(~municipality) +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(title = "North coast", x = " ", y = "% use of gear", fill = "Gear") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light() + scale_fill_brewer(palette="Paired") +
  scale_x_date(date_labels = "'%y")

ggsave(path = "figures/Gear", filename = "gear_time_NC.png", width = 20, height = 16, units = "cm")





