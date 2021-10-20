#DATA FROM TRIPS_SCRIPT.R AND SPECIES_SCRIPT.R (need to run those before starting this one)

#FOOD OR SALE
#IN THIS SCRIPT
#   - plot of destination of the catch (kg) (food/sale/both) over time 
#   - plot of catch used for own consumption over time
#   - plot of destination of catch for different size groups
#   - plot of destination of catch of different species and functional groups



#food/sale ratio over time
#catch in weight is used to show this

foodsale <- all_data %>%
  filter(weight_calc != 0) %>% 
  mutate(weight_calc = weight_calc/1000) %>% #make kg
  mutate(food_sale = replace(food_sale, food_sale == "food", "Own consumption"),
         food_sale = replace(food_sale, food_sale == "sale", "Sold on market"),
         food_sale = replace(food_sale, food_sale == "both", "Both")) %>% 
  group_by(week = floor_date(date, "1 week"), strata, food_sale) %>%
  summarize(weight_fs = sum(weight_calc)) %>% 
  group_by(week, strata) %>% 
  mutate(weight_tot = sum(weight_fs),
         percent = (weight_fs/weight_tot) *100)

foodsale_mean <- foodsale %>%
  filter(!is.na(food_sale)) %>% 
  group_by(month = floor_date(week, "3 months"), strata, food_sale) %>%
  summarize(perc_mean = mean(percent),
            perc_se = std.error(percent),
            weight_mean = mean(weight_fs),
            weight_se = std.error(weight_fs)) 

foodsale_mean %>% 
  mutate(food_sale = fct_reorder(food_sale, weight_mean, .fun = min, .desc = TRUE)) %>% 
ggplot(aes(x=month, y=weight_mean, col = food_sale)) +
  theme_light() + 
  geom_line(size=.5, position = position_dodge(0.1)) + geom_point() + 
  geom_errorbar(aes(ymin = weight_mean - weight_se, ymax = weight_mean + weight_se), 
                size=.3, position = position_dodge(0.1)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  facet_wrap(~strata) +
  scale_x_date(date_breaks = "6 months" , date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Weight (kg)", col = "Consumption or market") + 
  scale_color_brewer(palette = "Paired") + theme(axis.title.x = element_blank(), legend.position = "top")

ggsave(path = "figures/market", filename = "foodorsale_time_weight.png", width = 18, height = 11, units = "cm")

#show only the catch that is used for own consumption over time
foodsale %>% 
  filter(food_sale == "Own consumption") %>% 
  ggplot(aes(x=week, y=weight_fs)) +
  theme_light() + 
  geom_line(size=.5, position = position_dodge(0.1), col = "#1F78B4") + #geom_point() + 
  # geom_errorbar(aes(ymin = weight_mean - weight_se, ymax = weight_mean + weight_se), 
  #               size=.3, position = position_dodge(0.1)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-29")), linetype="dotted", 
             color = "red", size=0.8) +
  facet_grid(rows = vars(strata)) +
  scale_x_date(date_breaks = "6 months" , date_labels = "%b %Y", guide = guide_axis(angle = 45)) +
  labs(y = "Weight (kg)", col = "Consumption or market") + 
  scale_color_brewer(palette = "Paired") + theme(axis.title.x = element_blank())

ggsave(path = "figures/market", filename = "food_time_weight.png", width = 18, height = 11, units = "cm")

#destination for fish size
#make length groups of fish sizes
midcut<-function(x,from,to,length){
  breaks = seq(from,to,length=length+1)
  ## cut the data into bins...
  x=cut(x,breaks,include.lowest=T)
  ## make a named vector of the midpoints, names=binnames
  vec=(breaks[-length] + breaks[-1])/2
  names(vec)=levels(x)
  ## use the vector to map the names of the bins to the midpoint values
  unname(vec[x])
}

foodsale_size <- all_data %>%
  filter(weight_calc != 0) %>% 
  mutate(weight_calc = weight_calc/1000) %>% 
  mutate(food_sale = replace(food_sale, food_sale == "food", "Own consumption"),
         food_sale = replace(food_sale, food_sale == "sale", "Sold on market"),
         food_sale = replace(food_sale, food_sale == "both", "Both")) %>% 
  mutate(cut_length = midcut(length, from = min(length, na.rm = TRUE), to = max(length, na.rm = TRUE), 
                             length = 40)) %>% 
  group_by(strata, food_sale, cut_length) %>%
  summarize(weight_fs = sum(weight_calc)) %>% 
  group_by(strata, cut_length) %>% 
  mutate(weight_tot = sum(weight_fs),
         percent = (weight_fs/weight_tot) *100)

#percentage plot of size groups
foodsale_size %>% 
  mutate(food_sale = fct_relevel(food_sale, levels = c("Sold on market", "Own consumption", "Both"))) %>% 
  ggplot(aes(x=cut_length, y=weight_fs, fill = food_sale, group = cut_length)) +
  geom_col(position = "fill") + #geom_point() + 
  #geom_errorbar(aes(ymin = perc_mean - perc_se, ymax = perc_mean + perc_se), 
                #size=.3, position = position_dodge(0.1)) +
  facet_wrap(~strata) +
  theme_light() + theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 230, 30), guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  labs(x = "Length (cm)", y = "Percentage of catch (% kg)", fill = "") +
  scale_fill_brewer(palette = "Paired", na.value = "#FAA0A0", 
                    labels = c("Sold on market", "Own consumption", "Both", "Unknown")) 

ggsave(path = "figures/market", filename = "foodorsale_length_percent.png", width = 16, height = 10, units = "cm")

#total weight plot of size groups
foodsale_size %>% 
  mutate(food_sale = fct_relevel(food_sale, levels = c("Sold on market", "Own consumption", "Both"))) %>% 
  ggplot(aes(x=cut_length, y=weight_fs, fill = food_sale, group = cut_length)) +
  geom_col(position = "stack") + #geom_point() + 
  #geom_errorbar(aes(ymin = perc_mean - perc_se, ymax = perc_mean + perc_se), 
  #size=.3, position = position_dodge(0.1)) +
  facet_wrap(~strata, scale = "free_y") +
  theme_light() + theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(0, 230, 30), guide = guide_axis(angle = 45)) +
  labs(x = "Length (cm)", y = "Weight (kg)", fill = "") +
  scale_fill_brewer(palette = "Paired", na.value = "#FAA0A0", 
                    labels = c("Sold on market", "Own consumption", "Both", "Unknown")) 

ggsave(path = "figures/market", filename = "foodorsale_length_weight.png", width = 16, height = 10, units = "cm")


#plot destination of catch based on species (and functional groups)
all_data %>% 
  filter(weight_calc != 0) %>% 
  mutate(weight_calc = weight_calc/1000) %>% 
  mutate(food_sale = replace(food_sale, food_sale == "food", "Own consumption"),
         food_sale = replace(food_sale, food_sale == "sale", "Sold on market"),
         food_sale = replace(food_sale, food_sale == "both", "Both")) %>% 
  group_by(strata, functional_group, category, food_sale) %>%
  summarize(weight_fs = sum(weight_calc)) %>% 
  mutate(food_sale = fct_reorder(food_sale, weight_fs, .fun = median, .desc = TRUE)) %>% 
  ggplot(aes(x = category, y = weight_fs, fill = food_sale)) +
  geom_col(position = "fill") + coord_flip() +
  facet_grid(functional_group~., scales="free_y", space = "free") +
  theme(strip.text.y = element_text(angle = 0), legend.position="top") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), expand = c(0,0)) +
  #scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Destination of catch per species", 
       x = "Species", y = "Percentage (% kg)", fill = "Consumption or market") +
  scale_fill_brewer(palette = "Paired", na.value = "#FAA0A0", 
                    labels = c("Sold on market", "Own consumption", "Both", "Unknown")) 

ggsave(path = "figures/market", filename = "foodorsale_species_weight.png", width = 30, height = 20, units = "cm")

