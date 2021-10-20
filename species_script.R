#DATA FROM TRIPS_SCRIPT.R (need to run that one before starting this one)

#SPECIES
#IN THIS SCRIPT
#   - adding functional groups
#   - cleaning species and landing data
#   - adding missing catch weight data
#   - joining trips and landings df
#   - making graphs for species and functional group composition (also in relation to gear type)


##classify species into functional groups

spcies <- species %>% 
  mutate(functional_group = case_when(
    species_code %in%  c(1, 2, 5, 6, 7, 17, 23, 27, 32, 37, 48, 49) ~ "Small pelagics",
    species_code %in% c(3, 4, 9, 20, 22, 43, 44, 46, 51, 53) ~ "Large pelagics",
    species_code %in%  c(8, 11, 13, 14, 16, 19, 21, 25, 26, 28, 29, 30, 31, 33, 39, 40, 47, 52, 55) 
    ~ "Small demersal",
    species_code %in% c(10, 12, 15, 18, 35, 36, 42) ~ "Large demersal",
    species_code %in%  c(34, 54) ~ "Shark and rays",
    species_code %in% c(45) ~ "Crustaceans",
    species_code %in%  c(38) ~ "Shrimp",
    species_code %in% c(24, 41) ~ "Cephalopods",
    species_code %in%  c(50) ~ "Molluscs",
    species_code %in%  c(300, 999) ~ "Unknown",
    TRUE ~ NA_character_)) %>% 
  mutate(functional_group = functional_group, .before = species_code)

#join landings df with species df 
landings_spcies <- left_join(landings, spcies, by = "species_code")

#cleaning:
#   - change "nain rua" to both, as it is tetum for both (destination of catch)
#   - making negative weights positive (again assuming that is the case...)
#   - where number of fish is higher than 10,000 change it to NA (decided together with Jeppe)
#   - where number of fish is higher than 10,000 change the weight to NA as well (as it often appears to be calculated using the number of fish)

####       --> again, check appendix_script.R for the histograms and number of entries altered

lndngs_spcies <- landings_spcies %>% 
  mutate(nfish = abs(nfish),
         food_sale = recode(food_sale, 
                            "nain_rua"="both"),
         weight_g = abs(weight_g)) %>% 
  mutate(weight_g = if_else(nfish > 10000, NA_real_, weight_g),
         nfish = replace(nfish, nfish > 10000, NA_real_)) 


#quite a lot of NA's, can be calculated using W(g) = a * L^b (fishbase: https://www.fishbase.de/manual/fishbasethe_length_weight_table.htm)
#however, not all species (shrimp, cuttlefish, crab and cockles) have an a and b constant given in the dataset, so derive it in a different way:


#SHRIMP:
#to calculate shrimp, a and b constants from literature were used. A couple of different constants were tried from different articles, the one best matching the already existing data was used. 

#339788630_STUDY_OF_LENGTH_AND_WEIGHT_RELATIONSHIP_OF_LITOPENAEUS_VANNAMEI_BOONE_1931_FROM_EAST_COAST_OF_INDIA
#a = 0.0007 and b = 3.2605

lndngs_shrimp <- lndngs_spcies %>% 
  filter(species_code == 38) %>% 
  mutate(weight_individual = weight_g/nfish,
         a3 = 0.000007,
         b3 = 3.2605,
         weight3 = a3 * ((length*10)^b3)) %>% 
  mutate(weight_shrimp = weight3 * nfish)

lndngs_spcies3 <- left_join(lndngs_spcies, lndngs_shrimp, by = c('trip_id'='trip_id', 'species_code'='species_code', 'length'='length', "nfish" = "nfish"))
 
#CUTTLEFISH
#the weight of the cuttlefish was decided on the length-weight distribution found in the existing data
lndngs_cuttle <- lndngs_spcies %>% 
  filter(species_code == 41) %>% 
  mutate(weight_individual = weight_g/nfish) 

ggplot(lndngs_cuttle) +
  geom_line(data = lndngs_cuttle[!is.na(lndngs_cuttle$weight_individual),], aes(x = length, y = weight_individual)) +
  labs(y = "Weight (g)", x = "Length (cm)") + theme_light()

#ggsave(path = "figures/appendix/species", filename = "lenght_weight_cuttle.png", width = 19, height = 13, units = #"cm")


#CRAB
#same for crab as for cuttlefish
lndngs_crab <- lndngs_spcies %>% 
  filter(species_code == 45) %>% 
  mutate(weight_individual = weight_g/nfish) 

ggplot(lndngs_crab) +
  geom_line(data = lndngs_crab[!is.na(lndngs_crab$weight_individual),],aes(x = length, y = weight_individual)) +
  labs(y = "Weight (g)", x = "Length (cm)") + theme_light()

#ggsave(path = "figures/appendix/species", filename = "lenght_weight_crab.png", width = 19, height = 13, units = #"cm")


#COCKLES
#same for cockles as for cuttlefish and crab
lndngs_cockles <- lndngs_spcies %>% 
  filter(species_code == 50) %>% 
  mutate(weight_individual = weight_g/nfish) 

ggplot(lndngs_cockles) +
  geom_line(data = lndngs_cockles[!is.na(lndngs_cockles$weight_individual),],aes(x = length, y = weight_individual))+
  labs(y = "Weight (g)", x = "Length (cm)") + theme_light()

#ggsave(path = "figures/appendix/species", filename = "lenght_weight_cockles.png", width = 19, height = 13, units #= "cm")



#adding information of weight above to complete landings df
lndngs_spcies <- lndngs_spcies %>% 
  mutate(weight_ind = case_when(species_code == 38 ~ lndngs_spcies3$weight3,
                                species_code == 41 & length > 50 ~ 2600,
                                species_code == 41 & length < 50 & length > 40 ~ 1500,
                                species_code == 41 & length < 40 & length > 30 ~ 700,
                                species_code == 41 & length < 30 & length > 20 ~ 350,
                                species_code == 41 & length < 20 & length > 10 ~ 25,
                                species_code == 41 & length < 10 & length > 0 ~ 5,
                                species_code == 41 & is.na(length) ~ 350,
                                species_code == 45 ~ 50,
                                species_code == 50 & length == 7.5 ~ 6,
                                species_code == 50 & length == 12.5 ~ 60,
                                species_code == 50 & length == 17.5 ~ 100),
         weight_tot = (a * (length^b) * nfish),
         weight_tot2 = (weight_ind * nfish),
         weight_calc = if_else(is.na(weight_g), true = weight_tot, false = weight_g),
         weight_calc = if_else(is.na(weight_calc), true = weight_tot2, false = weight_calc)) %>% 
         mutate(weight_calc = weight_calc, .before = food_sale) 
#so weight_calc only fills the missing information of the original weight_g
#leftover NA's are due to missing lengths or nfish:
lndngs_spcies %>%
  filter(is.na(weight_calc)) %>% view

##MORE CLEANING
#there were some inconsistencies concerning the species code and the number of fish when there was no catch, or sometimes there was the code of no catch while there was catch.
#fixed this below:
#   - removed entries that had 0 fish, while on the same trip_id other fish were caught (so an entry that said that there was no catch, while there are other entries with the same trip_id that had catch)
#   - if nfish was 0, species code became 0 (and category "no catch")

lndngs_spcies2 <-  lndngs_spcies %>% 
  mutate(trip_id = as.factor(trip_id)) %>% 
  group_by(trip_id) %>% 
  mutate(species_code = if_else(n() == 1 & nfish == 0, true = 0L, false = species_code), 
         category = if_else(n() == 1 & nfish == 0, true = "No catch", false = as.character(category)),
         .after = species_code, 
         n = n()) %>% 
  ungroup() %>% 
  filter(!(nfish == 0 & n > 1)) 



###join trips df to landings df + some extra cleaning:
#   - removed catch value of sardines in south coast before July 2019, as it had crazy outliers
all_data <- inner_join(trps_stns, lndngs_spcies2, by = "trip_id") %>% 
  dplyr::select(-c(boat_id, boat_reg_no, owner, responsibility, fishbase, category_tetun, a, b, minlength, maxlength, nstudies, maxweight)) %>% #remove some unnecessary rows
  mutate(catch_value = if_else(strata == "South coast" & species_code == 6 & date < 2019-07-01, NA_real_, catch_value)) %>% 
  mutate(date = ymd(date))

#how many entries are lost?
anti_join(trps_stns, lndngs_spcies2, by = "trip_id") %>% view #121 trip entries with trip ID that doesn't exist in landing df. That means some trips have not noted their landings.
anti_join(lndngs_spcies2, trps_stns, by = "trip_id") %>% view #0 landing entries with trip ID that doesn't exist in trip df


###FUNCTIONAL GROUP/SPECIES COMPOSITIONS GRAPHS

fg_composition <- all_data %>% 
  filter(!is.na(functional_group)) %>% 
  group_by(trip_id, strata, functional_group) %>% 
  summarize(sumabundance = sum(nfish, na.rm = TRUE), sumweight = sum(weight_calc, na.rm = TRUE), .groups = "drop_last") %>% 
  group_by(strata) %>% 
  mutate(ntrip = n_distinct(trip_id)) %>% 
  group_by(strata, functional_group) %>% 
  summarize(sumabundance = sum(sumabundance, na.rm = TRUE), sumweight = sum(sumweight, na.rm = TRUE), n = (n()/ntrip[1]*100), .groups = "drop_last") %>% 
  group_by(strata) %>% 
  mutate(per_abundance = (sumabundance/sum(sumabundance, na.rm = TRUE) * 100), per_weight = (sumweight/sum(sumweight, na.rm = TRUE)*100)) %>% 
  arrange(sumweight) %>% 
  mutate(x_end = cumsum(n), x_start = cumsum(c(0,n)[-(n()+1)])) %>% 
  ungroup() %>% 
  mutate(functional_group = fct_relevel(functional_group, c("Small pelagics", "Large pelagics", "Small demersal", "Large demersal", "Unknown", "Cephalopods", "Shrimp", "Shark and rays", "Molluscs", "Crustaceans"))) %>% 
  pivot_longer(c(per_abundance, per_weight)) %>% 
  mutate(value = if_else(name == "per_weight",
                         true = value,
                         false = -1 * value))


fg_composition_AI <- fg_composition %>% 
  filter((strata == "Atauro island")) 
fg_composition_SC <- fg_composition %>% 
  filter((strata == "South coast")) 
fg_composition_NC <- fg_composition %>% 
  filter((strata == "North coast")) 
breaks <- seq(20, 100, 20)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

library(grid)
ggplot(fg_composition_AI, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = functional_group)) +
  geom_rect(colour = "black") +
  #geom_text_repel(aes(label = functional_group, y = 0, x = (x_start + x_end)/2), data = fg_composition_AI %>%
                   # filter(value > 0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  scale_x_continuous(breaks = (fg_composition_AI$x_start + fg_composition_AI$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(fg_composition_AI$n, 0)), '%')) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-18.5, xmax=-18.5,ymin=-30, ymax=-30) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-18.5, xmax=-18.5,ymin=25, ymax=25) +
  labs(title = "Atauro Island", x = "FRQ of occurrence", fill = "Functional group", y = " ") + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Species", filename = "IRI_AI.png", width = 17, height = 9.5, units = "cm")

ggplot(fg_composition_NC, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = functional_group)) +
  geom_rect(colour = "black") +
 # geom_text_repel(aes(label = functional_group, y = 0, x = (x_start + x_end)/2), data = fg_composition_NC %>%
                    #filter(value > 0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                      labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  scale_x_continuous(breaks = (fg_composition_NC$x_start + fg_composition_NC$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(fg_composition_NC$n, 0)), '%')) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-16, xmax=-16,ymin=-50, ymax=-50) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-16, xmax=-16,ymin=30, ymax=30) +
  labs(title = "North coast", x = "FRQ of occurrence", fill = "Functional group", y = " ")  + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Species", filename = "IRI_NC.png", width = 17, height = 9.5, units = "cm")


ggplot(fg_composition_SC, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = functional_group)) +
  geom_rect(colour = "black") +
  #geom_text_repel(aes(label = functional_group, y = 0, x = (x_start + x_end)/2), data = fg_composition_SC %>%
                   # filter(value > 0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  scale_x_continuous(breaks = (fg_composition_SC$x_start + fg_composition_SC$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(fg_composition_SC$n, 0)), '%')) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-17, xmax=-17,ymin=-50, ymax=-50) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-17, xmax=-17,ymin=25, ymax=25) +
  labs(title = "South coast", x = "FRQ of occurrence", fill = "Functional group", y = " ")  + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Species", filename = "IRI_SC.png", width = 17, height = 9.5, units = "cm")

###FUNCTIONAL GROUP COMPOSITION OVER TIME
#calculate Index of Relative Importance (IRI) (Jeppe Kolding, 1989)
# %IRI = (((%Wi + %Ni) * %Fi) / (sum(%Wj + %Nj) * %Fj)) * 100
#where %W i and %N i is percentage weight and number of each species of total catch, %F i is percentage
#frequency of occurrence of each species in total number of fishing operations, and S is total number of
#species. This index can be either tabulated as a percentage or displayed as a rectangle, which show
#simultaneously the relative numeric abundance (N), the relative weight (W) and the commonness (F)
#of a species relative to all the other species present.

FG_IRI <- all_data %>%
  filter(!is.na(functional_group)) %>% 
  group_by(date = floor_date(date, "6 month"), strata) %>%
  mutate(ntrip = n_distinct(trip_id)) %>% 
  group_by(date = floor_date(date, "6 month"), strata, functional_group) %>%
  summarize(Wi = sum(weight_calc, na.rm = TRUE),
            Ni = sum(nfish, na.rm = TRUE),
            p_Fi = (n()/ntrip[1]*100)) %>% 
  group_by(date = floor_date(date, "6 month"), strata) %>%
  mutate(sum_Wi = sum(Wi, na.rm = TRUE),
         sum_Ni = sum(Ni, na.rm = TRUE)) %>% 
  mutate(p_Wi = (Wi/sum_Wi)*100,
         p_Ni = (Ni/sum_Ni)*100) %>% 
  mutate(check_Wi = sum(p_Wi),
         check_Ni = sum(p_Ni)) %>% 
  mutate(IRI1 = (p_Wi + p_Ni)*p_Fi) %>% 
  group_by(date = floor_date(date, "6 month"), strata) %>% 
  mutate(IRI2 = sum(IRI1)) %>% 
  mutate(IRI3 = (IRI1/IRI2)*100) %>% 
  group_by(date = floor_date(date, "6 month"), strata) %>%
  mutate(check = sum(IRI3)) %>% 
  mutate(functional_group = fct_reorder(functional_group, IRI3, .fun = median, .desc = TRUE))

ggplot(FG_IRI) +
  geom_col(aes(x = date, fill = as.factor(functional_group), y = IRI3), 
           position = "stack", size = 0.2, col = "black") + facet_wrap(~strata) +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs( 
       x = "", y = "IRI", fill = "Functional group") +
  theme_light() + scale_fill_brewer(palette = "Paired") +
  scale_x_date(date_labels = "%Y", guide = guide_axis(angle = 45)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

ggsave(path = "figures/Species", filename = "FG_overtime.png", width = 18, height = 11, units = "cm")


##LENGTH PER FUNCTIONAL GROUP

#make function for variance bars in plot
median_IQR <- function(x) {
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4])  # 3rd quartile
}


all_data %>% 
  filter(!is.na(functional_group)) %>% 
  filter(!(functional_group == "Small pelagics" & length > 30)) %>% #for scaling of plot
  filter(!(functional_group == "Unknown" & length > 75)) %>% #for scaling of plot
  mutate(functional_group = fct_relevel(functional_group, c("Small pelagics", "Large pelagics", "Small demersal", "Large demersal",  "Cephalopods", "Crustaceans", "Molluscs", "Shark and rays", "Shrimp",  "Unknown"))) %>% 
ggplot() +
  theme_light() + 
  geom_violin(aes(x = strata, y = length, fill = strata)) +
  #geom_boxplot(aes(x = as.factor(strata), y = length), width = 0.1) +
  stat_summary(aes(x = strata, y = length), fun.data = median_IQR, #cut of at zero
               geom="point", size = 1.4) +
  stat_summary(aes(x = strata, y = length),
               fun.data = median_IQR, geom = "errorbar", width = .4) +
  labs( y = "Length (cm)", fill = "Strata") + 
  facet_wrap(~functional_group, scales = "free_y") +
  scale_x_discrete(guide = guide_axis(angle = 45)) + scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "top", axis.title.x = element_blank())

ggsave(path = "figures/Species", filename = "FG_length_violinpointrange.png", width = 18, height = 15, units = "cm")


#species composition per functional group
species_composition <- all_data %>%
  filter(species_code != "0") %>% 
  group_by(strata, functional_group) %>%
  mutate(ntrip = n_distinct(trip_id)) %>% 
  group_by(strata, functional_group, category) %>%
  summarize(Wi = sum(weight_calc, na.rm = TRUE),
            Ni = sum(nfish, na.rm = TRUE),
            p_Fi = (n()/ntrip[1]*100)) %>% 
  group_by(strata, functional_group) %>% 
  mutate(sum_Wi = sum(Wi, na.rm = TRUE),
         sum_Ni = sum(Ni, na.rm = TRUE)) %>% 
  mutate(p_Wi = (Wi/sum_Wi)*100,
         p_Ni = (Ni/sum_Ni)*100) %>% 
  mutate(check_Wi = sum(p_Wi),
         check_Ni = sum(p_Ni)) %>% 
  mutate(IRI1 = (p_Wi + p_Ni)*p_Fi) %>% 
  mutate(IRI2 = sum(IRI1)) %>% 
  mutate(IRI3 = (IRI1/IRI2)*100) %>% 
  group_by(strata, functional_group) %>%
  mutate(check = sum(IRI3))

 
species_composition_LP <- species_composition %>% 
  filter(functional_group == "Large pelagics") %>% 
  mutate(category = fct_reorder(category, IRI3, .fun = median, .desc = TRUE))

species_composition_SP <- species_composition %>% 
  filter(functional_group == "Small pelagics") %>% 
  mutate(category = fct_reorder(category, IRI3, .fun = median, .desc = TRUE))

species_composition_LD <- species_composition %>% 
  filter(functional_group == "Large demersal") %>% 
  mutate(category = fct_reorder(category, IRI3, .fun = median, .desc = TRUE))

species_composition_SD <- species_composition %>% 
  filter(functional_group == "Small demersal") %>% 
  mutate(category = fct_reorder(category, IRI3, .fun = median, .desc = TRUE))
  
c25 <- c(
  "#A6CEE3", "#1F78B4",
  "#B2DF8A", "#33A02C", 
  "#FB9A99", "#E31A1C", 
  "#FDBF6F", "#FF7F00", 
  "#CAB2D6", "#6A3D9A", 
  "#FFFF99", "#B15928", 
  "black", "gold1",
  "#F2F2F2", "#FED9A6", "#E5D8BD" 
)

ggplot(species_composition_LP) +
  geom_col(aes(x = strata, fill = category, y = IRI3),
           position = "stack", size = 0.2,  col = "black") +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(title = "Species composition of large pelagics", 
       x = "", y = "IRI", fill = "Species") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light() + scale_fill_brewer(palette = "Paired") 

ggsave(path = "figures/Species", filename = "SC_LP.png", width = 20, height = 12, units = "cm")


ggplot(species_composition_SP) +
  geom_col(aes(x = strata, fill = category, y = IRI3),
           position = "stack", size = 0.2,  col = "black") +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(title = "Species composition of small pelagics", 
       x = "", y = "IRI", fill = "Species") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light() + scale_fill_brewer(palette = "Paired") 

ggsave(path = "figures/Species", filename = "SC_SP.png", width = 20, height = 11, units = "cm")


ggplot(species_composition_LD) +
  geom_col(aes(x = strata, fill = category, y = IRI3),
           position = "stack", size = 0.2,  col = "black") +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(title = "Species composition of large demersal", 
       x = "", y = "IRI", fill = "Species") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light() + scale_fill_brewer(palette = "Paired") 

ggsave(path = "figures/Species", filename = "SC_LD.png", width = 20, height = 11, units = "cm")


ggplot(species_composition_SD) +
  geom_col(aes(x = strata, fill = category, y = IRI3),
           position = "stack", size = 0.2,  col = "black") +
  #geom_text(aes(label = totalcount,
  #y= totalpercent, x = date), vjust = -.5) + #make second plot
  labs(title = "Species composition of small demersal", 
       x = "", y = "IRI", fill = "Species") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light() + scale_fill_manual(values = c25)

ggsave(path = "figures/Species", filename = "SC_SD.png", width = 20, height = 13, units = "cm")


library(tidyverse)

###IRI OF FUNCTIONAL GROUPS PER GEAR TYPE
#show species composition per strata (and municipality?) What is the best way?
fg_composition_gear <- all_data %>% 
  filter(!is.na(functional_group),
         gear_code == "1" | gear_code == "2" | gear_code == "3" | gear_code == "4") %>% 
  group_by(trip_id, gear_code, functional_group) %>% 
  summarize(sumabundance = sum(nfish, na.rm = TRUE), sumweight = sum(weight_calc, na.rm = TRUE), .groups = "drop_last") %>% 
  group_by(gear_code) %>% 
  mutate(ntrip = n_distinct(trip_id)) %>% 
  group_by(gear_code, functional_group) %>% 
  summarize(sumabundance = sum(sumabundance, na.rm = TRUE), sumweight = sum(sumweight, na.rm = TRUE), n = (n()/ntrip[1]*100), .groups = "drop_last") %>% 
  group_by(gear_code) %>% 
  mutate(per_abundance = (sumabundance/sum(sumabundance, na.rm = TRUE) * 100), per_weight = (sumweight/sum(sumweight, na.rm = TRUE)*100)) %>% 
  arrange(sumweight) %>% 
  mutate(x_end = cumsum(n), x_start = cumsum(c(0,n)[-(n()+1)])) %>% 
  ungroup() %>% 
  mutate(functional_group = fct_relevel(functional_group, c("Small pelagics", "Large pelagics", "Small demersal", "Large demersal", "Unknown", "Cephalopods", "Shrimp", "Shark and rays", "Molluscs", "Crustaceans"))) %>% 
  pivot_longer(c(per_abundance, per_weight)) %>% 
  mutate(value = if_else(name == "per_weight",
                         true = value,
                         false = -1 * value))



fg_composition1 <- fg_composition_gear %>% 
  filter((gear_code == "1")) 
fg_composition2 <- fg_composition_gear %>% 
  filter((gear_code == "2")) 
fg_composition3 <- fg_composition_gear %>% 
  filter((gear_code == "3")) 
fg_composition4 <- fg_composition_gear %>% 
  filter((gear_code == "4")) 
breaks <- seq(20, 100, 20)


specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

library(grid)
ggplot(fg_composition1, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = functional_group)) +
  geom_rect(colour = "black") +
  #geom_text_repel(aes(label = functional_group, y = 0, x = (x_start + x_end)/2), data = fg_composition_AI %>%
  # filter(value > 0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  scale_x_continuous(breaks = (fg_composition1$x_start + fg_composition1$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(fg_composition1$n, 0)), '%')) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-18, xmax=-18,ymin=-30, ymax=-30) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-18, xmax=-18,ymin=25, ymax=25) +
  labs(title = "Gill net", x = "FRQ of occurrence", fill = "Functional group", y = " ") + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Species/gear", filename = "IRI_GN.png", width = 17, height = 9.5, units = "cm")

ggplot(fg_composition2, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = functional_group)) +
  geom_rect(colour = "black") +
  # geom_text_repel(aes(label = functional_group, y = 0, x = (x_start + x_end)/2), data = fg_composition_NC %>%
  #filter(value > 0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  scale_x_continuous(breaks = (fg_composition2$x_start + fg_composition2$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(fg_composition2$n, 0)), '%')) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-18, xmax=-18,ymin=-40, ymax=-40) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-18, xmax=-18,ymin=20, ymax=20) +
  labs(title = "Hand line", x = "FRQ of occurrence", fill = "Functional group", y = " ")  + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Species/gear", filename = "IRI_HL.png", width = 17, height = 9.5, units = "cm")


ggplot(fg_composition3, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = functional_group)) +
  geom_rect(colour = "black") +
  #geom_text_repel(aes(label = functional_group, y = 0, x = (x_start + x_end)/2), data = fg_composition_SC %>%
  # filter(value > 0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  scale_x_continuous(breaks = (fg_composition3$x_start + fg_composition3$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(fg_composition3$n, 0)), '%')) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-18.5, xmax=-18.5,ymin=-50, ymax=-50) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-18.5, xmax=-18.5,ymin=25, ymax=25) +
  labs(title = "Long line", x = "FRQ of occurrence", fill = "Functional group", y = " ")  + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Species/gear", filename = "IRI_LL.png", width = 17, height = 9.5, units = "cm")

ggplot(fg_composition4, aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = value, fill = functional_group)) +
  geom_rect(colour = "black") +
  #geom_text_repel(aes(label = functional_group, y = 0, x = (x_start + x_end)/2), data = fg_composition_SC %>%
  # filter(value > 0)) +
  coord_flip(clip = "off") +
  scale_y_continuous(breaks = c(-breaks, 0, breaks),
                     labels = function(x) paste0(c(breaks, 0, breaks), "%")) +
  scale_x_continuous(breaks = (fg_composition4$x_start + fg_composition4$x_end)/2, 
                     labels = function(x) paste0((specify_decimal(fg_composition4$n, 0)), '%')) +
  annotation_custom(textGrob("% No.", gp = gpar(col = "black")), 
                    xmin=-24.5, xmax=-24.5,ymin=-40, ymax=-40) +
  annotation_custom(textGrob("% Weight", gp = gpar(col = "black")), 
                    xmin=-24.5, xmax=-24.5,ymin=25, ymax=25) +
  labs(title = "Spear gun", x = "FRQ of occurrence", fill = "Functional group", y = " ")  + scale_fill_brewer(palette = "Paired")

ggsave(path = "figures/Species/gear", filename = "IRI_SG.png", width = 17, height = 9.5, units = "cm")


