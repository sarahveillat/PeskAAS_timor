#DATA FROM TRIPS_SCRIPT.R AND SPECIES_SCRIPT.R AND PDS_SCRIPT.R (need to run those before starting this one)

#MODELS TO TEST COVID19 EFFECT
#IN THIS SCRIPT
#   - model with dependent variable "number of trips taken per week per boat"
#   - model with dependent variable "total monthly landings"
#   - model with dependent variable "market value per kg catch"


library(MuMIn)
library(emmeans)
library(nlme)
library(lme4)
library(rsq)
if(!require(psych)){install.packages("psych")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(boot)){install.packages("boot")}
if(!require(rcompanion)){install.packages("rcompanion")}

if(!require(psych)){install.packages("rootSolve", type="binary")}

###the case for all models:
#making df for models:
#   - only choose two years, year before lockdown and year during lockdown
#   - make column for yes or no lockdown
#all models need to have independent variable lockdown stage
#model selection based on:
#   - AICc
#   - AICc weight
#   - pseudo R-squared

#NUMBER OF TRIPS PER WEEK PER BOAT
#make df for model
PDS_model <- left_join(PDS_trips, boats_tracked2_week, by = c("week" = "week", "strata" = "strata")) %>% 
  left_join(total_boats, by = "strata") %>% 
  mutate(total_trips = (n*100/per_tracked),
         trips_boat = total_trips/boats) %>%
  mutate(lockdown_stage = case_when(
    week >= "2020-03-28" ~ 1,
    TRUE ~ 0),
    lockdown_stage = as.factor(lockdown_stage)) %>% 
  mutate(season = case_when(
    month(week) == 12 | month(week) < 5 ~ "Wet season",
    TRUE ~ "Dry season")) %>% 
  filter(week <= "2021-03-28" & week >= "2019-03-28") %>% 
  group_by(week, strata) %>% 
  mutate(lag1 = lag(week, k = 1))

#show distribution
ggplot(PDS_model) +
  geom_histogram(aes(x = trips_boat), bins = 50, col = "black", fill = "grey") +
  labs(x = "Trips per week per boat")

ggsave(path = "figures/models/hist", filename = "pds_hist.png", width = 15, height = 10, units = "cm") 

#show log distribuition
ggplot(PDS_model) +
  geom_histogram(aes(x = log10(trips_boat)), bins = 50, col = "black", fill = "grey") +
  labs(x = "Log10(Trips per week per boat)")
  
ggsave(path = "figures/models/hist", filename = "pds_hist_log.png", width = 15, height = 10, units = "cm")

#trying different models
null_PDS <- glm(log10(trips_boat)  ~ 1, data = PDS_model)
model_PDS3 <- gls(log10(trips_boat)  ~ strata + lockdown_stage, data = PDS_model, cor = corAR1())
model_PDS1 <- glm(log10(trips_boat)  ~ strata + lockdown_stage, data = PDS_model)
model_PDS2 <- gls(log10(trips_boat)  ~ strata * lockdown_stage, data = PDS_model, cor = corAR1())
model_PDS <- glm(log10(trips_boat)  ~ lockdown_stage, data = PDS_model)


nagelkerke(model_PDS3)
nagelkerke(model_PDS2)
nagelkerke(model_PDS1)
nagelkerke(model_PDS)
nagelkerke(null_PDS)

AICc(null_PDS, model_PDS, model_PDS1, model_PDS2, model_PDS3)
as.data.frame(Weights(AICc(null_PDS, model_PDS, model_PDS1, model_PDS2, model_PDS3)))

#chose model 3
summary(model_PDS3)
anova(model_PDS3)

#rearrange strata
model_PDS4 <- PDS_model %>% 
  mutate(strata = fct_relevel(strata, c("North coast", "Atauro island", "South coast"))) %>% 
  gls(log10(trips_boat)  ~ strata + lockdown_stage, data = ., cor = corAR1())

summary(model_PDS4)

#visualise model (transform back)
PDS_em <- as.data.frame(emmeans(model_PDS3, ~ lockdown_stage | strata)) %>% 
  mutate(mean = 10^(emmean))

ggplot() +
  theme_light() + facet_wrap(~strata) +
  geom_jitter(data = PDS_model, aes(y = trips_boat, x = lockdown_stage), 
              alpha = .8, size = 1, col = "grey") +
  geom_point(data = PDS_em, aes(y = 10^(emmean), x = lockdown_stage, col = lockdown_stage),size = 5, shape = "triangle") +
  geom_errorbar(data = PDS_em, aes(ymax = 10^(upper.CL), ymin = 10^(lower.CL), x = lockdown_stage), width = 0.1) +
  scale_x_discrete(labels = c("Pre", "Post")) +
  labs(y = "No. of trips per week per boat", x = "Lockdown") + theme(legend.position = "none")
#scale_y_continuous(limits = c(0, 30)) 

ggsave(path = "figures/models", filename = "model_pds.png", width = 15, height = 10, units = "cm")


##LANDINGS
#make df for model
landings_model <- monthly_landings %>% 
  filter(date <= "2021-03-28" & date >= "2019-03-28") %>% 
  mutate(month = format(date, "%b"),
         lockdown_stage = case_when(
           date >= "2020-03-28"  ~ "1",
           TRUE ~ "0"), 
         lockdown_stage = as.factor(lockdown_stage)) %>% 
  mutate(season = case_when(
    month(date) == 12 | month(date) < 5 ~ "Wet season",
    TRUE ~ "Dry season"))

#show distribution
ggplot(landings_model) +
  geom_histogram(aes(x = catch_tons), bins = 50, col = "black", fill = "grey") +
  labs(x = "Monthly landings (tons)")

ggsave(path = "figures/models/hist", filename = "landings_hist.png", width = 15, height = 10, units = "cm") 

#show log distribution
ggplot(landings_model) +
  geom_histogram(aes(x = log10(catch_tons)), bins = 50, col = "black", fill = "grey") +
  labs(x = "Log10(Monthly landings (tons))")

ggsave(path = "figures/models/hist", filename = "landings_hist_log.png", width = 15, height = 10, units = "cm")

#trying different models
model_landings1 <- glm(log10(catch_tons) ~ lockdown_stage, data = landings_model)
model_landings2 <- glm(log10(catch_tons) ~  strata + lockdown_stage, data = landings_model)
model_landings3 <- glm(log10(catch_tons) ~  strata * lockdown_stage, data = landings_model)
null_landings <- glm(log10(catch_tons) ~ 1 ,data = landings_model)

#model selection
AICc(null_landings, model_landings1, model_landings2, model_landings3)
as.data.frame(Weights(AICc(null_landings, model_landings1, model_landings2, model_landings3)))
r.squaredLR(null_landings)
r.squaredLR(model_landings1)
r.squaredLR(model_landings2)
r.squaredLR(model_landings3)

anova(model_landings1, null_landings, test = "Chisq")
anova(model_landings2, null_landings, test = "Chisq")
anova(model_landings3, null_landings, test = "Chisq")

#only small difference between model 2 and 3 according to above criteria
anova(model_landings2, model_landings3, test = "Chisq")
#no significant difference according to above anova, so choosing simplest model, thus model 2

anova(model_landings2)
summary(model_landings2)

#rearrange strata
landings_model %>% 
  mutate(strata = fct_relevel(strata, c("South coast", "Atauro island", "North coast"))) %>% 
  lm(log10(catch_tons) ~  strata + lockdown_stage, data = .) %>% summary()

#calculate differences 
(10^(0.10381) / 10^(1.57069)) * 100
(10^(0.63174) / 10^(1.57069)) * 100
((10^(0.37118)) / (10^(1.19951))) * 100

#visualise model (transform back)
landings_em <- as.data.frame(emmeans(model_landings2, ~ lockdown_stage | strata))%>% 
  mutate(mean = 10^(emmean),
         UCL = 10^(asymp.UCL),
         LCL = 10^(asymp.LCL))

ggplot() +
  theme_light() + facet_wrap(~strata, scales = "free_y") +
  geom_jitter(data = landings_model, aes(y = catch_tons, x = lockdown_stage), 
              alpha = .8, size = 1, col = "grey") +
  geom_point(data = landings_em, aes(y = 10^(emmean), x = lockdown_stage, color = lockdown_stage),size = 5, shape = "triangle") +
  geom_errorbar(data = landings_em, aes(ymax = 10^(asymp.UCL), ymin = 10^(asymp.LCL), x = lockdown_stage), position = position_dodge(width=1), width = 0.1, col = "black") + 
  scale_x_discrete(labels = c("Pre", "Post")) +
  labs(y = "Catch (tons) per month", x = "Lockdown") + theme(legend.position = "none") 

ggsave(path = "figures/models", filename = "model_landings.png", width = 15, height = 10, units = "cm")


#CATCH VALUE
#make df for model
#removed some gear codes and station codes that have a very low sample count to keep model more balanced
value_model <- catchvalue %>%
  filter(date >= "2019-07-01") %>% 
  filter(catch_value != 0) %>% 
  filter(gear_code == 1 | gear_code == 2 | gear_code == 3 | gear_code == 4) %>% 
  filter(habitat_code == 1 | habitat_code == 2 | habitat_code == 3 | habitat_code == 4) %>% 
  filter(boat_code == 1 | boat_code == 2) %>% 
  filter(station_code != 20 & station_code != 30 & station_code != 13 & station_code != 5 & station_code != 9) %>%
  filter(date <= "2021-03-28" & date >= "2019-03-28") %>% 
  mutate(lockdown_stage = case_when(
    date >= "2020-03-28"  ~ "1",
    TRUE ~ "0"),
    lockdown_stage = as.factor(lockdown_stage))

#show distribution
ggplot(value_model) +
  geom_histogram(aes(x = value_per_kg), binwidth = 1, col = "black", fill = "grey") +
  labs(x = "Price (USD) per kg") + coord_cartesian(xlim = c(0,50))

ggsave(path = "figures/models/hist", filename = "value_hist.png", width = 15, height = 10, units = "cm") 

#show log distribution
ggplot(value_model) +
  geom_histogram(aes(x = log10(value_per_kg)), bins = 50, col = "black", fill = "grey") +
  labs(x = "Log10(Price (USD) per kg)")

ggsave(path = "figures/models/hist", filename = "value_hist_log.png", width = 15, height = 10, units = "cm")

#choose model
#boat and gear have some interaction so only choose one or the other
null_value <- glmer(log10(value_per_kg) ~  (1 | station_code), 
                      data = value_model)
model_value <- glmer(log10(value_per_kg) ~  lockdown_stage +  (1 | station_code), 
                      data = value_model)
model_value1 <- glmer(log10(value_per_kg) ~  gear_code + lockdown_stage +  (1 | station_code), 
                      data = value_model)
model_value2 <- glmer(log10(value_per_kg) ~  boat_code + lockdown_stage +  (1 | station_code), 
                      data = value_model)
model_value3 <- glmer(log10(value_per_kg) ~  gear_code + strata + lockdown_stage +  (1 | station_code), 
                      data = value_model)
model_value4 <- glmer(log10(value_per_kg) ~  gear_code * lockdown_stage +  (1 | station_code), 
                      data = value_model)

model_value5 <- MASS::glmmPQL(log10(value_per_kg) ~  gear_code * lockdown_stage, random = ~ 1 | station_code, 
                     data = value_model, family = gaussian)

#model selection
AICc(null_value, model_value, model_value1, model_value2, model_value3, model_value4)
as.data.frame(Weights(AICc(null_value, model_value, model_value1, model_value2, model_value3, model_value4)))

r.squaredGLMM(null_value)
r.squaredGLMM(model_value)
r.squaredGLMM(model_value1)
r.squaredGLMM(model_value2)
r.squaredGLMM(model_value3)
r.squaredGLMM(model_value4)

anova(model_value, null_value, test = "Chisq")
anova(model_value1, model_value2, test = "Chisq")
anova(model_value3, model_value1, test = "Chisq")
anova(model_value4, model_value3, test = "Chisq")
anova(model_value4, model_value1, test = "Chisq")
#ending up choosing model 4

summary(model_value4)
anova(model_value4)

#rearranging gears
value_model %>% 
  mutate(gear_code = fct_relevel(gear_code, c("4", "1", "2", "3"))) %>% 
  MASS::glmmPQL(log10(value_per_kg) ~  gear_code * lockdown_stage, random = ~ 1 | station_code, 
                data = ., family = gaussian) %>% 
  summary()

#visualise model (transform back)
value_em <- as.data.frame(emmeans(model_value4, ~ lockdown_stage | gear_code))

gear.labs <- c("Gill net", "Hand line", "Long line", "Spear gun")
names(gear.labs) <- c("1", "2","3", "4")

ggplot() +
  theme_light() + facet_wrap(~gear_code, labeller = labeller(gear_code = gear.labs), scales = "free_y") +
  geom_jitter(data = value_model, aes(y = value_per_kg, x = lockdown_stage), 
              alpha = .8, size = 1, col = "grey") +
  geom_point(data = value_em, aes(y = 10^(emmean), x = lockdown_stage, col = lockdown_stage),size = 5, shape = "triangle") +
  geom_errorbar(data = value_em, aes(ymax = 10^(asymp.UCL), ymin = 10^(asymp.LCL), x = lockdown_stage), position = position_dodge(width=1), width = 0.1, col = "black") + 
  scale_x_discrete(labels = c("Pre", "Post")) +
  labs(y = "Price ($) per kg", x = "Lockdown") + theme(legend.position = "none") +
 coord_cartesian(ylim = c(0, 10)) 

ggsave(path = "figures/models", filename = "model_value.png", width = 13, height = 10, units = "cm")
