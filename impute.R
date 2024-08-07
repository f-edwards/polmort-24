library(tidyverse)

set.seed(5)

mpv<-read_csv("./data/mpv-4-2-24.csv")

### magnitudes of missing data on core demographics (age, gender, race, date)
mpv<-mpv %>% 
  mutate(
    date = mdy(date),
    year = year(date),
    month = month(date)) %>% 
  select(name:cause_of_death, tract, latitude, longitude, date, year, month)

pop<-read_csv("./data/pop_st.csv")
xwalk<-data.frame(stname = state.name, 
                  state = state.abb)

pop<- pop %>% 
  left_join(xwalk) %>% 
  mutate(sex = ifelse(sex == 1,
                      "Male", 
                      "Female"))

### setup racial comp by state
pop_st_race_comp<-pop %>% 
  group_by(year, state, race_ethn) %>% 
  summarize(pop = sum(pop)) %>% 
  pivot_wider(names_from = race_ethn, 
              values_from = pop) %>% 
  left_join(pop %>% 
              group_by(year, state) %>% 
              summarize(pop = sum(pop))) %>% 
  mutate(across(aian:white, ~./pop * 1e2)) %>% 
  mutate(year = year + 2) %>%   # for imputation match
  select(-white)

### race
table(mpv$race)

# use of force eda --------------------------------------------------------
table(mpv$cause_of_death)

# inclusion criteria ------------------------------------------------------
# mpv includes all deaths where police action caused death
# e.g. vehicles appear to require collision with police vehicle
# suicides appear to require police use of force
# audit these carefully
# suicide -----------------------------------------------------------------
# # suicide
# table(str_detect(mpv$circumstances, "suici"))
# 
# ### evaluate these more carefully later
# ### for now just drop
# 
# suic<-mpv %>% 
#   filter(str_detect(circumstances, "suici"))
# 
# mpv<-mpv %>% 
#   filter(!(str_detect(circumstances, "suici")))

# vehicle -----------------------------------------------------------------
# v<-mpv %>% 
#   filter(cause_of_death=="Vehicle") %>% 
#   select(circumstances)

### try imputations
library(mice)

mpv<-mpv %>% 
  mutate(race = ifelse(race=="Unknown race", 
                       NA, 
                       race),
         gender = ifelse(gender == "Unknown",
                         NA,
                         gender)) 


mpv_imps<-mpv %>% 
  select(date, year, age, gender, race, state) %>% 
  left_join(pop_st_race_comp %>% 
              select(-pop)) %>% 
  mutate(race = factor(race),
         gender = factor(gender))

imps_temp <- mice(mpv_imps, 
                  m = 20,
                  maxit = 20)

mpv_imputed<-complete(imps_temp, 
                      action = "long")

### output
write_csv(mpv_imputed %>% 
            select(.imp:state), "./data/mpv_imputed.csv")


# EDA on imputed ----------------------------------------------------------

t<-complete(imps_temp, "long", include=T)
### visualize imputed
t %>% 
  group_by(.imp) %>% 
  summarize(age_mn = mean(age, na.rm=T),
            age_sd = sd(age, na.rm=T))

t %>% 
  group_by(.imp, race) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = .imp)) + 
  geom_col() + 
  facet_wrap(~race, scales = "free")

t %>% 
  group_by(.imp, race) %>% 
  summarize(prop = n()/nrow(mpv_imps))%>% 
  ggplot(aes(y = prop, x = .imp)) + 
  geom_col() + 
  facet_wrap(~race, scales = "free")
### compare to observed


