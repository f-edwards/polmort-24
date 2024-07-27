library(tidyverse)

# read MPV ----------------------------------------------------------------
mpv<-read_csv("./data/mpv_imputed.csv")

### magnitudes of missing data on core demographics (age, gender, race, date)
mpv<-mpv %>% 
  mutate(
    date = ymd(date),
    year = year(date)) %>% 
  mutate(race_ethn = 
           case_when(race == "Black" ~ "black",
                     race == "White" ~ "white",
                     race == "Hispanic" ~ "hispanic",
                     race == "Native Hawaiian and Pacific Islander" ~ "api",
                     race == "Native American" ~ "aian",
                     race == "Asian" ~ "api")) %>% 
  select(-race) %>% 
  filter(gender %in% c("Male", "Female"))

mpv_yr<-mpv %>% 
  mutate(age = case_when(
    age == 0 ~ 0,
    age < 5 ~ 1,
    age >= 80 ~ 80,
    T ~ plyr::round_any(age, 5)
  )) %>% 
  group_by(.imp, year, age, gender, race_ethn) %>% 
  summarize(n=n()) %>% 
  ungroup() 
# manually insert a 0 value for 0 age, then complete to populate zeroes
mpv_yr<-mpv_yr %>% 
  bind_rows(data.frame(.imp=1, year =2013, age = 0, gender = "Male", race_ethn = "black", n = 0)) %>% 
  tidyr::complete(.imp, year, age, gender, race_ethn, 
                  fill = list(n = 0))

# # read SEER ---------------------------------------------------------------
# pop<-read_csv("./data/pop_st.csv")
# xwalk<-data.frame(stname = state.name, 
#                   state = state.abb)
# pop_st<- pop %>% 
#   left_join(xwalk) %>% 
#   mutate(gender = ifelse(sex == 1,
#                          "Male", 
#                          "Female"))%>% 
#   mutate(age = as.numeric(age)) %>% 
#   mutate(
#     age = case_when(
#       age == 0 ~ 0,
#       age < 5 ~ 1,
#       age>85 ~ 85,
#       T ~ plyr::round_any(age, 5)
#     )) %>% 
#   group_by(age, gender, year, race_ethn) %>% 
#   summarize(pop = sum(pop)) %>% 
#   ungroup() 
# 
# mpv_pop<-mpv_yr %>% 
#   left_join(pop_st %>% 
#               mutate(year = year + 1))

# read nvss ---------------------------------------------------------------
# 00-20 race
# race is hispanic and non-hispanic except for white

m1_1<-read_tsv("./data/wonder_mort_race_age_sex_00_20.txt") %>% 
  filter(is.na(Notes)) %>% 
  rename(age5 = `Five-Year Age Groups`) %>% 
  filter(age5!="Not Stated") %>% 
  mutate(age = as.numeric(
    case_when(
      age5 == "< 1 year" ~ "0",
      age5 == "1-4 years" ~ "1",
      age5 == "5-9 years" ~ "5",
      age5 == "Not Stated" ~ NA,
      age5 == "100+ years" ~ "100",
      T ~ str_sub(age5, 1, 2)))) %>% 
  mutate(age = ifelse(age>80, 80, age)) %>% # top code, remove missing ages
  filter(!(is.na(age))) %>% 
  mutate(race_ethn = case_when(
    Race == "Black or African American" ~ "black",
    Race == "American Indian or Alaska Native" ~ "aian",
    Race == "Asian or Pacific Islander" ~ "api",
    Race == "Native Hawaiian or Other Pacific Islander" ~ "api")) %>% 
  rename_all(tolower) %>% 
  select(age, gender, year, deaths, race_ethn, population) %>% 
  filter(!(is.na(race_ethn))) %>% 
  mutate(deaths = as.numeric(deaths),
         population = as.numeric(population)) %>% # coerced suppressed to NA, small cells
  group_by(age, gender, year, race_ethn) %>% 
  summarize(deaths = sum(deaths, na.rm=T),
            population = sum(population, na.rm=T)) %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>% 
  ungroup() %>% 
  filter(year > 2012)

# 00-20 hispanic
m1_2<-read_tsv("./data/wonder_mort_hisp_00_20.txt") %>% 
  filter(is.na(Notes)) %>% 
  rename(age5 = `Five-Year Age Groups`) %>% 
  filter(age5!="Not Stated") %>% 
  mutate(age = as.numeric(
    case_when(
      age5 == "< 1 year" ~ "0",
      age5 == "1-4 years" ~ "1",
      age5 == "5-9 years" ~ "5",
      age5 == "Not Stated" ~ NA,
      age5 == "100+ years" ~ "100",
      T ~ str_sub(age5, 1, 2)))) %>% 
  mutate(age = ifelse(age>80, 80, age)) %>% # top code, remove missing ages
  filter(!(is.na(age))) %>% 
  filter(Race == "White") %>% 
  mutate(race_ethn = case_when(
    `Hispanic Origin` == "Hispanic or Latino" ~ "hispanic",
    `Hispanic Origin` == "Not Hispanic or Latino" ~ "white"
  )) %>% 
  filter(!(is.na(race_ethn))) %>% 
  rename_all(tolower) %>% 
  select(age, gender, year, deaths, race_ethn, population) %>% 
  filter(!(is.na(race_ethn))) %>% 
  mutate(deaths = as.numeric(deaths),
         population = as.numeric(population)) %>% # coerced suppressed to NA, small cells
  group_by(age, gender, year, race_ethn) %>% 
  summarize(deaths = sum(deaths, na.rm=T),
            population = sum(population, na.rm=T)) %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>% 
  ungroup() %>% 
  filter(year > 2012)

m1<-bind_rows(m1_1, m1_2) 

## provisional mortality, 31 race cats
m3<-read_tsv("./data/wonder_provisional_mort.txt") %>% 
  rename(age5 = `Five-Year Age Groups`) %>% 
  filter(age5!="Not Stated") %>% 
  mutate(age = as.numeric(
    case_when(
      age5 == "< 1 year" ~ "0",
      age5 == "1-4 years" ~ "1",
      age5 == "5-9 years" ~ "5",
      age5 == "Not Stated" ~ NA,
      age5 == "100+ years" ~ "100",
      T ~ str_sub(age5, 1, 2)))) %>% 
  mutate(age = ifelse(age>80, 80, age)) %>% # top code
  mutate(race_ethn = case_when(
    `Single Race 6` == "Black or African American" ~ "black",
    `Single Race 6` == "American Indian or Alaska Native" ~ "aian",
    `Single Race 6` == "Asian" ~ "api",
    `Single Race 6` == "Native Hawaiian or Other Pacific Islander" ~ "api",
    `Hispanic Origin` == "Hispanic or Latino" ~ "hispanic",
    `Single Race 6` == "White" ~ "white",
    T ~ "multi")) %>% 
  rename_all(tolower) %>% 
  select(age, gender, year, deaths, race_ethn, population) %>% 
  mutate(deaths = as.numeric(deaths),
         population = as.numeric(population)) %>% # coerced suppressed to NA
  group_by(age, gender, year, race_ethn) %>% 
  summarize(deaths = sum(deaths, na.rm=T),
            population = sum(population, na.rm=T)) %>% 
  mutate(year = as.numeric(str_sub(year, 1, 4))) %>% 
  ungroup() %>% 
  filter(year<2024)

m_tot<-bind_rows(m1 %>% 
                   filter(year<2018), 
                 m3) %>% 
  filter(!(is.na(population)))

### multi must be non-hispanic per spec
### see https://www.census.gov/library/stories/2021/08/improved-race-ethnicity-measures-reveal-united-states-population-much-more-multiracial.html


# join --------------------------------------------------------------------

dat<-mpv_yr %>% 
  filter(year<2024) %>% 
  left_join(m_tot) %>% 
  rename(pol_deaths = n)
