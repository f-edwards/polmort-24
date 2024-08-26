# callouts for SI ---------------------------------------------------------
# diagnostics and details on data and methods

library(tidyverse)
library(gridExtra)
library(broom)
library(patchwork)
library(bsts)
library(tidybayes)
library(scales)
library(tidybayes)
library(seasonal)
# source data
source("read_nat.R")
bsts_series<-readRDS("./models/bsts_series.RDS")
theme_set(theme_bw())

# missings

mpv<-read_csv("./data/mpv_cleaned.csv")

# cause
table(mpv$cause1)

### check circumstances on low n cats
cause %>% filter(cause=="baton")
cause %>% filter(cause=="drowning")
cause %>% filter(cause=="Chemical Agent")
cause %>% filter(cause=="Fire")
cause %>% filter(cause=="Other")



  

### magnitudes of missing data on core demographics (age, gender, race, date)
mpv %>% 
  select(age, gender, race) %>% 
  mutate(race = ifelse(race == "Unknown race", NA, race),
         gender = ifelse(gender == "Unknown", NA, gender)) %>% 
  summarize_all(.funs = ~sum(is.na(.x))/nrow(mpv))



bridged<-m1 %>% 
  filter(year>2017) %>% 
  mutate(type = "bridged")
single<-m3 %>% 
  filter(year<2021) %>% 
  mutate(type = "single")

comp<-m1 %>% 
  filter(year>2017) %>% 
  rename(deaths_bridged = deaths,
         pop_bridged = population) %>% 
  left_join(m3 %>% 
              filter(year<2021) %>% 
              rename(deaths_single = deaths,
                     pop_single = population))

comp <- comp %>% 
  mutate(q_bridged = deaths_bridged/pop_bridged*1e5,
         q_single = deaths_single/pop_single * 1e5)

ggplot(comp,
       aes(x = q_bridged,
           y = q_single)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  facet_wrap(~race_ethn) + 
  labs(subtitle = "NVSS Single Race 6 vs bridged Race crude mortality 2018-2020")



pol_ts_tot<-ts(mpv %>%
  mutate(month = floor_date(date, "month")) %>%
  filter(month!=(max(month)),
         .imp ==1) %>%
  group_by(month) %>%
  summarize(n = n()) %>%
    ungroup() %>% 
    select(-month),
  frequency = 12,
  start = 2013)

library(seasonal)
library(forecast)

m_tot<-seas(pol_ts_tot)
# format as table
tdat<-coefficients(summary(m_tot))
row.names(tdat)<-c("$\\alpha$", "$\\theta$")

knitr::kable(tdat,
             digits = 2,
             format = "latex",
             caption = "ARIMA(0,1,1) parameter estimates for total deaths caused 
             by police monthly time series January 2013 - February 2024")

pdf("./vis/arima.pdf")
autoplot(m_tot)
dev.off()

# for appx with full diagnostics
#out(m_tot)





### observed life tables, no sims

source("make_multi_table.R")

## 4 year MA
## 2013-15, 14-16, ...
#13-15
lt1<-dat %>% 
  filter(year <= 2015) %>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table()%>% 
  mutate(period = 2015)
#14-16
lt2<-dat %>% 
  filter(year>=2014, 
         year<=2016)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2016)
#15-17
lt3<-dat %>% 
  filter(year>=2015, 
         year<=2017)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2017)
#16-18
lt4<-dat %>% 
  filter(year>=2016,
         year<=2018)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2018)
#17-19
lt5<-dat %>% 
  filter(year>=2017,
         year<=2019)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2019)
#18-20
lt6<-dat %>% 
  filter(year>=2018,
         year<=2020)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2020)
#19-21
lt7<-dat %>% 
  filter(year>=2019,
         year<=2021)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2021)
#20-22
lt8<-dat %>% 
  filter(year>=2020,
         year<=2022)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2022)
#20-22
lt9<-dat %>% 
  filter(year>=2021,
         year<=2023)%>% 
  group_by(.imp, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup() %>% 
  make_multi_table() %>% 
  mutate(period = 2023)

lt_full<-bind_rows(lt1, lt2, lt3, lt4,
                   lt5, lt6, lt7, lt8,
                   lt9)

lt_c<-lt_full %>% 
  filter(age==80) %>% 
  group_by(race_ethn, gender, 
           period) %>% 
  summarize(c_mn = mean(c_i),
            c_se = sd(c_i)/sqrt(max(lt_full$.imp)))

# # plot male age spec q_i
# ggplot(lt_full,
#        aes(x = age, y = q_i * 1e5,
#            group = .imp)) + 
#   geom_line() + 
#   facet_wrap(~race_ethn)    

# plot male c_i
ggplot(lt_c,
       aes(x = period,
           y = c_i,
           ymin = c_i_lwr,
           ymax = c_i_upr,
           color = race_ethn,
           fill = race_ethn)) + 
  geom_line() + 
  geom_pointrange(alpha = 0.5, color = NA) + 
  labs(subtitle = "Lifetime risk for moving 3-year windows, 2015 - 2023") + 
  scale_x_continuous(breaks = breaks_pretty()) + 
  facet_wrap(~gender, 
             scales = "free") + 
  labs(x = "Period",
       fill = "Race/ethnicity",
       color = "Race/ethnicity",
       y = "Cumulative mortality rate, age 85")



p1<-ggplot(lt_full %>% 
             filter(gender == "Female"),
       aes(y= q_i.med * 1e5,
           x = age, 
           color = year, 
           group = year)) + 
  geom_line() + 
  facet_wrap(~race_ethn, ncol = 1, strip.position = "left") + 
  theme(legend.position = "right") + 
  scale_color_continuous(breaks = pretty_breaks()) + 
  labs(x = "Age", y = "Probability per 100,000",
       subtitle = "Female")

p2<-ggplot(lt_full %>% 
             filter(gender == "Male"),
       aes(y= q_i.med * 1e5,
           x = age, 
           color = year, 
           group = year)) + 
  geom_line() + 
  facet_wrap(~race_ethn, ncol = 1, strip.position = "left") +
  scale_color_continuous(breaks = pretty_breaks()) + 
  labs(x = "Age", y = "", color = "Year",
       subtitle = "Male")

p1|p2 + plot_layout(guides = "auto")




lt_wht<-lt_full %>% 
  filter(race_ethn == "White", 
         age == 80) %>% 
  rename(c_i.med.wht = c_i.med,
         c_i.lwr.wht = c_i.lwr,
         c_i.upr.wht = c_i.upr) %>% 
  select(year, age, gender, c_i.med.wht,c_i.upr.wht, c_i.lwr.wht)

lt_full %>% 
  filter(race_ethn !="White",
         age == 80) %>% 
  left_join(lt_wht) %>% 
  mutate(c_disp.med = c_i.med / c_i.med.wht,
         c_disp.lwr = c_i.lwr/c_i.upr.wht,
         c_disp.upr = c_i.upr/c_i.lwr.wht) %>% 
  ggplot(aes(x = year,
             y = c_disp.med,
             color = race_ethn,
             ymin = c_disp.lwr,
             ymax = c_disp.upr)) + 
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 1, lty = 2) + 
  facet_wrap(~gender) + 
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(color = "Race/ethnicity", y = "Cumulative mortality rate ratio")


betas<-read_csv("./models/bsts_floyd_betas.csv")

betas<-betas %>% 
  mutate(race_ethn = 
  case_when(race_ethn == "aian" ~ "AIAN",
            race_ethn == "api" ~ "API",
            race_ethn == "black" ~ "Black",
            race_ethn == "hispanic" ~ "Latine",
            race_ethn == "white" ~ "White",
            T ~ "Total"))

ggplot(betas, 
       aes(x = beta.draw)) +
  geom_density() + 
  facet_wrap(~race_ethn, scales = "free") + 
  labs(x = "Post-May 2020 Beta",
       y = "Posterior density")

ggsave("./vis/betas.pdf", width = 8, height = 6)
