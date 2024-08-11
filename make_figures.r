# make_figures.r ----------------------------------------------------------
# make all figures for polmort-24 manuscript

library(tidyverse)
library(gridExtra)
library(broom)
library(patchwork)
library(bsts)
library(tidybayes)
library(scales)
library(tidybayes)
# source data
source("read_nat.R")
bsts_series<-readRDS("./models/bsts_series.RDS")
theme_set(theme_bw())


# fig 1 time series decompositions
# apply expm1() to seasonal and trend components
pdat<-bind_rows(bsts_series) %>% 
  mutate(month = as_date(month)) %>% 
  mutate(n = case_when(
    type == "trend" ~ expm1(n),
    type == "seasonal" ~ expm1(n),
    type == "data" ~ n)) %>% 
  mutate(race_ethn = 
           case_when(race_ethn == "aian" ~ "AIAN",
                     race_ethn == "api" ~ "API",
                     race_ethn == "black" ~ "Black",
                     race_ethn == "hispanic" ~ "Latine",
                     race_ethn == "white" ~ "White",
                     race_ethn == "total" ~ "Total"))

p0<-ggplot(pdat %>% 
             filter(race_ethn == "Total",
                    type!="seasonal"),
           aes(x = as_date(month), y = n)) + 
  stat_lineribbon(linewidth = 0.2, 
                  .width = c(0.5, 0.8, 0.9)) + 
  geom_vline(xintercept = as_date("2020-05-01"), lty = 2) + 
  scale_fill_brewer() + 
  facet_wrap(~type, 
             ncol = 1,
             scales = "free",
             strip.position = "left") + 
  labs(y = "",
       x = "",
       subtitle = "1. Total deaths") +
  theme(legend.position = "right") + 
  scale_y_continuous(breaks = extended_breaks()) 
## race spec TS
p1 <- ggplot(pdat %>% 
               filter(type!="seasonal",
                      race_ethn!="Total"),
             aes(x = month, y = (n))) + 
  stat_lineribbon(linewidth = 0.2, 
                  .width = c(0.5, 0.8, 0.9)) + 
  scale_fill_brewer() + 
  geom_vline(xintercept = as_date("2020-05-01"), lty = 2) + 
  ggh4x::facet_grid2(rows = vars(type), cols = vars(race_ethn),
                     scales = "free_y", independent = "y",
                     switch = "y") + 
  theme(legend.position = "none") +
  labs(x = "", y = "", subtitle = "2. Deaths by race/ethnicity",
       fill = "Interval level") + 
  scale_y_continuous(breaks = pretty_breaks(n=3)) + 
  scale_x_date(breaks = pretty_breaks(n = 4))

p_out<-p0/p1
ggsave("./vis/fig1.pdf", p_out, units = "cm", width = 17.8, height = 12)


# Fig 2- lifetable cumulative risk

source("lifetable.R")
postpreds<-read_csv("./data/postpreds_mi.csv")
# join to observed mortality NVSS
# compute expected median, lwr, upr police killings from post pred
dat_lt<-dat %>% 
  select(year:race_ethn, deaths, population) %>% 
  distinct() %>% 
  left_join(postpreds) %>% 
  mutate(pol_deaths.med = m_i.med * population,
         pol_deaths.lwr = m_i.lwr * population,
         pol_deaths.upr = m_i.upr * population) %>% 
  select(-(m_i.med:m_i.upr))
# estimate life tables by year, race, gender
# upr, lwr, median post pred, 0.8 interval
lt_med<-dat_lt %>% 
  rename(pol_deaths = pol_deaths.med) %>% 
  group_by(year, gender, race_ethn) %>% 
  group_map(~make_life_table(.x), .keep = T) %>% 
  bind_rows() %>% 
  rename(d_i.med = d_i, q_i.med = q_i, c_i.med = c_i) %>% 
  select(year:race_ethn, q_i.med:c_i.med)

lt_lwr<-dat_lt %>% 
  rename(pol_deaths = pol_deaths.lwr) %>% 
  group_by(year, gender, race_ethn) %>% 
  group_map(~make_life_table(.x), .keep = T) %>% 
  bind_rows() %>% 
  rename(d_i.lwr = d_i, q_i.lwr = q_i, c_i.lwr = c_i) %>% 
  select(year:race_ethn, q_i.lwr:c_i.lwr)

lt_upr<-dat_lt %>% 
  rename(pol_deaths = pol_deaths.upr) %>% 
  group_by(year, gender, race_ethn) %>% 
  group_map(~make_life_table(.x), .keep = T) %>% 
  bind_rows() %>% 
  rename(d_i.upr = d_i, q_i.upr = q_i, c_i.upr = c_i) %>% 
  select(year:race_ethn, q_i.upr:c_i.upr)

lt_full<-lt_med %>% 
  left_join(lt_upr) %>% 
  left_join(lt_lwr)

# format labels
lt_full <- lt_full %>% 
  mutate(race_ethn = 
           case_when(race_ethn == "aian" ~ "AIAN",
                     race_ethn == "api" ~ "API",
                     race_ethn == "black" ~ "Black",
                     race_ethn == "hispanic" ~ "Latine",
                     race_ethn == "white" ~ "White"))


# plot changes in c_i
# plot male c_i
p1<-ggplot(lt_full %>% 
             filter(age == 80,
                    gender == "Female"),
           aes(x = year,
               y = c_i.med,
               ymin = c_i.lwr,
               ymax = c_i.upr,
               color = race_ethn)) + 
  geom_pointrange(alpha = 0.5, position = position_dodge(width = 0.7),
                  fatten = 0.5) + 
  geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = breaks_pretty(n=4)) + 
  labs(x = "",
       color = "",
       y = "Police use of force deaths per 100,000 births",
       subtitle = "Female") + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "right")
# male ci estimates - within CI from PNAS for each group
# checking on time effects | race

p2<-ggplot(lt_full %>% 
             filter(age == 80,
                    gender == "Male"),
           aes(x = year,
               y = c_i.med,
               ymin = c_i.lwr,
               ymax = c_i.upr,
               color = race_ethn)) + 
  geom_pointrange(alpha = 0.5, position = position_dodge(width = 0.5),
                  fatten = 0.5) + 
  geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = breaks_pretty(n=4)) + 
  labs(x = "",
       color = "",
       y = "Police use of force deaths per 100,000 births",
       subtitle = "Male") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")

p_out<-p1/p2 + plot_layout(guides = 'collect', 
                           axis_titles = "collect",
                           axes = 'collect') 
ggsave("./vis/fig2.pdf", p_out, units = "cm", width = 8.7, height = 10)
