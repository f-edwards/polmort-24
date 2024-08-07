library(brms)
library(tidyverse)
library(tidybayes)
library(rstan)
library(rstanarm)
library(modelr)

source("read_nat.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## let gender / race vary by year and age
# could run factor(age) as fe, but this should be fine
# not folding time variation in with 1|year prior model
# sim_model<-brm(pol_deaths ~ 1 +
#                  offset(log(population)) +
#                  (gender * race_ethn|age) + 
#                  (1|year)
#                data = dat %>% 
#                  filter(.imp==1),
#                family = "negbinomial")

# sim_model<-brm(pol_deaths ~ 1 +
#           offset(log(population)) +
#           (gender * race_ethn | year) + 
#             (gender * race_ethn | age),
#         data = dat %>% 
#           filter(.imp==1),
#         family = "negbinomial")
# 
# saveRDS(sim_model, "./models/sim_model2.RDS")

## think on this

# check loo on this one. if it's even comparable to sim_model, I should use it
# this will capture time variation with much more detail. 

# 
sim_model<-readRDS("./models/sim_model2.RDS")
# try race /yr effects
# 
# s2<-update(sim_model, 
#            formula. = ~. -(1|year) + (race_ethn|year))
# 
# s3<-update(sim_model, 
#            formula. = ~. -(1|year) + (race_ethn*gender|year))
# 
# 
# loo_compare(loo(sim_model), loo(s2), loo(s3))
# 
# # no meaninguful improvement in fit


## posterior preds look good
# pred_dat<-mdat %>%
#   data_grid(gender, race_ethn, year, age,
#             population = 1e8)
# 
# preds<-pred_dat %>%
#   add_predicted_draws(m2) %>%
#   median_qi(.width = 0.9)
# 
# ggplot(preds %>%
#          filter(year == 2023),
#        aes(x = age, y = .prediction,
#            ymin = .lower, ymax = .upper,
#            color = race_ethn, fill = race_ethn)) +
#   geom_line() +
#   geom_ribbon() +
#   facet_wrap(year~gender)

m_out<-list()
for(i in 12:max(dat$.imp)){
  m_temp<-update(sim_model,
                 newdata = dat %>%
                   filter(.imp == i))
  m_out[[i]]<-m_temp
  filename<-paste("./models/sim_model", i, ".RDS", sep = "")
  saveRDS(m_temp, file = filename)
}
