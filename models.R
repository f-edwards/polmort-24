library(brms)
library(tidyverse)
library(tidybayes)
library(rstan)
library(rstanarm)
library(modelr)

source("read_nat.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# sim_model<-brm(pol_deaths ~ 1 + 
#           offset(log(population)) + 
#           (1|year) + 
#           (gender * race_ethn|age),
#         data = mdat,
#         family = "negbinomial")

sim_model<-readRDS("./models/sim_model.RDS")
# try race /yr effects
s2<-update(sim_model, 
           formula. = ~. -(1|year) + (race_ethn|year))



loo_compare(loo(sim_model), loo(s2))

### posterior preds look good
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
for(i in 20:max(dat$.imp)){
  m_temp<-update(sim_model,
                 newdata = dat %>% 
                   filter(.imp == i))
  m_out[[i]]<-m_temp
  filename<-paste("./models/sim_model", i, ".RDS", sep = "")
  saveRDS(m_temp, file = filename)
}

saveRDS(m_out, file = "./models/all_models.RDS")
