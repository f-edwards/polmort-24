library(brms)
library(tidyverse)
library(tidybayes)
library(rstan)
library(rstanarm)
library(modelr)

source("read_nat.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

sim_model<-brm(pol_deaths ~ 1 +
          offset(log(population)) +
          (gender * race_ethn | year) +
            (gender * race_ethn | age),
        data = dat %>%
          filter(.imp==1),
        family = "negbinomial")

saveRDS(sim_model, "./models/sim_model.RDS")

m_out<-list()
for(i in 2:max(dat$.imp)){
  m_temp<-update(sim_model,
                 newdata = dat %>%
                   filter(.imp == i))
  m_out[[i]]<-m_temp
  filename<-paste("./models/sim_model", i, ".RDS", sep = "")
  saveRDS(m_temp, file = filename)
  gc()
}
