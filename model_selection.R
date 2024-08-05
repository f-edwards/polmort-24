library(brms)
library(tidyverse)
library(rstan)
library(lme4)

source("read_nat.R")

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dat_mult <- dat %>%
  nest(data = -.imp) %>%
  .$data

### this isn't fitting at all!
### what' going on?
### let's make a generative model 
### or start from total deaths for trouble shooting
### add complexity as I go

temp<-dat %>% 
  filter(.imp ==1 ) %>% 
  group_by(year, age, gender, race_ethn) %>% 
  summarize(pol_deaths = sum(pol_deaths),
            deaths = sum(deaths),
            population = sum(population)) %>% 
  ungroup()


#### start with total deaths
### big q is whether time component impacts death rates...
## i'm not doing much with time here

f0 <- brm(
  pol_deaths ~ gender + race_ethn + 
    (1|year) +
    (1|age) + 
    offset(log(population)),
  data = temp,
  family = 'negbinomial',
  control=list(adapt_delta=0.99, 
               max_treedepth=15))

f1 <- update(f0,
             formula = ~ . + race_ethn*gender)

f1_2<-update(f1, 
             formula = ~ . + log(deaths),
             newdata = temp)

loo_compare(loo(f0), loo(f1), loo(f1_2))

save.image("model_selection.RData")

# huge gains with total mortality in model
## check posterior predictions,
library(tidybayes)
library(modelr)

temp %>% 
  add_predicted_draws(f1_2) %>% 
  ggplot(aes(x = age, y = pol_deaths)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5)) +
  geom_point(data = temp) +
  scale_fill_brewer() +
  facet_wrap(gender ~ race_ethn, scales = "free_y")
#  they look good!
# confirm

temp %>% 
  add_predicted_draws(f1_2) %>% 
  ggplot(aes(x = .prediction)) +
  geom_histogram() + 
  scale_fill_brewer() +
  facet_wrap(gender ~ race_ethn, scales = "free_y")

# seems good!

## anything further? check postpred, postepreds
#### this is promising
## more dynamic effects of time? 



