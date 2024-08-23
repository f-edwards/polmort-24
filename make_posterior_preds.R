# read in models for each imputation
# generate posterior predictions
# pool, compute tables over
# draw credible intervals
library(modelr)
library(tidybayes)
library(brms)
source("read_nat.R")

files<-list.files(path = "./models")
files<-files[grep("sim_", files)]

files<-paste("./models/", files, sep = "")

m_in<-lapply(files, readRDS)
# prediction over 100million pop
pred_dat<- dat %>% 
  data_grid(gender, race_ethn, year, age,
            population = 1e8) 
# posterior predictive over each model
postpreds<- m_in %>% 
  map(add_predicted_draws,
      newdata = pred_dat)

postpreds<-postpreds %>% 
  bind_rows()

postpreds_ci<-postpreds %>% 
  median_qi(.width = 0.8)

# check distribution
# ggplot(preds %>%
#          filter(year == 2023),
#        aes(x = age, y = .prediction,
#            ymin = .lower, ymax = .upper,
#            color = race_ethn, fill = race_ethn)) +
#   geom_line() +
#   geom_ribbon() +
#   facet_wrap(~gender, scales = "free")
# looks good

# estimate tables over gender, race_ethn, year,
# .prediction, .lower, .upper
# compute intervals  

# convert to predicted deaths for empirical population
# using m as .prediction/1e8 * population

postpreds_mi<-postpreds_ci %>% 
  mutate(m_i.med = .prediction / population,
         m_i.lwr = .lower / population,
         m_i.upr = .upper / population) %>% 
  select(gender:age, m_i.med:m_i.upr)

write_csv(postpreds_mi, 
          "./data/postpreds_mi.csv")


