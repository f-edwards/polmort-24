# estimate bsts models -------------------------------------------------------
# sourced from ms.qmd
# working on 7/29/24
# convert mpv to subseries ------------------------------------------------
library(bsts)

pol_ts_tot<-mpv %>%
  mutate(month = floor_date(date, "month")) %>%
  filter(month!=(max(month)),
         .imp ==1) %>%
  group_by(month) %>%
  summarize(n = n()) %>%
  ungroup() 

race<-unique(mpv$race_ethn)
dates<-unique(pol_ts_tot$month)

pol_ts_race<-mpv %>%
  mutate(month = floor_date(date, "month")) %>%
  filter(month!=(max(month))) %>%
  group_by(month, race_ethn, .imp) %>%
  summarize(n = n()) %>% 
  ungroup()
## backfill zeroes
backfill<-expand_grid(month = dates,
                 race_ethn = race,
                 .imp = unique(mpv$.imp))

pol_ts_race<-backfill %>% 
  left_join(pol_ts_race) %>% 
  mutate(n = replace_na(n, 0))

# estimate ts model for each imp and race ---------------------------------
### race subseries models, by imputation
# set iterations for sampler
n_iter<-1000
bsts_out<-list()
for(j in race){
  imp_temp<-list()
  for(k in 1:max(mpv$.imp)){
    # obtain y, estimate model
    temp <- pol_ts_race %>% 
      filter(race_ethn == j,
             .imp==k) 
    y<-temp$n
    logy<-log1p(y)
    ss <- AddLocalLinearTrend(list(), logy)
    ss <- AddSeasonal(ss, logy, nseasons = 12)
    bsts.model <- bsts(logy, state.specification = ss, 
                       niter = n_iter)
    burn <- SuggestBurn(0.1, bsts.model)
    # extract components
    trend<-bsts.model$state.contributions[-(1:burn),"trend",] %>% 
      as_tibble() 
    names(trend)<-dates
    trend<-trend %>% 
      rownames_to_column(var = ".imp")
    trend<-trend %>% 
      pivot_longer(cols = -.imp,
                   names_to = "month",
                   values_to = "n") %>% 
      mutate(type = "trend",
             n = (n),
             month = as_date(month),
             .imp = as.numeric(.imp))
    
    seasonal<-bsts.model$state.contributions[-(1:burn),"seasonal.12.1",] %>% 
      as_tibble() 
    names(seasonal)<-dates
    seasonal<-seasonal %>% 
      rownames_to_column(var = ".imp")
    seasonal<-seasonal %>% 
      pivot_longer(cols = -.imp,
                   names_to = "month",
                   values_to = "n") %>% 
      mutate(type = "seasonal",
             n = (n),
             month = as_date(month),
             .imp = as.numeric(.imp))
    
    out<-bind_rows(
      trend,
      seasonal,
      temp %>% 
        mutate(type = "data")) %>% 
      mutate(race_ethn = j) %>% 
      mutate(type = fct_relevel(type,
                                "data", "trend", "seasonal"))
    
    imp_temp[[k]]<-out
    
  }
  bsts_out[[eval(j)]]<-bind_rows(imp_temp)
}

# estimate total mortality ts model ---------------------------------------
# with seasonality and local linear trend
# log1p for growth, exp1m as inverse for plotting

pol_ts_tot<-pol_ts_tot %>% 
  mutate(post_floyd = month > as_date("2020-05-25"))

y<-pol_ts_tot$n
logy<-log1p(y)
x<-pol_ts_tot$post_floyd
ss <- AddLocalLinearTrend(list(), logy)
ss <- AddSeasonal(ss, logy, nseasons = 12)
bsts_tot <- bsts(logy, state.specification = ss, 
                 niter = n_iter)

pred <- predict(bsts_tot, horizon = 12, burn = 100)
plot(pred)

saveRDS(bsts_tot, "bsts_tot.RDS")

burn <- SuggestBurn(0.1, bsts_tot)
trend<-bsts_tot$state.contributions[-(1:burn),"trend",] %>% 
  as_tibble() 
names(trend)<-dates
trend<-trend %>% 
  rownames_to_column(var = ".imp")
trend<-trend %>% 
  pivot_longer(cols = -.imp,
               names_to = "month",
               values_to = "n") %>% 
  mutate(type = "trend",
         n = (n),
         month = as_date(month),
         .imp = as.numeric(.imp))

seasonal<-bsts_tot$state.contributions[-(1:burn),"seasonal.12.1",] %>% 
  as_tibble() 
names(seasonal)<-dates
seasonal<-seasonal %>% 
  rownames_to_column(var = ".imp")
seasonal<-seasonal %>% 
  pivot_longer(cols = -.imp,
               names_to = "month",
               values_to = "n") %>% 
  mutate(type = "seasonal",
         n = (n),
         month = as_date(month),
         .imp = as.numeric(.imp))

out<-bind_rows(
  trend,
  seasonal,
  pol_ts_tot %>% 
    mutate(type = "data")) %>% 
  mutate(race_ethn = "total") %>% 
  mutate(type = fct_relevel(type,
                            "data", "trend", "seasonal"))

bsts_out$total <- out

saveRDS(bsts_out, file = "bsts_series.RDS")
