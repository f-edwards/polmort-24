source("read_nat.R")
library(bsts)
# estimate impact of floyd death 5/25/20 ----------------------------------
# add regressor for date, evaluate posterior for non-zero

pol_ts_tot<-mpv %>%
  mutate(month = floor_date(date, "month")) %>%
  filter(month!=(max(month)),
         .imp ==1) %>%
  group_by(month) %>%
  summarize(n = n()) %>%
  ungroup() %>% 
  mutate(post_floyd = month>="2020-05-25")

# total deaths model
y<-pol_ts_tot$n
x<-pol_ts_tot$post_floyd
logy<-log1p(y)
ss <- AddLocalLinearTrend(list(), logy)
ss <- AddSeasonal(ss, logy, nseasons = 12)
bsts.model <- bsts(logy~x, state.specification = ss, 
                 niter = 2000)

# race specific models
race<-unique(mpv$race_ethn)
dates<-unique(mpv$month)

pol_ts_race<-mpv %>%
  mutate(month = floor_date(date, "month")) %>%
  filter(month!=(max(month))) %>%
  group_by(month, race_ethn, .imp) %>%
  summarize(n = n()) %>% 
  ungroup() %>% 
  mutate(post_floyd = month>="2020-05-25")
## backfill zeroes
backfill<-expand_grid(month = dates,
                      race_ethn = race,
                      .imp = unique(mpv$.imp))

pol_ts_race<-backfill %>% 
  left_join(pol_ts_race) %>% 
  mutate(n = replace_na(n, 0))
# estimate models

betas_out<-list()
for(j in race){
  m_out<-list()
  for(k in 1:max(pol_ts_race$.imp)){
    temp<-pol_ts_race %>% 
      filter(race_ethn==j,
             .imp == k)
    y<-temp$n
    x<-temp$post_floyd
    logy<-log1p(y)
    ss <- AddLocalLinearTrend(list(), logy)
    ss <- AddSeasonal(ss, logy, nseasons = 12)
    m_temp <- bsts(logy~x, state.specification = ss, 
                   niter = 2000)
    burn<-SuggestBurn(0.1, m_temp)
    betas<-tibble(beta.draw = m_temp$coefficients[-(1:burn),2],
                  race_ethn = j)
    m_out[[k]]<- betas
  }
  betas_out[[eval(j)]]<-bind_rows(m_out)
}

burn<-SuggestBurn(0.1, bsts.model)
betas_out[["total"]]<-tibble(
  beta.draw = bsts.model$coefficients[-(1:burn),2],
  race_ethn = "total")

betas_out<-bind_rows(betas_out)

write_csv(betas_out, "./models/bsts_floyd_betas.csv")
