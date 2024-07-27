library(tidyverse)
pop<-read_fwf("./data/us.1990_2022.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips", "cnty_fips", "reg", "race", 
                           "hisp", "sex", "age", "pop"))) %>% 
  filter(year>2010)

pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn = 
           case_when(
             race==1 & hisp ==0 ~ "white",
             race==2 ~ "black",
             race==3 ~ "aian",
             race==4 ~ "api",
             hisp==1 ~ "hispanic")
  )

pop_st<-pop%>%
  group_by(year, state, st_fips, 
           age, race_ethn,
           sex)%>%
  summarise(pop = sum(pop))%>%
  ungroup()

write_csv(pop_st, "./data/pop_st.csv")