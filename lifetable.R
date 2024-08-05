### make period lifetables  for police mort data
## uses methods from Preston et al. (2001)
## uses kf nax from demogR package

make_life_table<-function(dat){
  ### compute crude death rate m
  dat<-dat%>%
    ungroup() %>% 
    mutate(m = deaths / population)
  ### define n as period length for each age
  ### age is 0-1, 1-4, then 5 years
  ### 80 is top code, arbitrarily set n to 18.1 for nax/2 
  dat<-dat%>%
    mutate(n = case_when(
      age==0 ~ 1,
      age==1 ~ 4,
      age==80 ~ 18.1,
      ! age %in% c(1, 4, 80) ~ 5
    ))
  ### define nax as avg yrs lived of deceased
  ### borrowing kf nax values from demogR package
  ### ~= Keyfitz Flieger 1990
  ### early mortality for infants, 
  ### then midpoint of n for others
  dat<-dat %>% 
    mutate(nax = case_when(
      age == 0 ~ 0.0806,
      age == 1 ~ 1.5,
      T ~ n/2
      ))
  ### compute pr(death) as q
  ## q = (n * m) / (1 + (n - nax) * m)
  ## manually fix pr(Death) at 85+ to 1
  dat<-dat%>%
    mutate(q = case_when(
      age != 85 ~ (n * m) / (1 + (n - nax) * m),
      age == 85 ~ 1
    ))
  ### compute survival p as 1 - q
  dat<-dat %>% 
    mutate(p = 1-q)
  ### make cumulative survival
  ### l0 = 1e5, l_x+n = lx*p
  dat<-dat%>%
    mutate(lx = 1e5 * cumprod(c(1, p[-nrow(dat)])))
  ### cohort deaths
  dat<-dat%>%
    mutate(d = c(-diff(lx),lx[nrow(dat)]))
  ## person-years in each group
  dat<-dat%>%
    mutate(L = (lx - d) * n + d * nax)
 ## person years lived above age
  dat<-dat %>% 
    mutate(t = sum(L) - cumsum(L) + L)
  ## life expectancy at age as person years / persons
  dat<-dat %>% 
    mutate(e = t / lx)
  ### cum incidence
  ### as 1 - survivors from prior
  dat<-dat%>%
    mutate(c = 1-lx/1e5)
  ## cause specific mortality probability
  dat<-dat %>% 
    mutate(q_i = q * pol_deaths / deaths)
  ## cohort deaths due to cause
  dat<-dat %>% 
    mutate(d_i = q_i*lx)
  ## cumulative cause deaths
  dat<-dat %>% 
    mutate(c_i = cumsum(d_i))
  return(dat)
}

