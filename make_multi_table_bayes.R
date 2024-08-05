make_multi_table_bayes<-function(dat_lt){
  # provide deaths, pop, pol_deaths to function
  lt_med<-dat_lt %>% 
    rename(pol_deaths = pol_deaths.med) %>% 
    mutate(type = "median") %>% 
    group_by(gender, race_ethn) %>% 
    group_map(~make_life_table(.x), .keep = T) %>% 
    bind_rows() %>% 
    rename(d_i.med = d_i, q_i.med = q_i, c_i.med = c_i) %>% 
    select(age:race_ethn, q_i.med:c_i.med)
    
  lt_lwr<-dat_lt %>% 
    rename(pol_deaths = pol_deaths.lwr) %>% 
    mutate(type = "lwrian") %>% 
    group_by(gender, race_ethn) %>% 
    group_map(~make_life_table(.x), .keep = T) %>% 
    bind_rows() %>% 
    rename(d_i.lwr = d_i, q_i.lwr = q_i, c_i.lwr = c_i) %>% 
    select(age:race_ethn, q_i.lwr:c_i.lwr)
  
  lt_upr<-dat_lt %>% 
    rename(pol_deaths = pol_deaths.upr) %>% 
    mutate(type = "uprian") %>% 
    group_by(gender, race_ethn) %>% 
    group_map(~make_life_table(.x), .keep = T) %>% 
    bind_rows() %>% 
    rename(d_i.upr = d_i, q_i.upr = q_i, c_i.upr = c_i) %>% 
    select(age:race_ethn, q_i.upr:c_i.upr)
  
  out<-lt_med %>% 
    left_join(lt_lwr) %>% 
    left_join(lt_upr)
  
  
  
}