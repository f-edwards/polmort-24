make_multi_table<-function(dat){
  # provide deaths, pop, pol_deaths to function
  lt_aianM<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "aian",
             .imp == i,
             gender == "Male")
    lt_aianM[[i]]<-make_life_table(temp)
  }
  
  lt_blackM<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "black",
             .imp == i,
             gender == "Male")
    lt_blackM[[i]]<-make_life_table(temp)
  }
  
  lt_whiteM<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "white",
             .imp == i,
             gender == "Male")
    lt_whiteM[[i]]<-make_life_table(temp)
  }
  
  lt_apiM<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "api",
             .imp == i,
             gender == "Male")
    lt_apiM[[i]]<-make_life_table(temp)
  }
  
  lt_hispanicM<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "hispanic",
             .imp == i,
             gender == "Male")
    lt_hispanicM[[i]]<-make_life_table(temp)
  }
  
  lt_aianF<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "aian",
             .imp == i,
             gender == "Female")
    lt_aianF[[i]]<-make_life_table(temp)
  }
  
  lt_blackF<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "black",
             .imp == i,
             gender == "Female")
    lt_blackF[[i]]<-make_life_table(temp)
  }
  
  lt_whiteF<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "white",
             .imp == i,
             gender == "Female")
    lt_whiteF[[i]]<-make_life_table(temp)
  }
  
  lt_apiF<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "api",
             .imp == i,
             gender == "Female")
    lt_apiF[[i]]<-make_life_table(temp)
  }
  
  lt_hispanicF<-list()
  for(i in 1:max(dat$.imp)){
    temp<-dat %>% 
      filter(race_ethn == "hispanic",
             .imp == i,
             gender == "Female")
    lt_hispanicF[[i]]<-make_life_table(temp)
  }
  
  lt_m<-bind_rows(lt_aianM, lt_blackM, lt_whiteM, lt_apiM, lt_hispanicM)
  lt_f<-bind_rows(lt_aianF, lt_blackF, lt_whiteF, lt_apiF, lt_hispanicF)
  
  lt_out<-bind_rows(lt_m, lt_f)
  
}