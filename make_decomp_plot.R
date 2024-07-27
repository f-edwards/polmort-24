library(forecast)

make_decomp_plot<-function(ts_input, plot_label){
  ts_list<-list()
  ts_models<-list()
  ts_models<-list()
  ts_seasonal<-list()
  for(i in 1:10){
    ts_list[[i]]<-ts(ts_input %>% 
                       filter(.imp==i) %>% 
                       select(n),
                     frequency = 12,
                     start = 2013)
    ts_models[[i]]<-seas(ts_list[[i]],
                         arima.model = c(0, 1, 1))
  }
  
  p_data<-autoplot(ts_list[[1]], series = "Data",
                   color = "black", alpha = 0.2) + 
    autolayer(ts_list[[2]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[3]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[4]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[5]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[6]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[7]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[8]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[9]], color = "black", alpha = 0.2) + 
    autolayer(ts_list[[10]], color = "black", alpha = 0.2) +
    labs(x = "",
         y = "") +
    theme(plot.margin=unit(c(-0.2, 0.2, -0.2, 0.2), "cm"))+
    labs(subtitle = plot_label) +
    scale_x_continuous(breaks = c(2014, 2018, 2022))
  
  p_trend<- autoplot(trendcycle(ts_models[[1]]), 
                     color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[2]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[3]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[4]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[5]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[6]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[7]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[8]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[9]]), 
              color = "black", alpha = 0.4) + 
    autolayer(trendcycle(ts_models[[10]]), 
              color = "black", alpha = 0.4)  +
    labs(x = "",
         y = "") +
    theme(plot.margin=unit(c(-0.2, 0.2, -0.2, 0.2), "cm")) + 
    scale_x_continuous(breaks = c(2014, 2018, 2022))
  
  
  p_out<-list(p_data,
              p_trend)
  
}