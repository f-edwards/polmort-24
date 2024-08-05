library(forecast)
library(scales)
library(patchwork)

make_decomp_plot<-function(ts_input, plot_label){
  ts_list<-list()
  ts_models<-list()
  ts_models<-list()
  ts_seasonal<-list()
  ts_plotdat<-list()
  for(i in 1:max(ts_input$.imp)){
    ts_list[[i]]<-ts(ts_input %>% 
                       filter(.imp==i) %>% 
                       select(n),
                     frequency = 12,
                     start = 2013)
    ts_models[[i]]<-seas(ts_list[[i]],
                         arima.model = c(0, 1, 1))
    
    ts_plotdat[[i]]<-autoplot(ts_models[[i]])$data 
    ts_plotdat[[i]]$.imp<-i
  }
  
  ts_plotdat <- ts_plotdat %>% 
    bind_rows()
  
  p_out<-ggplot(ts_plotdat%>% 
           filter(parts!="seasonal"),
         aes(x = datetime, y = y,
             group = .imp)) + 
    geom_line(alpha = 0.2) + 
    facet_wrap(~parts,
               scales = "free_y",
               ncol = 1,
               strip.position = "left") + 
    labs(y = "",
         x = "",
         subtitle = plot_label) +
    scale_x_continuous(breaks = breaks_pretty(3)) + 
    scale_y_continuous(breaks = extended_breaks(3)) 
}
