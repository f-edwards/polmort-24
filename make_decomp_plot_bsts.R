library(scales)
library(patchwork)





# 
# p1<-ggplot(bsts_series$black,
#            aes(x = month, y = n)) + 
#   stat_lineribbon(linewidth = 0.2, 
#                   .width = c(0.5, 0.8, 0.9, 0.95)) + 
#   scale_fill_brewer() + 
#   facet_wrap(~type, 
#              ncol = 1,
#              scales = "free",
#              strip.position = "left") + 
#   labs(y = "",
#        x = "",
#        subtitle = "Black") +
#   scale_x_continuous(breaks = breaks_pretty()) + 
#   scale_y_continuous(breaks = extended_breaks()) +
#   theme(axis.title = element_blank(),
#         legend.position = "none")
# 
# p2<-ggplot(bsts_series$hispanic,
#            aes(x = month, y = n)) + 
#   stat_lineribbon(linewidth = 0.2, 
#                   .width = c(0.5, 0.8, 0.9, 0.95)) + 
#   scale_fill_brewer() + 
#   facet_wrap(~type, 
#              ncol = 1,
#              scales = "free",
#              strip.position = "left") + 
#   labs(y = "",
#        x = "",
#        subtitle = "Latine") +
#   scale_x_continuous(breaks = breaks_pretty()) + 
#   scale_y_continuous(breaks = extended_breaks()) +
#   theme(axis.title = element_blank(),
#         legend.position = "none")
#   
# p3<-ggplot(bsts_series$white,
#            aes(x = month, y = n)) + 
#   stat_lineribbon(linewidth = 0.2, 
#                   .width = c(0.5, 0.8, 0.9, 0.95)) + 
#   scale_fill_brewer() + 
#   facet_wrap(~type, 
#              ncol = 1,
#              scales = "free",
#              strip.position = "left") + 
#   labs(y = "",
#        x = "",
#        subtitle = "White") +
#   scale_x_continuous(breaks = breaks_pretty()) + 
#   scale_y_continuous(breaks = extended_breaks()) +
#   theme(axis.title = element_blank(),
#         legend.position = "none")
# 
# p4<-ggplot(bsts_series$aian,
#            aes(x = month, y = n)) + 
#   stat_lineribbon(linewidth = 0.2, 
#                   .width = c(0.5, 0.8, 0.9, 0.95)) + 
#   scale_fill_brewer() + 
#   facet_wrap(~type, 
#              ncol = 1,
#              scales = "free",
#              strip.position = "left") + 
#   labs(y = "",
#        x = "",
#        subtitle = "AIAN") +
#   scale_x_continuous(breaks = breaks_pretty()) + 
#   scale_y_continuous(breaks = extended_breaks()) +
#   theme(axis.title = element_blank(),
#         legend.position = "none")
# 
# p5<-ggplot(bsts_series$api,
#            aes(x = month, y = n)) + 
#   stat_lineribbon(linewidth = 0.2, 
#                   .width = c(0.5, 0.8, 0.9, 0.95)) + 
#   scale_fill_brewer() + 
#   facet_wrap(~type, 
#              ncol = 1,
#              scales = "free",
#              strip.position = "left") + 
#   labs(y = "",
#        x = "",
#        subtitle = "API") +
#   scale_x_continuous(breaks = breaks_pretty()) + 
#   scale_y_continuous(breaks = extended_breaks()) +
#   theme(axis.title = element_blank(),
#         legend.position = "none")
