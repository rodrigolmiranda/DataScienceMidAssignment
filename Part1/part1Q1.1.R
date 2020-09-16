# install.packages("ggplot2")
# install.packages("nycflights13")
# install.packages("dplyr")

library(ggplot2)
library(nycflights13)
library(dplyr)

#######################
# flightsPlot = flights %>%
#   # filter(is.na(arr_delay)) %>%
#   mutate(month = factor(month)) %>% 
#   select(month, arr_delay)
# 
# 
# flightsPlot[is.na(flightsPlot$arr_delay), "canceled"] =  1
# flightsPlot[!is.na(flightsPlot$arr_delay), "canceled"] =  0
# 
# flightsPlot[flightsPlot$canceled == 0, "sched"] =  1
# flightsPlot[flightsPlot$canceled == 1, "sched"] =  1

# flightsPlot %>%
#   filter(sched==1) %>%
#   View()

# ggplot(data=flightsPlot, aes(x=month, fill=canceled, width=1)) + 
#   geom_bar()
#   geom_bar(position = 'dodge', stat='identity') +
#   geom_text(aes(label=month), position=position_dodge(width=0.9), vjust=-0.3)


#########################



  ##########################

  
# ##########################
# flightsNcanc = flights %>% 
#   # filter(is.na(arr_delay)) %>% 
#   select(month, arr_delay) %>% 
#   mutate(canceled = is.na(arr_delay)) %>% 
#   group_by(month) %>% 
#   summarise(c = sum(!is.na(arr_delay))) %>% 
#   mutate(type="Not Canceled")
# 
# flightsCanc = flights %>% 
#   # filter(is.na(arr_delay)) %>% 
#   select(month, arr_delay) %>% 
#   mutate(canceled = is.na(arr_delay)) %>% 
#   group_by(month) %>% 
#   summarise(c = sum(is.na(arr_delay))) %>% 
#   mutate(type="Canceled")
# 
# total = rbind(flightsNcanc, flightsCanc)
# 
# 
# ggplot(data=total, aes(x=factor(month), y=c, fill=type, width=1)) + 
#   geom_bar(position = 'dodge', stat='identity') +
#   geom_text(aes(label=c), position=position_dodge(width=0.9), vjust=-0.3)
# 
# ##########################
# 
# 
# 
# ##########################
# flightsCancProp = flights %>% 
#   filter(is.na(arr_delay))
#   
# 
# ggplot(flightsCancProp, aes(factor(month))) + 
#   geom_bar(aes(y = (..count..)/sum(..count..)), fill="tomato") + 
#   scale_y_continuous(labels=scales::percent, limits = c(0,0.15)) +
#   ylab("relative frequencies")+
#   theme_classic()+
#   xlab("Month")+
#   ylab("Proportion")
# ####################### 



####################### answers
flightsPlot = flights %>%
  select(month, arr_delay) %>%
  group_by(month) %>%
  summarise(Total = sum(!is.na(month)), NotCancelled = sum(!is.na(arr_delay)), Cancelled = sum(is.na(arr_delay))) %>%
  mutate(Proportion = Cancelled/Total*100) 
# %>% 
#   mutate(countFlightsTOT = countFlightsCanc/countFlights*100)


#1.1 What month had the highest proportion of canceled flights (the arr_delay variable is NA)? 1340
# max(flightsPlot$Cancelled, na.rm=TRUE)

flightsPlot %>% 
  filter(Proportion == max(flightsPlot$Proportion, na.rm=TRUE)) %>% 
  select(month)


#)? What month had the lowest? 271
# min(flightsPlot$Cancelled, na.rm=TRUE)
flightsPlot %>% 
  filter(Proportion == min(flightsPlot$Proportion, na.rm=TRUE)) %>% 
  select(month)


#Plot the proportion of canceled flights each month
ggplot(data=flightsPlot, aes(x=factor(month), y=Proportion, fill=Cancelled, width=1)) + 
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=gsub(" ", "", paste(Cancelled, "\n", round(Proportion, 2),"%"))), position=position_dodge(width=0.9), vjust=-0.3)+
  scale_y_continuous(limits = c(0,6))+
  labs(title = "Proportion of cancelled flights per month\n", x = "Month", y = "Proportion(%)", fill = "Cancelled") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 

# interpret any seasonal patterns
#Analysing the plot it is possible to notice that there are 3 peaks(4 months) when the cancellation rate increases, 
#which are in February, June/July, and December.

