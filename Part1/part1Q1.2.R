# install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(nycflights13)
library(dplyr)


flightsPlot = flights %>% 
  filter(origin %in% c("JFK", "LGA", "EWR"), year==2013) %>% 
  select(year, month, day, tailnum, origin) 

# there are 2512 flights without the airplane information
count(flightsPlot %>% 
  filter(is.na(tailnum)) )
  
#1.2 What plane (specified by the tailnum variable) traveled the most times 
# from New York City airports (JFK, LGA or EWR) in 2013? 

### Despite the fact that there are 2512 flights without the airplane information, the airplane which traveled the most times from NY in 2013 is N725MQ with 575 flights.

flightsPlot %>% 
  group_by(tailnum) %>% 
    summarise(TotalFlight = n()) %>% 
  arrange(TotalFlight) %>% 
  filter(!is.na(tailnum)) %>% 
  filter(TotalFlight == max(TotalFlight))

###########################################################################
  

flightsPlot$date = as.Date(with(flightsPlot, paste(year, month, day,sep="-")), "%Y-%m-%d")
flightsPlot$weeks = as.Date(cut(flightsPlot$date, breaks="week"))

flightsPlotgb = flightsPlot %>% 
  group_by(weeks) %>% 
  summarise(NumbersTrip = n())

# sum(flightsPlotgb$NumbersTrip)
# count(flightsPlot)


ggplot(flightsPlotgb, aes(as.Date(weeks), NumbersTrip)) + 
  geom_point() + 
  scale_x_date() +
  geom_line()+
  labs(title = "Number of trips per week\n", x = "Week", y = "Number of trips", fill = "Cancelled") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 


