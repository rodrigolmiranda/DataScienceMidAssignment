# install.packages("nasaweather")
library(nasaweather)

ggplot(storms, aes(pressure, wind, color = factor(type))) +
  geom_point(alpha = 0.5, shape=16)+
  geom_jitter(width=0.3)+
  labs(title = "Pressure x Wind per type of storm\n", x = "Pressure", y = "Wind", color = "Type") +
  theme(plot.title = element_text(hjust = 0.5)) 
#There are a lot of overlapping data points in this scatter plot, so, in order to improve the visualization I've reduced the size of the shape, have used 0.5 of transparency, and have used geom_Jitter.
