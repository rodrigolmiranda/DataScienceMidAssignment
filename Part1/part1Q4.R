install.packages("macleish")
library(macleish)

whately_2015 %>% 
  head(., 1000) %>% 
  View()

ggplot(whately_2015, aes(as.Date(when), temperature)) + 
  geom_point() + 
  scale_x_date() +
  geom_line()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average temperature over each 10-minutes interval\n", x = "Time(10 minutes interval)", y = "Temperature") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 


