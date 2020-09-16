# install.packages("Lahman")
library(Lahman)


#2.1 Define two new variables in the Teams data frame: batting average (BA) and slugging percentage (SLG). 
#Batting average is the ratio of hits (H) to at-bats (AB), and slugging percentage is total bases divided by at-bats. 
#To compute total bases, you get 1 for a single, 2 for a double, 3 for a triple, and 4 for a home run.

# %>% 
TeamsClone = Teams %>% 
  mutate(BA = H/AB, SLG = (((AB-X2B-X3B-HR) + (2*X2B)+ (3*X3B)+ (4*HR))/AB))
#   # View()
#   select(H, AB, X2B, X3B, HR)




# TeamsClone$BA2 = round(TeamsClone$H/TeamsClone$AB, 5)
# TeamsClone$SLG2 = round((TeamsClone$H + (TeamsClone$X2B*2)+ (TeamsClone$X3B*3)+ (TeamsClone$HR*4))/TeamsClone$AB, 5)



#2.2 Display the top 15 teams ranked in terms of slugging percentage in MLB history.
TeamsClone %>% 
  arrange(desc(SLG)) %>% 
  head(., 15) %>% 
  select(name, yearID, SLG) %>% 
  View()


#Repeat this using teams since 1969
TeamsClone %>% 
  filter(yearID >= 1969) %>% 
  arrange(desc(SLG)) %>% 
  head(., 15) %>% 
  select(name, yearID, SLG) %>% 
  View()



# 2.3 Create a factor called election that divides the yearID into four-year blocks that correspond to 
# U.S. presidential terms (from 1788 to 2017). During which term have the most home runs been hit?

# The term that has the most home runs were between 2000 and 2004 with a total of 21175 home runs

TeamsClone$Election = cut(TeamsClone$yearID, seq(1788, 2017, 4))

class(TeamsClone$Election)
TeamsClone %>% 
  select(yearID, Election, HR) %>% 
  # filter(!is.na(Election)) %>% 
  group_by(Election) %>% 
  summarise(TotalHomeRun = sum(HR)) %>% 
  arrange(desc(TotalHomeRun)) %>% 
  head(.,1)
  # View()



