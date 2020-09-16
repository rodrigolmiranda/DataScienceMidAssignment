# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("dplyr")
library(magrittr)
library(dplyr)    
library(ggplot2)
library(scales)

dCovid19 = read.csv("Part2//time_series_covid_19_confirmed.csv", header=TRUE, stringsAsFactors = FALSE)


dCovid19Clone = dCovid19 %>% 
  mutate(key = paste(Province.State, Country.Region)) %>% 
  select(key, Province.State, Country.Region, starts_with("X1.2")) #%>% 
  # head(.,1) 


# dCovid19Clone %>% 
#   View()
# 

dCovid19Cols = 
  dCovid19 %>% 
  select(starts_with("X")) %>% 
  head(., 1)


dCovid19LastReport = data.frame(Provincy=character(), 
                        Country=character(), 
                        Date=as.Date(character()),
                        Cases=as.integer(character()), stringsAsFactors = FALSE) 

listCols = colnames(dCovid19Cols)
# listCols = rev(listCols)

for (k in dCovid19Clone$key) {

    for (d in rev(listCols)) {
    
      cs=as.integer(dCovid19 %>%
              filter(paste(Province.State, Country.Region) == k) %>%
              select(contains(d)))
  
      c = as.character(dCovid19Clone %>% 
              filter(key == k) %>% 
              select(Country.Region) %>% 
              head(.,1))
      p = as.character(dCovid19Clone %>% 
                filter(key == k) %>% 
              select(Province.State) %>% 
              head(.,1))

      if(is.na(cs))  {
        cs = 0
      }

      if(as.numeric(cs, na.rm = T) >0)  {
        dCovid19LastReport = dCovid19LastReport %>% add_row(Provincy = p, Country = c, Date=as.Date(gsub("X", c(""), d),format = "%m.%d.%y"), Cases = cs)
        break
      }
    
    }
  if(cs == 0)  {
    dCovid19LastReport = dCovid19LastReport %>% add_row(Provincy = p, Country = c, Date=as.Date(gsub("X", c(""), d),format = "%m.%d.%y"), Cases = cs)
  }
}

# count(dCovid19LastReport)
dCovid19LastReportGB = dCovid19LastReport %>% 
                  group_by(Country) %>% 
                  summarise(TotCasesperCountry = sum(Cases, na.rm = T)) %>% 
                  arrange(desc(TotCasesperCountry)) %>% 
                  head(., 10) 


# 1. Create a clear bar chart that displays the latest number of COVID-19 cases of the top 10 countries. 
# Consider how to improve the quality and aesthetics of your visualization.

ggplot(data=dCovid19LastReportGB, aes(x=factor(reorder(Country, TotCasesperCountry)), y=TotCasesperCountry, width=0.8)) + 
  geom_bar(position = 'dodge', stat='identity', fill="tomato")+
  ylab("Total Cases per Country") +
  xlab("Country") +
  ggtitle("The latest number of COVID-19 cases of the top 10 countries") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 


dCovid19Ww = data.frame(Provincy=character(), 
                        Country=character(), 
                        Date=as.Date(character()),
                        Cases=as.integer(character()), stringsAsFactors = FALSE) 

for (k in dCovid19Clone$key) {
  
  for (d in listCols) {
    
    cs=as.integer(dCovid19 %>%
                    filter(paste(Province.State, Country.Region) == k) %>%
                    select(contains(d)))
    
    c = as.character(dCovid19Clone %>% 
                       filter(key == k) %>% 
                       select(Country.Region) %>% 
                       head(.,1))
    p = as.character(dCovid19Clone %>% 
                       filter(key == k) %>% 
                       select(Province.State) %>% 
                       head(.,1))
    
    if(is.na(cs))  {
      cs = 0
    }
    
    dCovid19Ww = dCovid19Ww %>% add_row(Provincy = p, Country = c, Date=as.Date(gsub("X", c(""), d),format = "%m.%d.%y"), Cases = cs)

  }
}



# 2. Visualize the confirmed cases worldwide from January to March. 
dCovid19GBWW = dCovid19Ww %>% 
  group_by(Date) %>% 
  summarise(TotperMonth = sum(Cases, na.rm = T)) %>% 
  arrange(desc(TotperMonth))

dCovid19GBWW$weeks = as.Date(cut(dCovid19GBWW$Date, breaks="week"))


ggplot(data=dCovid19GBWW, aes(x=Date, y=as.integer(TotperMonth), width=0.8)) + 
  geom_bar(position = 'dodge', stat='identity', fill="tomato")+
  ylab("Total Cases per Country") +
  xlab("Country") +
  scale_y_continuous(labels = comma, limits = c(0,400000))+
  ggtitle("Confirmed cases worldwide from January to March") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 





# 3. Visualize the confirmed cases of COVID-19 in China and the rest of the world from January to March. 
# Can you relate the main changes observed from the plot with the landmark events such as WHO declared a pandemic?
dCovid19GBwwandChina = dCovid19Ww %>% 
  group_by(Date, Grp = ifelse(Country=="China", "China", "rest of the World")) %>% 
  summarise(Tot = sum(Cases, na.rm = T)) %>% 
  arrange(desc(Tot))

# dCovid19GBwwandChina$weeks = as.Date(cut(dCovid19GBwwandChina$Date, breaks="week"))


ggplot(data=dCovid19GBwwandChina, aes(x=Date, y=as.integer(Tot), fill=Grp, width=0.8)) + 
  geom_bar(position = 'dodge', stat='identity')+
  ylab("Total Cases per Group") +
  xlab("Country") +
  scale_y_continuous(labels = comma, limits = c(0,300000))+
  ggtitle("Confirmed cases of COVID-19 in China and the rest of the world from January to March") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

#At the beginning of March, the numbers of cases confirmed in the rest of the World more than duplicated, very close to the numbers of China. After the third week, the number already was more than double in China.


# 4. Add a smooth trend line using linear regression to measure how fast the number of cases is growing in China after 15 February 2020. 
# How does the rest of the world compare to linear growth?


dCovid19Ww2 = dCovid19Ww %>% 
  filter(Date>="2020-02-15") %>% 
  mutate(Grp = ifelse(Country=="China", "China", "rest of the World"), 
         weeks = as.Date(cut(Date, breaks="week"))
         ) %>% 
  group_by(Grp, Date) %>% 
  summarise(Tot = sum(Cases, na.rm = T)) %>% 
  arrange(Date, Grp)


ggplot(dCovid19Ww2, aes(x=Date, y=Tot, color=Grp, na.rm = FALSE)) + 
  geom_point(mapping=aes(x=Date, 
                         y=Tot),
             colour="blue",alpha=0.5) + 
  geom_smooth(se=FALSE)+
  scale_y_continuous(labels = comma, limits = c(0,300000))+
  facet_wrap (~Grp,
              nrow=1,scales="fixed") +
  ggtitle("Linear regression to measure how fast the number of cases is growing in China after 15 February 2020") +
  theme(plot.title = element_text(hjust = 0.5)) 


dCovid19Ww3 %>%
  View()



# 5. Raise at least one question from your own regarding the COVID-19 pandemic and find answers using the given dataset. 
# Which country had the highest growth curve among the 10 largest in numbers of cases after 01 March 2020?
# Top 10 largest in numbers of cases
# dCovid19LastReportGB$Country
dCovid19Ww3 = dCovid19Ww %>% 
  filter(Country %in% dCovid19LastReportGB$Country) %>% 
  mutate(Grp = ifelse(Country=="China", "China", "rest of the World"), 
         weeks = as.Date(cut(Date, breaks="week"))
  ) %>% 
  group_by(Country, Grp, Date) %>% 
  summarise(Tot = sum(Cases, na.rm = T)) %>% 
  arrange(Date, Grp)
dCovid19Ww3$Country = with(dCovid19Ww3, reorder(Country, desc(Tot)))

ggplot(dCovid19Ww3, aes(x=Date, y=Tot, color=Country, na.rm = FALSE)) + 
  geom_point(mapping=aes(x=Date, 
                         y=Tot, color=Country),alpha=0.5) + 
  geom_smooth(se=FALSE)+

  # scale_y_continuous(labels = comma, limits = c(0,300000))+
  # facet_wrap (~Grp,
  #             nrow=1,scales="fixed") +
  ggtitle("Top 10 largest in numbers of cases") +
  theme(plot.title = element_text(hjust = 0.5)) 
