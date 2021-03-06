# install.packages("ggpubr")

library(dplyr)    
library(ggplot2)
library(scales)
library(ggpubr)

# 1. Are there any cryptocurrencies listed in this dataset that have no known market capitalization (market_cap_usd)?

cripto = read.csv("Part2//cryptocurrency_market_2017.csv", stringsAsFactors = FALSE)
cripto %>%
  View()

# nrow(cripto)

cripto %>% 
  mutate(market_cap_usdNA = is.na(market_cap_usd)) %>% 
  group_by(market_cap_usdNA) %>% 
  summarise(n())

criptoMCUNA = cripto %>% 
                filter(is.na(market_cap_usd))
# criptoMCUNA %>% 
#   View()

# 2. Bitcoin has the largest market capitalization. Let’s compare Bitcoin with the rest of the cryptocurrencies. 
# You can visualize the percentage of market capitalization for the top 10 coins as a barplot. 
# Consider how to improve the plot to make it easier to read and convey more information. 

cripto2 = cripto

cript10 = cripto2 %>% 
  group_by(name, market_cap_usd) %>% 
  summarise(market_cap_usdperc = market_cap_usd/sum(cripto2$market_cap_usd, na.rm=T)) %>% 
  arrange(desc(market_cap_usd)) %>% 
  head(., 10)

cript2GB = cripto2 %>% 
  filter(name %in% cript10$name) %>%
  mutate(name2 = ifelse(name %in% cript10$name, name, "Others")) %>% 
  group_by(name2) %>% 
  summarise(market_cap_usdperc = sum(market_cap_usd/sum(cripto2$market_cap_usd, na.rm=T), na.rm=T)*100) 

# str(cript2GB)
# str(cripto)

# cript2GB %>% 
#   View()


ggplot(cript2GB, aes(x=factor((reorder(name2, desc(market_cap_usdperc)))), y=market_cap_usdperc, width=0.8))+
  geom_bar(position = 'dodge', stat='identity', fill="chartreuse3")+
  geom_text(aes(label=gsub(" ", "", paste(round(market_cap_usdperc, 2),"%"))), position=position_dodge(width=0.9), vjust=-0.3)+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60)) +
  ylab("Percentage of market capitalization (%)") +
  xlab("Cryptocurrency") +
  ggtitle("Top 10 cryptocurrency in terms of market capitalization") +
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

# 3. Let’s explore the volatility of the cryptocurrencies market. 
# You can select and plot the top 10 (sort by percent_change_24h in ascending order) coins’ 1 hour (percent_change_1h),
# 24 hours (percent_change_24h) and 7 days percentage change (percent_change_7d).

cripto3 = cripto

cripto3top10_1h = cripto3 %>%
  arrange(percent_change_1h) %>% 
  head(10)

cripto3top10_24h = cripto3 %>%
  arrange(percent_change_24h) %>% 
  head(10)


cripto3top10_7d = cripto3 %>%
  arrange(percent_change_7d) %>% 
  head(10)






ggplot(cripto3top10_1h, aes(x=factor((reorder(name, desc(percent_change_1h)))), y=percent_change_1h, width=0.8))+
  geom_bar(position = 'dodge', stat='identity', fill="chartreuse3")+
  geom_text(aes(label=gsub(" ", "", paste(round(percent_change_1h, 2),"%"))), position=position_dodge(width=0.9), vjust=-0.3)+
  # scale_y_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60)) +
  ylab("Percentage change in the last 1 hour (%)") +
  xlab("Cryptocurrency") +
  ggtitle("Top 10 cryptocurrency in terms of percentage change in the last 1 hour") +
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 




ggplot(cripto3top10_24h, aes(x=factor((reorder(name, desc(percent_change_24h)))), y=percent_change_24h, width=0.8))+
  geom_bar(position = 'dodge', stat='identity', fill="chartreuse3")+
  geom_text(aes(label=gsub(" ", "", paste(round(percent_change_24h, 2),"%"))), position=position_dodge(width=0.9), vjust=-0.3)+
  # scale_y_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60)) +
  ylab("Percentage change in the last 24 hours (%)") +
  xlab("Cryptocurrency") +
  ggtitle("Top 10 cryptocurrency in terms of percentage change in the last 24 hours") +
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot(cripto3top10_7d, aes(x=factor((reorder(name, desc(percent_change_7d)))), y=percent_change_7d, width=0.8))+
  geom_bar(position = 'dodge', stat='identity', fill="chartreuse3")+
  geom_text(aes(label=gsub(" ", "", paste(round(percent_change_7d, 2),"%"))), position=position_dodge(width=0.9), vjust=-0.3)+
  # scale_y_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0,60)) +
  ylab("Percentage change in the last 7 days (%)") +
  xlab("Cryptocurrency") +
  ggtitle("Top 10 cryptocurrency in terms of percentage change in the last 7 days") +
  # theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 




# 4. Design the bar plots to best display the daily and weekly biggest gainers and the biggest losers in market capitalization.

cripto4 = cripto

cripto4dailyLosers = cripto4 %>% 
                       filter(! is.na(market_cap_usd)) %>% 
                       arrange(percent_change_24h) %>% 
                       mutate(group = "Daily") %>% 
                       select(name, percent_change_24h, market_cap_usd, group) %>% 
                       rename(perc = percent_change_24h) %>% 
                       head(5)


cripto4weeklyLosers = cripto4 %>% 
                       filter(! is.na(market_cap_usd)) %>% 
                       arrange(percent_change_7d) %>% 
                       mutate(group = "Weekly") %>% 
                       select(name, percent_change_7d, market_cap_usd, group) %>% 
                       rename(perc = percent_change_7d) %>% 
                       head(5)


cripto4dailyGainers = cripto4 %>% 
                       filter(! is.na(market_cap_usd)) %>% 
                       arrange(desc(percent_change_24h)) %>% 
                       mutate(group = "Daily") %>% 
                       select(name, percent_change_24h, market_cap_usd, group) %>% 
                       rename(perc = percent_change_24h) %>% 
                       head(5)


cripto4weeklyGainers = cripto4 %>% 
                        filter(! is.na(market_cap_usd)) %>% 
                        arrange(desc(percent_change_7d)) %>% 
                        mutate(group = "Weekly") %>% 
                        select(name, percent_change_7d, market_cap_usd, group) %>% 
                        rename(perc = percent_change_7d) %>% 
                        head(5)

cripto4plotLosers = rbind(cripto4dailyLosers, cripto4weeklyLosers)
cripto4plotGainers = rbind(cripto4dailyGainers, cripto4weeklyGainers)


dl = ggplot(cripto4dailyLosers, aes(x=factor((reorder(name, desc(perc)))), y=perc, fill=market_cap_usd, width=0.8))+
  geom_bar(position = 'dodge', stat='identity') +
  labs(title = "Daily biggest losers in market capitalization\n", x = "Cryptocurrency", y = "Percentage change (%)", fill = "Market Capitalization") +

  guides(fill = guide_colourbar(barwidth = 0.8, barheight = 15))+
  scale_fill_gradient2(  high="red",
                         low="red1",
                         midpoint = 30000,
                         guide = "colourbar",
                         aesthetics = "fill", label = comma)+
      # facet_wrap (~group,
      #         nrow=1,scales="fixed", ncol=2)+
  theme(plot.title = element_text(hjust = 0.5)) 

wl = ggplot(cripto4weeklyLosers, aes(x=factor((reorder(name, desc(perc)))), y=perc, fill=market_cap_usd, width=0.8))+
  geom_bar(position = 'dodge', stat='identity') +
  labs(title = "Weekly biggest losers in market capitalization\n", x = "Cryptocurrency", y = "Percentage change (%)", fill = "Market Capitalization") +
  
  guides(fill = guide_colourbar(barwidth = 0.8, barheight = 15))+
  scale_fill_gradient2(  high="red",
                         low="red",
                         midpoint = 11933,
                         guide = "colourbar",
                         aesthetics = "fill", label = comma)+
  # facet_wrap (~group,
  #         nrow=1,scales="fixed", ncol=2)+
  theme(plot.title = element_text(hjust = 0.5)) 



dg = ggplot(cripto4dailyGainers, aes(x=factor((reorder(name, desc(perc)))), y=perc, fill=market_cap_usd, width=0.8))+
  geom_bar(position = 'dodge', stat='identity') +
  labs(title = "Daily biggest gainers in market capitalization\n", x = "Cryptocurrency", y = "Percentage change (%)", fill = "Market Capitalization") +
  
  guides(fill = guide_colourbar(barwidth = 0.8, barheight = 15))+
  scale_fill_gradient2(  high="chartreuse3",
                         low="chartreuse3",
                         midpoint = 5999999,
                         space = "Lab",
                         guide = "colourbar",
                         aesthetics = "fill", label = comma)+
  # facet_wrap (~group,
  #         nrow=1,scales="fixed", ncol=2)+
  theme(plot.title = element_text(hjust = 0.5)) 


wg = ggplot(cripto4weeklyGainers, aes(x=factor((reorder(name, desc(perc)))), y=perc, fill=market_cap_usd, width=0.8))+
  geom_bar(position = 'dodge', stat='identity') +
  labs(title = "Weekly biggest gainers in market capitalization\n", x = "Cryptocurrency", y = "Percentage change (%)", fill = "Market Capitalization") +
  
  guides(fill = guide_colourbar(barwidth = 0.8, barheight = 15))+
  scale_fill_gradient2(  high="chartreuse3",
                         low="chartreuse3",
                         midpoint = 7000000,
                         space = "Lab",
                         guide = "colourbar",
                         aesthetics = "fill", label = comma)+
  # facet_wrap (~group,
  #         nrow=1,scales="fixed", ncol=2)+
  theme(plot.title = element_text(hjust = 0.5)) 

figure = ggarrange(dg, wg, dl, wl, ncol = 2, nrow = 2)
figure


# 5. In general, cryptocurrencies with smaller capitalization are less stable projects, 
# and therefore even riskier investments than the bigger ones. Following the Investopedia's large capitalization 
# definitions (https://www.investopedia.com/terms/b/bitcoin.asp), let’s find out the coins with large capitalization.



cripto5 = cripto


cripto5TopCap = cripto5 %>%
  filter(market_cap_usd >= 10000000000) %>% 
  arrange(desc(market_cap_usd))

cripto5TopCap = rbind(cripto5 %>% 
                        filter(market_cap_usd >= 10000000000) %>% 
                        group_by(name, market_cap_usd, percent_change_1h) %>% 
                        summarise(MCperc = market_cap_usd/1000000000) %>% 
                        arrange(desc(market_cap_usd)) %>% 
                        mutate(group = "A)1 hour") %>% 
                        select(name, percent_change_1h, MCperc, group) %>% 
                        rename(perc = percent_change_1h),
                      cripto5 %>% 
                        filter(market_cap_usd >= 10000000000) %>% 
                        group_by(name, market_cap_usd, percent_change_24h) %>% 
                        summarise(MCperc = market_cap_usd/1000000000) %>% 
                        arrange(desc(market_cap_usd)) %>% 
                        mutate(group = "B) 1 day") %>% 
                        select(name, percent_change_24h, MCperc, group) %>% 
                        rename(perc = percent_change_24h),
                      cripto5 %>% 
                        filter(market_cap_usd >= 10000000000) %>% 
                        group_by(name, market_cap_usd, percent_change_7d) %>% 
                        summarise(MCperc = market_cap_usd/1000000000) %>% 
                        arrange(desc(market_cap_usd)) %>% 
                        mutate(group = "C) 7 days") %>% 
                        select(name, percent_change_7d, MCperc, group) %>% 
                        rename(perc = percent_change_7d))


# cripto5BotCap = cripto5 %>% 
#   arrange(market_cap_usd) %>% 
#   select(name) %>% 
#   head(10)

# cripto5BotCap = rbind(cripto5 %>% 
#                         filter(! is.na(market_cap_usd)) %>% 
#                         arrange(percent_change_7d) %>% 
#                         mutate(group = "Weekly") %>% 
#                         select(name, percent_change_7d, market_cap_usd, group) %>% 
#                         rename(perc = percent_change_7d) %>% 
#                         head(1),
#                       cripto5 %>% 
#                         filter(! is.na(market_cap_usd)) %>% 
#                         arrange(desc(percent_change_7d)) %>% 
#                         mutate(group = "Weekly") %>% 
#                         select(name, percent_change_7d, market_cap_usd, group) %>% 
#                         rename(perc = percent_change_7d) %>% 
#                         head(1))

# cripto5plot = rbind(cripto5daily, cripto5weekly)


# cripto5daily %>% 
#   arrange(market_cap_usd)
# 
# ggplot(cripto5TopCap)+
#   geom_bar(aes(x=name, y=perc, fill=group),position = 'dodge', stat="identity")

  
ggplot(cripto5TopCap)+
  geom_bar(aes(x=factor((reorder(name, desc(MCperc)))), y=perc, fill=MCperc),position = 'dodge', stat="identity")+
  scale_y_continuous(breaks = c(-10,0,30,60,90, 120, 150, 180, 210, 240, 270, 300), limits = c(-10,260)) +
  labs(title = "Cryptocurrencies with large market capitalization and its changes\n", x = "Cryptocurrency", y = "Change(%)", fill = "Market Capitalization(Billions)") +
  # geom_point(aes(x=factor((reorder(name, desc(MCperc)))), y=MCperc, width=0.8))+
  # scale_y_continuous(sec.axis = sec_axis(~.+40, name = "series2"))
  scale_fill_gradient2(  high="chartreuse3",
                         low="red",
                         midpoint = 125,
                         space = "Lab",
                         guide = "colourbar",
                         aesthetics = "fill", label = comma)+
  facet_wrap (~group,
              nrow=1,scales="fixed")+
  theme(plot.title = element_text(hjust = 0.5)) 


# 6. Raise at least one question from your own regarding the bitcoin cryptocurrency market
# and find answers using the given dataset. 
cripto6 = cripto





cripto6Changes = rbind(
cripto6 %>% 
  mutate(perc = percent_change_1h, Group = "1 Hour", market_cap_usd = market_cap_usd/1000000000) %>% 
  mutate(name = paste(name, "(", round(market_cap_usd, 1), "b )")) %>% 
  select(name, perc, Group, market_cap_usd) %>% 
    arrange(desc(market_cap_usd)) %>% 
  head(5),


cripto6 %>% 
  mutate(perc = percent_change_24h, Group = "24 Hour", market_cap_usd = market_cap_usd/1000000000) %>% 
  mutate(name = paste(name, "(", round(market_cap_usd, 1), "b )")) %>% 
  select(name, perc, Group, market_cap_usd) %>% 
  arrange(desc(market_cap_usd)) %>% 
  head(5),

cripto6 %>% 
  mutate(perc = percent_change_7d, Group = "7 Days", market_cap_usd = market_cap_usd/1000000000) %>%
  mutate(name = paste(name, "(", round(market_cap_usd, 1), "b )")) %>% 
  select(name, perc, Group, market_cap_usd) %>% 
  arrange(desc(market_cap_usd)) %>% 
  head(5)
)






ggplot(cripto6Changes, aes(x=factor(Group), y = perc, group = name, color = name))+
  geom_line()+
  labs(title = "Top 5 cryptocurrency in terms of Market Capitalization and its daily and weekly changes\n", x = "Timeframe", y = "Change(%)", color = "Criptocurrency\nMarket Capitalization in billions(%)")
  # scale_color_manual(values = c("darkred", "steelblue"))



