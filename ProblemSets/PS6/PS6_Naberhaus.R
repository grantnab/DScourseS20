library(ballr)
library(tidyverse)
library(ggthemes)
library(dbplyr)

#3-point percentage
#Players who take more than 7.5 Harden, Steph Curry, Eric Gordon, Damien Lillard, Paul George, Kyle Lowry
df1 <- NBAPerGameStatistics(season = 2018)
df1 <- df1 %>% select("player" , "pos" , "age" , "x3p" , "x3pa" , "x3ppercent" , "gs" , "mp" , "tm") %>% drop_na("x3ppercent") 
df1 <- df1 %>% filter(tm != "TOT")
df1 <- df1 %>% filter(gs > 15)
df1 <- df1 %>% filter(mp >10)
df1 <- df1 %>% filter(x3p > 0.1)
df1 <- df1 %>% rename(ThreePointPercentage = "x3ppercent")
df1 <- df1 %>% rename(ThreePointersAttempted = "x3pa")
ggplot(data = df1, aes(y = ThreePointPercentage, x=ThreePointersAttempted)) + geom_point()+ theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "ThreePointPercentage", x = "ThreePointersAttempted") + geom_smooth(method = "lm")

#Age and Minutes Played
df2 <- NBAPerGameStatistics(season = 2018)
df2 <- df2 %>% select("player" , "pos" , "age"  , "gs" , "mp" , "tm")
df2 <- df2 %>% filter(tm != "TOT")
df2 <- df2 %>% filter(gs > 15)
df2 <- df2 %>% filter(mp >10)
df2 <- df2 %>% rename(MinutesPlayedPerGame = "mp")
df2 <- df2 %>% rename(Age = "age")
ggplot(data = df2, aes(x = Age, y = MinutesPlayedPerGame)) + geom_point()+ theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "MinutesPlayedPerGame", x = "Age") + geom_smooth(method = "lm")


#Free Throw
df3 <- NBAPerGameStatistics(season = 2018)
df3 <- df3 %>% select("player" , "pos" , "age" , "fta" , "ftpercent" , "gs" , "mp" , "tm") 
df3 <- df3 %>% filter(tm != "TOT")
df3 <- df3 %>% filter(gs > 15)
df3 <- df3 %>% filter(mp >10)
df3 <- df3 %>% filter(fta > 1)
df3 <- df3 %>% rename(FreeThrowsAttempted = "fta")
df3 <- df3 %>% rename(FreeThrowPercent = "ftpercent")
ggplot(data = df3, aes(x = FreeThrowPercent, y = FreeThrowsAttempted)) + geom_point()+ theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y = "FreeThrowsAttempted", x = "FreeThrowPercent") + geom_smooth(method = "lm")
