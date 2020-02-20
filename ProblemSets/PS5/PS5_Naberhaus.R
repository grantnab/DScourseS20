library(tidyverse)
library(rvest)
#table.wikitable:nth-child(5)

#Webscrapping
NBAWIN <- read_html("https://en.wikipedia.org/wiki/List_of_NBA_teams_by_single_season_win_percentage")
table <- NBAWIN %>% html_nodes("table.wikitable:nth-child(5)") %>%
  html_table(fill = TRUE)
table <- table[[1]]


#API ballr
install.packages("ballr")
library(ballr)
library(tidyverse)
table1 <- NBASeasonTeamByYear("HOU", 2018)
table2 <- NBASeasonTeamByYear("NOP", 2018)
table3 <- NBAPerGameAdvStatistics(season = 2018)

