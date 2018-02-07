# ======================================================================
# Title: Make HOF Players Stats Table
# Description:
#   This script makes a players stats csv.
# Input(s): data file 'hof.csv', 'hof-batting.csv', 'hof-pitching.csv'
# Output(s): data file 'hof-players.csv'
# Author: Timothy Tan
# Date: 2-5-2018
# ======================================================================

library(ggplot2)
library(dplyr)
library(readr)

hof_general <- read_csv("data/hof.csv")
hof_general <- hof_general[,-3]
names(hof_general)[names(hof_general) == 'Year'] <- 'Inducted'
names(hof_general)[names(hof_general) == 'Voted By'] <- 'By'
names(hof_general)[names(hof_general) == 'Inducted As'] <- 'Category'

hof_batting <- read_csv("data/hof-batting.csv")
hof_batting <- hof_batting[,-1]
names(hof_batting)[names(hof_batting) == 'WAR/pos'] <- 'Batting_WAR'

hof_pitching <- read_csv("data/hof-pitching.csv")
hof_pitching <- hof_pitching[,-1]
names(hof_pitching)[names(hof_pitching) == 'WAR'] <- 'Pitching_WAR'

hof <- merge(hof_general, hof_batting, by = 'Name', all = TRUE)
hof <- merge(hof, hof_pitching, by = 'Name', all = TRUE)

hof_players <- filter(hof, Category == 'Player')
hof_players <- hof_players[, c(-7, -34)]
names(hof_players)[names(hof_players) == 'Inducted.x'] <- 'Inducted'

write_csv(hof_players, "data/hof-player-stats.csv")



