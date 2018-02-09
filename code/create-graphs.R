# ============================================================================
# Title: Create Graphs
# Description:
#   This script makes graphs.
# Input(s): multiple .csv files
# Output(s): graphs in .png files
# Author: Timothy Tan
# Date: 2-5-2018
# ============================================================================

library(ggplot2)
library(dplyr)
library(readr)

hof_players <- read_csv("data/hof-player-stats.csv")
yearly_avg_pitch <- read_csv("data/year_average_pitching.csv")
yearly_avg_hit <- read_csv("data/year_average_batting.csv")
war_leaders <- read_csv("data/war_leaders.csv")

hofWAR <- merge(hof_players, war_leaders, by = 'Name')

#3000 hits and 500 home runs
ggHitsHR <- ggplot() +
  geom_point(data = filter(hof_players, AB > 4000, H.x < 3000, HR.x < 500),
             aes(x = H.x, y = HR.x)) +
  geom_point(data = filter(hof_players, AB > 4000, H.x >= 3000, HR.x < 500),
             aes(x = H.x, y = HR.x, color = 'blue')) +
  geom_point(data = filter(hof_players, AB > 4000, H.x < 3000, HR.x >= 500),
             aes(x = H.x, y = HR.x, color = 'red')) +
  geom_point(data = filter(hof_players, AB > 4000, H.x >= 3000, HR.x >= 500),
             aes(x = H.x, y = HR.x, color = 'green')) +
  scale_color_manual(labels = c('3000 hits', '3000 hits and 500 home runs', 
                                '500 home runs'), 
                     values = c('blue', 'green', 'red')) +
  geom_hline(yintercept = 500) +
  geom_vline(xintercept = 3000) +
  labs(x = "Hits", y = "Home Runs",
       title = "Home Runs and Hits among current Hall of Famers") +
  theme(legend.title=element_blank())
ggsave(filename = "images/hits_hr.png")

#3000 strikeouts and 300 wins
ggKW <- ggplot(data = filter(hof_players, IP > 0)) +
  geom_point(aes(x = SO.y, y = W)) + 
  geom_point(data = filter(hof_players, IP > 0, SO.y < 3000, W < 300), 
             aes(x = SO.y, y = W)) + 
  geom_point(data = filter(hof_players, IP > 0, SO.y >= 3000, W < 300), 
             aes(x = SO.y, y = W, color = 'blue')) + 
  geom_point(data = filter(hof_players, IP > 0, SO.y < 3000, W >= 300), 
             aes(x = SO.y, y = W, color = 'red')) + 
  geom_point(data = filter(hof_players, IP > 0, SO.y >= 3000, W >= 300), 
             aes(x = SO.y, y = W, color = 'green')) + 
  scale_color_manual(labels = c('3000 strikeouts', 
                                '3000 strikeouts and 300 wins', 
                                '300 wins'), 
                     values = c('blue', 'green', 'red')) +
  geom_hline(yintercept = 300) +
  geom_vline(xintercept = 3000) + 
  labs(x = "Strikeouts", y = "Wins", 
       title = "Strikeouts and Wins among current Hall of Famers") +
  theme(legend.title=element_blank())
ggsave(filename = "images/strikeouts_wins.png")

#HR over time
ggHR <- ggplot() +
  geom_point(data = filter(hof_players, AB > 4000, HR.x < 500),
             aes(x = To.x, y = HR.x)) +
  geom_point(data = filter(hof_players, AB > 4000, HR.x >= 500),
             aes(x = To.x, y = HR.x, color = 'green')) +
  scale_color_manual(labels = c('500 home runs'), 
                     values = c('green')) +
  geom_hline(yintercept = 500) +
  labs(x = "Last Year", y = "Home Runs",
       title = "Home Runs over Time among Hall of Famers") +
  theme(legend.title=element_blank())
ggsave(filename = "images/homeruns.png")

#Hits over time
ggHits <- ggplot() +
  geom_point(data = filter(hof_players, AB > 4000, H.x < 3000),
             aes(x = To.x, y = H.x)) +
  geom_point(data = filter(hof_players, AB > 4000, H.x >= 3000),
             aes(x = To.x, y = H.x, color = 'green')) +
  scale_color_manual(labels = c('3000 hits'), 
                     values = c('green')) +
  geom_hline(yintercept = 3000) +
  labs(x = "Last Year", y = "Hits",
       title = "Hits over Time among Hall of Famers") +
  theme(legend.title=element_blank())
ggsave(filename = "images/hits.png")

#Wins over time
ggWins <- ggplot() +
  geom_point(data = filter(hof_players, IP > 0, W < 300),
             aes(x = To.x, y = W)) +
  geom_point(data = filter(hof_players, IP > 0, W >= 300),
             aes(x = To.x, y = W, color = 'green')) +
  scale_color_manual(labels = c('300 wins'), 
                     values = c('green')) +
  geom_hline(yintercept = 300) +
  labs(x = "Last Year", y = "Wins",
       title = "Wins over Time among Hall of Famers") +
  theme(legend.title=element_blank())
ggsave(filename = "images/wins.png")

#Strikeouts over time
ggHR <- ggplot() +
  geom_point(data = filter(hof_players, IP > 0, SO.y < 3000),
             aes(x = To.x, y = SO.y)) +
  geom_point(data = filter(hof_players, IP > 0, SO.y >= 3000),
             aes(x = To.x, y = SO.y, color = 'green')) +
  scale_color_manual(labels = c('3000 strikeouts'), 
                     values = c('green')) +
  geom_hline(yintercept = 3000) +
  labs(x = "Last Year", y = "Strikeouts",
       title = "Strikeouts over Time among Hall of Famers") +
  theme(legend.title=element_blank())
ggsave(filename = "images/strikeouts.png")

#how many players in the hall of fame?
numBatters <- as.numeric(nrow(filter(hof_players, AB > 4000)))
numPitchers <- as.numeric(nrow(filter(hof_players, IP > 0)))
numRelievers <- as.numeric(nrow(filter(hof_players, GS < 100, IP > 0)))
numTotal <- as.numeric(nrow(hof_players))

#how many above 3000 hits or 500 homeruns out of qualifying batters?
hits3k <- as.numeric(length(na.omit(hof_players$H.x[hof_players$H.x >= 3000])))
hr500 <- as.numeric(length(na.omit(hof_players$HR.x[hof_players$HR.x >= 500])))
over500and3k <- as.numeric(length(na.omit(
  hof_players$HR.x[((hof_players$HR.x >= 500)&(hof_players$H.x >= 3000))])))
percentOver3k <- round(hits3k/nrow(filter(hof_players, AB > 4000)) * 100, 2)
percentOver500 <- round(hr500/nrow(filter(hof_players, AB > 4000)) * 100, 2)
over500and3kPercent <- 
  round((over500and3k)/nrow(filter(hof_players, AB > 4000)) * 100, 2)

#how many above 3000 strikeouts or 300 wins out of qualifying pitchers?
SO3k <- as.numeric(length(na.omit(hof_players$SO.y[hof_players$SO.y >= 3000])))
wins300 <- as.numeric(length(na.omit(hof_players$W[hof_players$W >= 300])))
over300and3k <- as.numeric(length(na.omit(
  hof_players$SO.y[((hof_players$W >= 300)&(hof_players$SO.y >= 3000))])))
percentOfSO3k <- round(SO3k/nrow(filter(hof_players, IP > 0)) * 100, 2)
percentOver300 <- round(wins300/nrow(filter(hof_players, IP > 0)) * 100, 2)
over300and3kPercent <- 
  round((over300and3k)/nrow(filter(hof_players, IP > 0)) * 100, 2)

#others
no3kOr500 <- round((numBatters - hits3k - hr500 + over500and3k)/
                     nrow(filter(hof_players, AB > 4000)) * 100, 2)
no3kOr300 <- round((numPitchers - SO3k - wins300 + over300and3k)/
                     nrow(filter(hof_players, IP > 0)) * 100, 2)

#league HR over time
ggHR_avg <- ggplot(data = yearly_avg_hit, aes(x = Year, y = HR)) +
  geom_point() +
  labs(x = "Year", y = "Home Runs Per Game",
       title = "Home Runs Per Game") +
  geom_smooth()
ggsave(filename = "images/homeruns_per_game.png")

#league Hits over time
ggHit_avg <- ggplot(data = yearly_avg_hit, aes(x = Year, y = H)) +
  geom_point() +
  labs(x = "Year", y = "Hits Per Game",
       title = "Hits Per Game") +
  geom_smooth()
ggsave(filename = "images/hits_per_game.png")

#league Strikeout over time
ggK_avg <- ggplot(data = yearly_avg_pitch, aes(x = Year, y = SO)) +
  geom_point() +
  labs(x = "Year", y = "Strikeouts Per Game",
       title = "Strikeouts Per Game") +
  geom_smooth()
ggsave(filename = "images/strikeouts_per_game.png")

#WAR over time
ggWAR_HOF <- ggplot(data = hofWAR, aes(x = To.x, y = WAR)) +
  geom_point() +
  geom_hline(aes(yintercept = mean(hofWAR$WAR), colour = 2)) +
  labs(x = "Year", y = "Career WAR",
       title = "WAR among Hall of Famers") +
ggsave(filename = "images/warHOF.png")



