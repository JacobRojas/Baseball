require(dplyr)
require(ggplot2)
#Read in lahman csv of all players in mlb from all time and their biographical info (birthyear, height, weight, etc.)
all_players = read.csv("C:/Users/jacob/Dropbox/Baseball/Baseball Project/People.csv")
#Converting debut date column, into Date format to prepare for filtering
all_players$debut = as.Date(all_players$debut)
#A subset table of players whose debut was during or after the 2012 season
all_players_subset <- subset(all_players, debut > as.Date("2012-01-13"))
#batting data for each player for each active season
all_players_batting = read.csv("C:/Users/jacob/Dropbox/Baseball/Baseball Project/Batting.csv")

#Combining the age, height, wt info to each player's seasons of unique batting info
new_table = full_join(all_players, all_players_batting, by = "playerID")
new_table$Age = 2018 - new_table$birthYear

#filtering new_table to batting data back to 2012 instead of 1870
homerunswt <- new_table %>% filter(yearID >= 2012 & yearID <= 2017)

#reading in a table of pitchers since 1870
pitchers <- read.csv("Pitching.csv")

#retrieving the ID columns of both the player's batting data and pitcher data to identify 
#which of the entries of homerunswt are pitchers in the pitchers table 
homerunswtID = homerunswt$playerID
pitchID = pitchers$playerID
pitch_dupe_index = homerunswtID %in% pitchID

#Removing pitchers 
homerunswt_no_pitcher = homerunswt[-which(pitch_dupe_index),]

#graphing height vs weight across seasons with and without home runs
avgs_with_pitchers = homerunswt %>% group_by(yearID) %>% summarise(mean_ht = mean(height), mean_wt = mean(weight))
avgs_without_pitchers = homerunswt_no_pitcher %>% group_by(yearID) %>% summarise(mean_ht = mean(height), mean_wt = mean(weight))

ggplot(homerunswt_no_pitcher, aes(weight, height))+
  geom_point(aes(colour = HR, size = HR, alpha = 3))+
  geom_hline(data = avgs_without_pitchers, aes(yintercept = mean_ht), color = "blue") +
  geom_vline(data = avgs_without_pitchers, aes(xintercept = mean_wt), color = "blue") +
  geom_text(data = avgs_without_pitchers, aes(x = 260, y = 65, 
                                    label = paste("Avg =", round(mean_wt, 2), "lbs", sep = " ")), size = 6) + 
  geom_text(data = avgs_without_pitchers, aes(x = 310, y = 73, 
                                    label = paste("Avg =", round(mean_ht, 2), "in", sep = " ")), size = 6, angle = 90) +
  facet_wrap(~yearID) + 
  coord_cartesian(ylim = c(65, 80)) 

ggplot(homerunswt, aes(weight, height))+
  geom_point(aes(colour = HR, size = HR, alpha = 3))+
  geom_hline(data = avgs_with_pitchers, aes(yintercept = mean_ht), color = "blue") +
  geom_vline(data = avgs_with_pitchers, aes(xintercept = mean_wt), color = "blue") +
  geom_text(data = avgs_with_pitchers, aes(x = 260, y = 65, 
                                    label = paste("Avg =", round(mean_wt, 2), "lbs", sep = " ")), size = 6) + 
  geom_text(data = avgs_with_pitchers, aes(x = 310, y = 73, 
                                    label = paste("Avg =", round(mean_ht, 2), "in", sep = " ")), size = 6, angle = 90) +
  facet_wrap(~yearID) + 
  coord_cartesian(ylim = c(65, 80)) 

#Creating a table of only rookies
rookie2012 <- new_table %>% filter (debut > as.Date("2012-04-01") & debut < as.Date("2012-10-30") & yearID == 2012)
rookie2013 <- new_table %>% filter (debut > as.Date("2013-04-01") & debut < as.Date("2013-10-30") & yearID == 2013)
rookie2014 <- new_table %>% filter (debut > as.Date("2014-04-01") & debut < as.Date("2014-10-30") & yearID == 2014)
rookie2015 <- new_table %>% filter (debut > as.Date("2015-04-01") & debut < as.Date("2015-10-30") & yearID == 2015)
rookie2016 <- new_table %>% filter (debut > as.Date("2016-04-01") & debut < as.Date("2016-10-30") & yearID == 2016)
rookie2017 <- new_table %>% filter (debut > as.Date("2017-04-01") & debut < as.Date("2017-10-30") & yearID == 2017)
rookies = rbind(rookie2012, rookie2013, rookie2014, rookie2015, rookie2016, rookie2017)

#retrieving the ID columns of both the player's batting data and pitcher data to identify 
#which of the entries of homerunswt are pitchers in the pitchers table 
rookieID = rookies$playerID
pitch_dupe_index_rook = rookieID %in% pitchID

#Removing pitchers 
rookies_no_pitcher = rookies[-which(pitch_dupe_index_rook),]

rookie_avgs_without_pitchers = rookies_no_pitcher %>% group_by(yearID) %>% summarise(mean_ht = mean(height), mean_wt = mean(weight))
rookie_avgs = rookies %>% group_by(yearID) %>% summarise(mean_ht = mean(height), mean_wt = mean(weight) )
#Possible different way to calculate averages -> rookie_avgs_byname = rookies %>% group_by(playerID) %>% group_by(yearID) %>% summarise(mean_ht = mean(height), mean_wt = mean(weight) )


rookie_hrs = rookies %>% group_by(yearID) %>% summarise(HR = sum(HR))

ggplot(rookie_hrs, aes(HR)) +
  geom_line(aes(x = yearID, y = HR)) +
  ggtitle("Home runs by Rookies per year")+
  labs(x = "Year")+
  ggsave("rookie_hrs.png")

ggplot(rookies, aes(weight, height))+
  geom_point(aes(colour = HR, size = HR, alpha = 3))+
  #geom_hline(data = rookie_avgs, aes(yintercept = mean_ht), color = "blue") +
 # geom_vline(data = rookie_avgs, aes(xintercept = mean_wt), color = "blue") +
  #geom_text(data = rookie_avgs, aes(x = 260, y = 65, 
                                  #  label = paste("Avg =", round(mean_wt, 2), "lbs", sep = " ")), size = 4) + 
  #geom_text(data = rookie_avgs, aes(x = 310, y = 73, 
                                   # label = paste("Avg =", round(mean_ht, 2), "in", sep = " ")), size = 4, angle = 90) +
  coord_cartesian(ylim = c(65, 80)) +
  ggtitle("Rookie Classes by Season") +
  facet_wrap(~yearID) +
  ggsave("rookie_with_pitcher.png")

ggplot(rookies_no_pitcher, aes(weight, height))+
  geom_point(aes(colour = HR, size = HR, alpha = 3))+
  #geom_hline(data = rookie_avgs_without_pitchers, aes(yintercept = mean_ht), color = "blue") +
  #geom_vline(data = rookie_avgs_without_pitchers, aes(xintercept = mean_wt), color = "blue") +
  #geom_text(data = rookie_avgs_without_pitchers, aes(x = 260, y = 65, 
   #                                 label = paste("Avg =", round(mean_wt, 2), "lbs", sep = " ")), size = 4) + 
  #geom_text(data = rookie_avgs_without_pitchers, aes(x = 310, y = 73, 
   #                                 label = paste("Avg =", round(mean_ht, 2), "in", sep = " ")), size = 4, angle = 90) +
  coord_cartesian(ylim = c(65, 80)) +
  ggtitle("Rookie Classes by Season") +
  facet_wrap(~yearID)+
  ggsave("rookie_no_pitcher.png")

#barplots


rookies_no_pitcher %>% group_by(yearID) %>% summarise(meanage = mean(Age))

ggplot(rookies_no_pitcher, aes(Age)) + geom_histogram(binwidth = 0.5) + 
  facet_grid(yearID ~ .) + ggtitle("Age Frequency of Rookies")+
  labs(x ="Age" , y = "Freqency")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  ggsave("rookie_no_pitcher_age.png")

sp <- ggplot(rookies_no_pitcher, aes(x = weight, y = HR)) +
  geom_col(width = 4) + facet_grid(yearID~ .) + ggtitle("Home runs vs. Weight")+
  labs(x = "Weight" , y ="Homeruns" )+ 
  theme(plot.title = element_text(hjust = 0.5))

p <- ggplot(rookies_no_pitcher, aes(x = height, y = HR)) +
  geom_col() + facet_grid(yearID~ .) + ggtitle("Home runs vs. Height")+
  labs(x = "Height" , y ="Homeruns" )+ 
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(sp, p, nrow = 1, top = "Rookie Distributions")
  

