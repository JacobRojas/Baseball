require(dplyr)
require(tidyr)
require(baseballr)

#read in complete pitching db then removing all pitches that were not hit (launch_speed = NA)
pitching.data = read.csv("Pitching.data.csv")
pitching_bip <- pitching.data %>% drop_na(launch_speed)
#Adding binary homerun column, if homerun was hit hr = 1
pitching_bip$hr = 0
pitching_bip$hr[which(pitching_bip$events == "home_run")] = 1
#read in players db which has birthYear, height, weight. Filtering db to recent times to exclude old players
players = read.csv("People.csv")
players = players %>% filter(as.Date(finalGame) > as.Date("2010-01-01") | as.character(finalGame) == "" )

name = strsplit(as.character(pitching_bip$player_name), " ")
#Finding how many unique batters are in the pitching db then separating their names into first and last name strings for searching purposes
unique_batters = unique(pitching_bip$player_name)
unique_batters_sep = strsplit(as.character(unique_batters), " ")
#begin building database which includes unique batter names and batterID 
name_and_code = subset(pitching_bip, select = c(player_name, batter))
name_and_code_unique = unique(name_and_code)
name_and_code_split = strsplit(as.character(name_and_code_unique$player_name), " ")
#Separating player names into potentially 4 names, (Alex, De, La, jr.) so the correct name is used in search
for (i in 1:length(name_and_code_split))
{
  name_and_code_unique$firstName[i] = name_and_code_split[[i]][1]
  name_and_code_unique$lastName[i] = name_and_code_split[[i]][2]
  name_and_code_unique$alternateName[i] = name_and_code_split[[i]][3]
  name_and_code_unique$alternateName2[i] = name_and_code_split[[i]][4]
  
}

name_and_code_unique$count = 1:nrow(name_and_code_unique)

name_and_code_unique_what = strsplit(as.character(name_and_code_unique$player_name), " ")
#editing name so it can go through the search loop
name_and_code_unique[776,]$lastName = "Fratus"


#Search loop using unique names from pitching db and baseballr lookup function to pair names with mlbIDs
for (i in 1:dim(name_and_code_unique)[1])
{
  test_name = playerid_lookup(last_name = name_and_code_unique$lastName[i])#, first_name = name_and_code_unique$firstName[i])
  
  if (nrow(test_name) == 1) #if name returns unique value
  {
    name_and_code_unique$bbref[i] = test_name$bbref_id
    
  } else if (nrow(test_name) == 0)#if no last names returned from playerid_lookup
    { 
       test_name = playerid_lookup(last_name = name_and_code_unique$alternateName[i])#, first_name = name_and_code_unique$firstName[i])
       output = test_name$bbref_id[which(test_name$mlbam_id == name_and_code_unique$batter[i])]
       
       if(length(output) == 0) #if no last names returned from playerid_lookup after trying 2nd name
       {
         test_name = playerid_lookup(last_name = name_and_code_unique$alternateName2[i])#, first_name = name_and_code_unique$firstName[i])
         output = test_name$bbref_id[which(test_name$mlbam_id == name_and_code_unique$batter[i])]
         
         if (length(output) == 0) #if no last names returned from playerid_lookup after trying 3rd name
         {
           name_and_code_unique$bbref[i] = NA
         }
       } 
       else 
         {
         name_and_code_unique$bbref[i] = output
         } 
    } 
  else
  {
    output = test_name$bbref_id[which(test_name$mlbam_id == name_and_code_unique$batter[i])]
    
    if(length(output) == 0) #if no exact match is found with batterID and test_name table
    {
      test_name = playerid_lookup(last_name = name_and_code_unique$alternateName[i])#, first_name = name_and_code_unique$firstName[i])
      output = test_name$bbref_id[which(test_name$mlbam_id == name_and_code_unique$batter[i])]
      
      if (length(output) == 0) #if no last names returned from playerid_lookup after trying 3rd name
      {
        test_name = playerid_lookup(last_name = name_and_code_unique$alternateName2[i])#, first_name = name_and_code_unique$firstName[i])
        output = test_name$bbref_id[which(test_name$mlbam_id == name_and_code_unique$batter[i])]
        
        if (length(output) == 0)
        {
          name_and_code_unique$bbref[i] = NA
        }
      }
      else
      {
        name_and_code_unique$bbref[i] = output
      }
    } 
    else
      {
        name_and_code_unique$bbref[i] = output
      } 
  }
}

#remove NAs before this step


#Adding birth year, weight, and height to intermediate database with batter codes identical to pitching database
for (i in 1:nrow(name_and_code_unique))
  {
    name_and_code_unique$birthYear[i] = players[which(name_and_code_unique$bbref[i] == players$bbrefID),]$birthYear
    name_and_code_unique$weight[i] = players[which(name_and_code_unique$bbref[i] == players$bbrefID),]$weight
    name_and_code_unique$height[i] = players[which(name_and_code_unique$bbref[i] == players$bbrefID),]$height
  }

#Creating three new columns in the pitching database
pitching_bip$birthYear = NA
pitching_bip$weight = NA
pitching_bip$height = NA

#Populating the three added columns with data from intermediate database (name_and_code_unique)
for (i in 1:nrow(name_and_code_unique))
  {
    name_ind = which(name_and_code_unique$batter[i] == pitching_bip$batter)
    pitching_bip$birthYear[name_ind] = name_and_code_unique[i,]$birthYear
    pitching_bip$weight[name_ind] = name_and_code_unique[i,]$weight
    pitching_bip$height[name_ind] = name_and_code_unique[i,]$height
}
#Adding/calculating column of age using new data
pitching_bip$age = pitching_bip$game_year - pitching_bip$birthYear

#Logistical Regression
log_reg <- glm(hr ~ release_speed + launch_speed + weight + height + age + stand + p_throws, data = pitching_bip, family = "binomial")
summary(log_reg)
confint(log_reg)
