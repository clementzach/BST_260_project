players_df <- read.csv("players_11_nov.csv")
injuries_df <- read.csv('injuries_2009_2021.csv')

library(dplyr)
library(lubridate)

injuries_df <- injuries_df %>% 
  mutate(name = sapply(Player, function(x){full = str_split(x, ",")[[1]]; return(paste(full[2], full[1]) %>% str_trim())}),
         injury_types = tolower(injury_types), #easier for string processing if these are all consistent
         team = str_trim(team)) #not sure if there's whitespace around these

upper_duplicates <- which(duplicated(injuries_df[,c('name', "year", "team")]))
lower_duplicates <- which(duplicated(injuries_df[,c('name', "year", "team")], fromLast = T))

my_duplicates <- c(lower_duplicates, upper_duplicates)

injuries_df[my_duplicates,]

## seems like the player with the lower index has better information. 

injuries_df <- injuries_df[-upper_duplicates,] #get rid of higher index


players_df <- players_df %>% mutate(year = 0)

with_year <- players_df[c(),] 


for(i in 1:nrow(players_df)){
  current_row <- players_df[i, ]
  
  for(c_year in current_row$first_year:current_row$last_year){
    current_row$year <- c_year
    with_year <- rbind(with_year, current_row)
  }
}

team_key <- c("buf" = "Buffalo Bills", 
              "nwe" = "New England Patriots", 
              "nyj" = "New York Jets",
              "oti" = "Tennessee Titans",
              "clt" = "Indianapolis Colts",
              "jax" = "Jacksonville Jaguars",
              "htx" = "Houston Texans",
              "rav" = "Baltimore Ravens",
              "pit" = "Pittsburgh Steelers",
              "cin" = "Cincinnati Bengals",
              "cle" = "Cleveland Browns",
              "rai" = "Las Vegas Raiders",
              "sdg" = "Los Angeles Chargers",
              "den" = "Denver Broncos",
              "mia" = "Miami Dolphins",
              "kan" = "Kansas City Chiefs",
              "dal" = "Dallas Cowboys",
              "nyg" = "New York Giants",
              "phi" = "Philadelphia Eagles",
              "was" = "Washington Football Team",
              "tam" = "Tampa Bay Buccaneers",
              "nor" = "New Orleans Saints",
              "atl" = "Atlanta Falcons",
              "car" = "Carolina Panthers",
              "gnb" = "Green Bay Packers",
              "min" = "Minnesota Vikings",
              "chi" = "Chicago Bears",
              "det" = "Detroit Lions",
              "crd" = "Arizona Cardinals",
              "ram" = "Los Angeles Rams",
              "sea" = "Seattle Seahawks",
              "sfo" = "San Francisco 49ers"
)


output_df <- left_join(with_year, injuries_df, by = c("Player_id", "year"))  %>% 
  mutate(birthday = ymd(birthday), 
         height = sapply(height, FUN = function(x){
           if(is.character(x)){
           x_v <- str_split(x, pattern = "-")[[1]] 
           if(length(x_v) == 2){
           
             x_v <- as.numeric(x_v)
           return(x_v[1] * 12  + x_v[2])
           }
           else{
             return(NA)
           }
           }
           else{
             return(NA)
           }
         }),
         college = college %>% str_replace("(College Stats)", ""),
         full_team = team_key[team], 
         ) %>% 
  select(c(name.x, position, full_team,team,year, height, weight, birthday, birthplace, college, 
            games_in_season, num_games_missing, num_games_injured, 
           injury_types, earliest_injury, latest_injury)) %>% 
  rename(name = name.x)


write.csv(write.csv(output_df, paste("../all_players_some_injuries", Sys.Date() ,".csv", sep = '')))



