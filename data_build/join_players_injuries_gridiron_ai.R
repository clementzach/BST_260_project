library(dplyr)
library(stringr)


players <- read.csv('/Users/zacharyclement/Downloads/data/player_data.csv') #~500 MB file, so not in github

players_small <- players %>% filter(season >= 2009) %>%  #match range of our data
  select(c(name, franchise_id, position_id, NFL_draft_year, NFL_draft_round, NFL_draft_rank, #not sure which will be relevant
           height_inches, weight_pounds, birthdate, forty_yd, three_cone, bench, broad_jump, 
           combine_height, combine_weight, shuttle, vertical, season, game_starter)) %>% 
  mutate(team = tolower(franchise_id) %>% str_trim(), name = name %>% str_trim()) %>%  #to match the entries in injuries_df table
  rename(year = season) #to match colname of injuries_df

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

injuries_df <- read.csv("injuries_2009_2021.csv") 

injuries_df <- injuries_df %>% 
  mutate(name = sapply(Player, function(x){full = str_split(x, ",")[[1]]; return(paste(full[2], full[1]) %>% str_trim())}),
         injury_types = tolower(injury_types), #easier for string processing if these are all consistent
         team = str_trim(team)) #not sure if there's whitespace around these

## make sure we have just one entry per player
upper_duplicates <- which(duplicated(injuries_df[,c('name', "year", "team")]))
lower_duplicates <- which(duplicated(injuries_df[,c('name', "year", "team")], fromLast = T))

my_duplicates <- c(lower_duplicates, upper_duplicates)

injuries_df[my_duplicates,]

## seems like the player with the lower index has better information. 

injuries_df <- injuries_df[-upper_duplicates,] #get rid of higher index







injuries_join_players <- left_join(x = players_small,
                                   y = injuries_df, 
                               by = c("year", "name", "team"))

dup_from_join <- which(duplicated(injuries_join_players[,c('name', "year", "team")], 
                                  fromLast = T)) #we prefer the first row if possible

injuries_join_players <- injuries_join_players[-dup_from_join,] %>% #get rid of duplicated
  mutate(full_team = team_key[team]) %>% #one column will have the full name
  select(c(name, full_team, team, year, games_in_season, num_games_missing, 
           num_games_injured, injury_types, earliest_injury, latest_injury, 
           position_id, NFL_draft_year, NFL_draft_round, NFL_draft_rank, 
           height_inches, weight_pounds, birthdate, forty_yd, three_cone, 
           bench, broad_jump, combine_height, combine_weight, shuttle, vertical, 
           game_starter))



write.csv(injuries_join_players, paste("../all_players_some_injuries", Sys.Date() ,".csv", sep = ''))

# print(paste(colnames(injuries_join_players), collapse = ', '))
