library(dplyr)
library(stringr)


players <- read.csv('/Users/zacharyclement/Downloads/data/player_data.csv') #~500 MB file, so not in github

players_small <- players %>% filter(season >= 2009) %>%  #match range of our data
  select(c(name, franchise_id, position_id, NFL_draft_year, NFL_draft_round, NFL_draft_rank, #not sure which will be relevant
           height_inches, weight_pounds, birthdate, forty_yd, three_cone, bench, broad_jump, 
           combine_height, combine_weight, shuttle, vertical, season, game_starter)) %>% 
  mutate(team = tolower(franchise_id) %>% str_trim(), name = name %>% str_trim()) %>%  
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


duplicates <- which(duplicated(players_small[,c('name', "year", "team")], 
                                  fromLast = T)) #we prefer the first row if possible


players_small <- players_small[-duplicates,] %>% #get rid of duplicated
  mutate(full_team = team_key[team]) %>% #one column will have the full name
  select(c(name, full_team, team, year, 
           position_id, NFL_draft_year, NFL_draft_round, NFL_draft_rank, 
           height_inches, weight_pounds, birthdate, forty_yd, three_cone, 
           bench, broad_jump, combine_height, combine_weight, shuttle, vertical, 
           game_starter))



write.csv(players_small, 
          "../all_player_demographic_clean.csv",
          row.names = F)


