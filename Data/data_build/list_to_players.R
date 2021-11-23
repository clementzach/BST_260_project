library(dplyr)
library(stringr)
load('Injuries_year.Rdata')



full_df <- 42 #don't want to specify colnames in loop below

for (year in names(current_year_list)) {
  for (team in names(current_year_list[[year]])) {
    current_df <- current_year_list[[year]][[team]]
    
    games_in_season = min(ncol(current_df) - 2, 16)
    
    df_as_list <- current_df %>%
      select(-c(Player_id, Player)) %>%
      as.matrix(nrow = nrow(current_df)) %>%
      .[,1:games_in_season] %>% ## remove post-season games
      t() %>% #one col for each player
      as.data.frame() %>%  #so as.list creates one list item for each col
      as.list() 
    
    
    
    num_games_missing <-
      sapply(df_as_list, function(x) {
        sum(!is.na(stringr::str_extract(x, "DNP")))
      })
    num_games_injured <-
      sapply(df_as_list, function(x) {
        sum(!(x == ''))
      })
    earliest_injury <-
      sapply(df_as_list, function(x) {
        injury_times <- which(!(x == ''))
        if(length(injury_times) == 0){
          return(NA)
        }
        else{
          return(min(injury_times))
        }

      })
    latest_injury <-
      sapply(df_as_list, function(x) {
        injury_times <- which(!(x == ''))
        if(length(injury_times) == 0){
          return(NA)
        }
        else{
          return(max(injury_times))
        }

      })
    injury_types <- sapply(df_as_list, function(x) {
      injury <- sapply(x, function(x) {
        last_word <-
          str_split(x, ' ')[[1]] %>% tail(n = 1) #split each entry by space and get the last word
        if (substr(last_word,
                   start = nchar(last_word),
                   stop = nchar(last_word)) == ':') {
          #if the last character is a :, that means they didn't specify the injury type because each injury level ends with :
          return('')
        }
        else{
          return(last_word)
        }
      }) %>% unique() %>%
        paste(collapse = ' ') %>%
        str_trim()
    })
    
    
    
    to_bind <- current_df  %>%
      select(c(Player_id, Player)) %>%
      mutate(
        year = year,
        team = team,
        games_in_season = games_in_season,
        num_games_missing = num_games_missing,
        num_games_injured = num_games_injured,
        injury_types = injury_types,
        earliest_injury = earliest_injury,
        latest_injury = latest_injury
      )
    if (is.numeric(full_df)) {
      full_df <- to_bind
    }
    else{
      full_df <- rbind(full_df, to_bind)
    }
    
  }
}

write.csv(full_df, "injuries_2009_2021.csv")


