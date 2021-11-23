
rm(list = ls())

library(rvest)
library(stringr)
library(dplyr)
library(lubridate)


base_url <- 'https://www.pro-football-reference.com/teams/'

website <- read_html(base_url)

all_links <- website %>% html_elements('a') %>% html_attr("href")

team_names <-
  all_links[stringr::str_detect(all_links, "/teams/")] %>%
  stringr::str_extract("/\\w{3}/") %>%
  stringr::str_replace_all('/', '') %>%
  unique() %>%
  .[!is.na(.)]


current_year_list <- list()

for (year in 2009:2021) {
  current_teams_list <- list()
  for (team in team_names) {
    Sys.sleep(5) #not hammer his servers too badly
    url = paste(
      'https://www.pro-football-reference.com/teams/',
      team,
      '/',
      year,
      '_injuries.htm',
      sep = ''
    )
    website <- read_html(url)
    
    cols <- website %>%
      html_elements('thead') %>%
      html_text() %>%
      stringr::str_split('\n') %>%
      .[[1]] %>%
      stringr::str_trim() %>%
      .[!(. == '')]
    
    cols <- c("Player_id", cols)
    
    team_matrix <- cols
    
    player_rows <- website %>%
      html_elements('tbody') %>% # this is the table where stuff is stored
      html_elements('tr') #website is structured with one 'tr' per player
    
    for (i in 1:length(player_rows)) {
      unique_id <- player_rows[i] %>% html_elements('th')  %>%
        html_attr("data-append-csv")#will have their name with a couple of zeros or something.
      #Should be a unique identifier.
      
      player_name <- player_rows[i] %>% html_elements('th')  %>%
        html_attr("csk")
      
      injury_data <- player_rows[i] %>%
        html_elements('td') %>%
        html_attr("data-tip") %>%
        stringr::str_replace_na(replacement = "") #so that paste doesn't replace NAs with "NA"
      
      dnp <- player_rows[i] %>%
        html_elements('td') %>%
        html_attr('class') %>%
        stringr::str_extract("dnp") %>% #if they didn't play, the class will have "dnp" somewhere
        stringr::str_to_upper() %>% 
        stringr::str_replace_na(replacement = "") #so that paste doesn't replace NAs with "NA"
      
      all_info <-
        c(unique_id, player_name, paste(dnp, injury_data, sep = " ")) %>% 
        stringr::str_trim()
      
      
      
      team_matrix <- rbind(team_matrix, all_info)
      
    }
    
    team_df <- as.data.frame(team_matrix[2:nrow(team_matrix),])
    colnames(team_df) <- team_matrix[1,]
    
    current_teams_list[[team]] <- team_df
    
  }
  
  current_year_list[[toString(year)]] <- current_teams_list
  
}

save(current_year_list, file = "Injuries_year.Rdata")
