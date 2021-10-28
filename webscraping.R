library(rvest)
library(stringr)
library(dplyr)


base_url <- 'https://www.lineups.com/nfl/rosters'

website <- read_html(base_url)

rosters_urls <-
  website %>% html_elements('.link-black') %>% #grab all of the things of this class
  html_attr('href') %>% #grab any links
  paste("https://www.lineups.com", ., sep = '') #the hrefs only contain a relative path I think so we have to add this

players_df <- #empty dataframe so we can bind all together
  as.data.frame(
    cbind(
      "Pos" = c(),
      "Name" = c(),
      "Number" = c(),
      "Rating" = c(),
      "Ranking" = c(),
      "Height" = c(),
      "Weight" = c(),
      "Age" = c(),
      "Birthday" = c(),
      "Exp." = c(),
      "Drafted" = c(),
      "Draft Round" = c(),
      "Draft Pick" = c(),
      "College" = c(),
      "Team" = c()
    )
  )

for (url in rosters_urls) {
  current_name <- substr(url, start = 36, stop = nchar(url)) %>%
    str_replace(pattern = "-", replacement = " ") %>% str_to_title()
  
  website <- read_html(url)
  tab <-
    website %>% html_element('.multi-row-data-table') %>% html_table() %>% mutate(Team = current_name)
  
  
  players_df <- players_df %>% bind_rows(tab)
}

height_to_inches <- function(str){
  #this can be simple because the website formatted everything uniformly
  feet <- substr(str, start = 1, stop = 1) %>% as.numeric()
  inches <- substr(str, start = 3, stop = nchar(str) - 1) %>% as.numeric()
  return(feet*12 + inches)
  
}

players_df <- players_df %>% mutate(height_inches = height_to_inches(Height), ranking_numeric = str_extract(Ranking, pattern = "[:digit:]+") %>% as.numeric())

write.csv(players_df, file = "nfl_roster.csv")