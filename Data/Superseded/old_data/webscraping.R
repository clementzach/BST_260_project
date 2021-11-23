library(rvest)
library(stringr)
library(dplyr)

library(lubridate)

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

players_df <- players_df %>% 
  mutate(height_inches = height_to_inches(Height), 
         ranking_numeric = str_extract(Ranking, pattern = "[:digit:]+") %>% as.numeric(),
         Birthday_string = Birthday,
         Birthday = mdy(Birthday))


is_even <- str_split(players_df$Name, pattern = ' ') %>% sapply(FUN = length) 
max(is_even)
min(is_even)
sum(is_even == 5) #none are odd, good.

full_name <- function(in_string){
  words_vec <- str_split(in_string, pattern = ' ')[[1]]
  name <- paste(words_vec[1:(length(words_vec)/2)], collapse = ' ')
  return(name)
}

short_name <- function(in_string){
  words_vec <- str_split(in_string, pattern = ' ')[[1]]
  name <- paste(words_vec[(length(words_vec)/2 +1):length(words_vec)], collapse = ' ')
  return(name)
}
  

players_df <- players_df %>% mutate(Full_Name = sapply(Name, FUN = full_name),
                                    Short_Name = sapply(Name, FUN = short_name)) %>% 
  select(-c(Name))



write.csv(players_df, file = "nfl_roster.csv")






injuries_url <- "https://www.cbssports.com/nfl/injuries/daily/"

website <- read_html(injuries_url) 

tables <- website %>% html_elements('.TableBase') %>% html_table()

names <- website %>% html_elements('.TableBase-title')  %>% html_text() %>% str_trim()

injuries_df <- data.frame(Team = c(), Player = c(), Position = c(), Injury = c(), "Injury Status" = c(), Date = c())

for (i in 1:length(names)){
  current_df <- tables[[i]] %>% mutate(Date = names[i])
  injuries_df <- rbind(injuries_df, current_df)
  
}

short_name <- function(input_string){
  v <- str_split(input_string, '\n')
  name <- v[[1]][1] %>% str_trim()
  return(name)
}


full_name <- function(input_string){
  v <- str_split(input_string, '\n')
  name <- v[[1]][length(v[[1]])] %>% str_trim()
  return(name)
}

injuries_df <- injuries_df %>% mutate(Date = mdy(Date),
                                      Short_Name = sapply(Player, short_name),
                                      Full_Name = sapply(Player, full_name)
                                      ) %>% select(-Player)



write.csv(injuries_df, file = "injuries.csv")




