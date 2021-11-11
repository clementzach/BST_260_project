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


for (year in 2009:2021){
for (team in team_names){
  url = paste(
    'https://www.pro-football-reference.com/teams/',
    team,
    '/',
    year,
    '_roster.htm',
    sep = ''
  )
  website <- read_html(url)
  
  website %>% 
    html_element('.stats_table')

  
  
}
}

url = "https://www.pro-football-reference.com/teams/was/2021_injuries.htm"

url = "https://www.pro-football-reference.com/teams/was/2021_roster.htm#roster"



website <- read_html(url)

website %>% 
  html_element('body')


website %>% 
  html_element('.right , .left') %>% 
  html_text()
 

## The roster website is dynamic, so it's not working. 


