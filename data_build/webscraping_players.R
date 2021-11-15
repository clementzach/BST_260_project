library(rvest)
library(stringr)
library(dplyr)



full_df <- 42
num_failures = 0

base_url <- 'https://www.pro-football-reference.com'

for (c_letter in LETTERS) {
  url <- paste(base_url, '/players/', c_letter, '/', sep = '')
  website <- read_html(url)
  
  player_names <- website %>%
    html_elements('.section_content') %>%
    html_elements('p')
  for (i in 1:length(player_names)) {
    obs_added = F
    year = player_names[i] %>%
      html_text() %>%
      str_split(string = ., pattern = "\\s+") %>%
      .[[1]] %>%
      tail(n = 1) %>% #the years will be the last element
      str_split(string = ., pattern = "-") %>%
      .[[1]] %>% #last year the player was active
      tail(n = 1) %>% #we only care about the year they stopped playing
      as.numeric()
    
    if (year >= 2009) {
      Sys.sleep(5) #avoid hammering his servers to badly
      #year is in our range of data
      try(#don't want one error to sxrew up stuff
        {
          c_name <- c_position <- c_height <- c_weight <- c_birthday <- c_birth_place <- c_college <- player_id <-  first_year <- last_year <- ''
          
          player_url <- player_names[i] %>%
            html_elements('a') %>%
            html_attr("href") %>%
            paste(base_url, ., sep = '')
          
          player_id <- player_names[i] %>%
            html_elements('a') %>%
            html_attr("href") %>%
            str_extract("/\\w+.htm") %>%
            str_replace('/', '') %>%
            str_replace('.htm', '')
          
          last_year = year
          
          first_year = player_names[i] %>%
            html_text() %>%
            str_split(string = ., pattern = "\\s+") %>%
            .[[1]] %>%
            tail(n = 1) %>% #the years will be the last element
            str_split(string = ., pattern = "-") %>%
            .[[1]] %>% #last year the player was active
            head(n = 1) %>% #we only care about the year they stopped playing
            as.numeric()
          
          player_website <- read_html(player_url)
          
          c_info <- player_website %>%
            html_elements('p')
          
          c_name <-
            player_website %>% html_element('h1') %>% html_text() %>% str_trim()
          
          c_position <- c_info[2] %>%
            html_text() %>%
            str_split(string = ., pattern = "\\s+") %>%
            .[[1]] %>%
            str_trim() %>%
            paste(., collapse = ' ') %>% #get the vector into one string
            str_replace("Position:", '') %>%
            str_trim()
          
          if (c_info[3] %>% html_text(trim = T) %>% substr(start = 1, stop = 5) != "Team:"){
          
          c_measures <- c_info[3] %>% html_elements('span')
          c_height <- c_measures[1] %>% html_text(trim = T)
          c_weight <- c_measures[2] %>% html_text(trim = T) %>%
            str_extract(pattern = '\\d+') %>% #only digits
            as.numeric()
          }
          else{
            
            c_measures <- c_info[4] %>% html_elements('span')
            c_height <- c_measures[1] %>% html_text(trim = T)
            c_weight <- c_measures[2] %>% html_text(trim = T) %>%
              str_extract(pattern = '\\d+') %>% #only digits
              as.numeric()
          }
          
          if (c_info[4] %>% html_text(trim = T) %>% substr(start = 1, stop = 5) == "Born:") {
            c_birth_info <- c_info[4] %>% html_elements('span')
            c_birthday <-
              c_birth_info[1] %>% html_attr('data-birth')
            c_birth_place <- c_birth_info[2] %>% html_text(trim = T)
            c_college <- c_info[5] %>% html_text(trim = T) %>%
              str_split(string = ., pattern = "\\s+") %>%
              .[[1]] %>%
              tail(n = length(.) - 1) %>%
              str_trim() %>%
              paste(., collapse = ' ') %>% #get the vector into one string
              str_trim()
          }
          else if (c_info[4 + 1] %>% html_text(trim = T) %>% substr(start = 1, stop = 5) == "Born:") {
            c_birth_info <- c_info[4 + 1] %>% html_elements('span')
            c_birthday <-
              c_birth_info[1] %>% html_attr('data-birth')
            c_birth_place <- c_birth_info[2] %>% html_text(trim = T)
            c_college <- c_info[5 + 1] %>% html_text(trim = T) %>%
              str_split(string = ., pattern = "\\s+") %>%
              .[[1]] %>%
              tail(n = length(.) - 1) %>%
              str_trim() %>%
              paste(., collapse = ' ') %>% #get the vector into one string
              str_trim()
          }
          else{
            c_birthday <- ''
            c_birth_place <- ''
            c_college <- ''
          }
          

          
          
          c_info_all <-
            data.frame(
              "name" = c_name,
              "position" = c_position,
              "height" = c_height,
              "weight" = c_weight,
              "birthday" = c_birthday,
              "birthplace" = c_birth_place,
              "college" = c_college,
              "Player_id" = player_id,
              'first_year' = first_year,
              'last_year' = last_year
            )
          
          if (is.numeric(full_df)) {
            full_df <- c_info_all
          }
          else{
            full_df <- rbind(full_df, c_info_all)
          }
          obs_added = T
          
        })
      
      if (obs_added == F){
        print("messed one up")
        print(player_url)
        print(i)
        num_failures = num_failures + 1
        
      }
      
      
    }
    
  }
  
  
}

write.csv(full_df, "players_11_nov.csv")
          