# BST_260_project
Group project for Introduction to Data Science

#### Data Collection
The data collection/cleaning scripts in Data/data_build can be used as follows:

Running webscraping_injuries.R (as in Rscript webscraping_injuries.R) updates the Injuries_year.Rdata file, which contains a list with dataframes with injury tables for every year/team combinations. The file scrapes creates these dataframes from tables found in webpages such as [this one](https://www.pro-football-reference.com/teams/buf/2021_injuries.htm).

Running list_to_players.R uses the Injuries_year.Rdata file to produce a csv file (injuries_2009_2021.csv) which has a row for every player who appeared in the injuries tables. 

Running clean_injuries.R uses the injuries_2009_2021.csv file to produce a csv file with our definition of number of injuries per year for each player. This file is written to Data/all_injuries_clean.csv

Running clean_players.R takes a csv file from gridironai.com (not contained on the github because of file size, but it can be downloaded [here](https://drive.google.com/drive/u/0/folders/1KTcoZRtcylZRc9rZk9zfA4PwJon-xtwB)) and writes a file to Data/all_player_demographic_clean.csv which contains one row per player per year. 

<br>

#### Shiny App
The R Shiny App can be found in the following path:

BST_260_project>EDA>Question2 and is named `app.R`

This app is also shown on a separate tab in the website

<br>

#### Website
The google site displaying our objects and analysis can be found here: [Google Site](https://sites.google.com/view/bst260groupprojectkntailgaters/home)
