# BST_260_project
Group project for Introduction to Data Science

The data collection/cleaning scripts in Data/data_build can be used as follows:

Running webscraping_injuries.R (as in Rscript webscraping_injuries.R) updates the Injuries_year.Rdata file, which contains a list with dataframes with injury tables for every year/team combinations. 

Running list_to_players.R uses the Injuries_year.Rdata file to produce a csv file (injuries_2009_2021.csv) which has a row for every player who appeared in the injuries tables. 

Running clean_injuries.R uses the injuries_2009_2021.csv file to produce a csv file with our definition of number of injuries per year for each player. This file is written to Data/all_injuries_clean.csv

Running clean_players.R takes a csv file from gridironai.com (not contained on the github because of file size) and writes a file to Data/all_player_demographic_clean.csv which contains one row per player per year. 

