# NFL Injury Analysis

### 1. Data Build/Web Scraping
The data collection/cleaning scripts in Data/data_build can be used as follows:

Running `webscraping_injuries.R` (as in Rscript webscraping_injuries.R) updates the Injuries_year.Rdata file, which contains a list with dataframes with injury tables for every year/team combinations. 

Running `list_to_players.R` uses the Injuries_year.Rdata file to produce a csv file (injuries_2009_2021.csv) which has a row for every player who appeared in the injuries tables. 

Running `clean_injuries.R` uses the injuries_2009_2021.csv file to produce a csv file with our definition of number of injuries per year for each player. This file is written to Data/all_injuries_clean.csv

Running `clean_players.R` takes a csv file from gridironai.com (not contained on the github because of file size) and writes a file to Data/all_player_demographic_clean.csv which contains one row per player per year. 

### 2. Exploratory Data Analysis
For each of our 4 research questions, we completed exploratory data analysis. Each analysis is saved in the `EDA` folder for each question being analyzed. Data, plots, and regression output which appears in our final RMarkdown file are available in the question-specific EDA files.

### 3. RShiny
The app.R file underlying our RShiny app is saved here: `EDA/Question2/app.R`. The iteractive RShiny app is also available on our website, [here](https://sites.google.com/view/bst260groupprojectkntailgaters/analysis/rshiny-app?authuser=0).  

### 4. Final Report
Our final report RMarkdown and associated HTML files are saved in the `Final Report` folder.  
