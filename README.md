# NFL Injury Analysis

### 1. Data Build/Web Scraping

We generated two csv files which underlie all of our analysis in this project. Both are saved in the `Data` folder, and they are called:

- `all_injuries_clean.csv` - This is the data on all injuries incurred by players in the NFL between 2009-2021.It contains information on player name, team, year, injury type, and injury duration.   
- `all_player_demographic_clean.csv` - This dataset contains "demographic" data on NFL players in the 2009-2021 period. For example, height, weight, position, and birth date. 

We built these datasets ourselves by web scraping a website which we assessed to be a reliable source of information on NFL injuries called [pro-football-reference.com](https://www.pro-football-reference.com/). All web scraping and data building scripts are located in the `Data/data_build` folder. The process for scraping and constructing these data is listed below:

**Building all_player_demographic_clean.csv**

1) `webscraping_injuries.R` (as in Rscript webscraping_injuries.R) updates the `Injuries_year.Rdata` file, which contains a list with dataframes with injury tables for every year/team combination. The file scrapes creates these dataframes from tables found in webpages such as [this one](https://www.pro-football-reference.com/teams/buf/2021_injuries.htm).

2) `clean_players.R` takes a csv file from gridironai.com (not contained on the github because of file size, but it can be downloaded [here](https://drive.google.com/drive/u/0/folders/1KTcoZRtcylZRc9rZk9zfA4PwJon-xtwB)) and writes to `Data/all_player_demographic_clean.csv` which contains one row per player per year. 

**Building all_injuries_clean.csv**

1) `list_to_players.R` uses the `Injuries_year.Rdata` file to produce a csv file (`injuries_2009_2021.csv`) which has a row for every player who appeared in the injuries tables. 

2) `clean_injuries.R` uses the `injuries_2009_2021.csv` file to produce a csv file with our definition of number of injuries per year for each player. This file is written to `Data/all_injuries_clean.csv`.

There are 2 other subfolders within the `Data` folder: `Data/data_dictionary` and `Data/data_for_plots`. The data dictionary folder contains a single file explaining each variable in our datasets. The data_for_plots folder contains datasets that were generated by adapting our two primary datasets each of our 4 various analyses. We save these intermediate datasets in this folder so that we can refer to them directly, and so minimize cleaning code in our final RMarkdown file.  

### 2. Exploratory Data Analysis
For each of our 4 research questions, we completed exploratory data analysis. Each analysis is saved in the `EDA` folder for each question being analyzed. Data, plots, and regression output which appears in our final RMarkdown file are available in the question-specific EDA files.

### 3. Shiny App
The R Shiny App can be found in the following path: `BST_260_project/EDA/Question2` and is named `app.R` This app is also shown on a separate tab on our project [website](https://sites.google.com/view/bst260groupprojectkntailgaters/home) under the [RShiny Tab](https://sites.google.com/view/bst260groupprojectkntailgaters/analysis/rshiny-app). 

### 4. Final Report
Our final report RMarkdown and associated HTML files are saved in the `Final Report` folder.  

### 5. Website
The Google site displaying our pri=oject objectives and analyses can be found here: [Google Site](https://sites.google.com/view/bst260groupprojectkntailgaters/home).
