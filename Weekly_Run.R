library(httr)
library(jsonlite)
library(reticulate)

NFL_week <- 8

#py_install("pandas")
py_run_file('/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions/NFL_Data_Pull.py')

setwd("/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions")

fileName <- paste0('/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions/Weekly_Update/Weekly_Update_', Sys.Date(), '.csv')

keep_update_columns <- function(){
  x <-c(
  "ID",
  "Team", 
  "Opponent", 
  "Season", 
  "Week", 
  "HomeOrAway", 
  "Score", 
  "OffensiveYards", 
  "OffensivePlays", 
  "OffensiveYardsPerPlay", 
  "Giveaways", 
  "TimeOfPossession",
  "PassingCompletions", 
  "PassingAttempts", 
  "CompletionPercentage", 
  "PassingYards",
  "PassingTouchdowns", 
  "PassingInterceptions",
  "TimesSacked", 
  "TimesSackedYards", 
  "PasserRating", 
  "RushingYards", 
  "RushingYardsPerAttempt", 
  "RushingTouchdowns", 
  "RushingAttempts", 
  "Penalties", 
  "PenaltyYards"
  )
  return(x)
}

new_column_names <- function(){
  x <-c(
      "ID",
      "Tm",
      "Opp",
      "Year",
      "Week",
      "Loc",
      "Tm.Score",
      "Total_Yards.Tot",
      "Total_Yards.Ply",
      "Total_Yards.Y.P",
      "Total_Yards.TO",
      "Total_Yards.ToP",
      "Pass_Stats.Cmp",
      "Pass_Stats.Att",
      "Pass_Stats.Cmp.",
      "Pass_Stats.Yds",
      "Pass_Stats.TD",
      "Pass_Stats.Int",
      "Pass_Stats.Sk",
      "Pass_Stats.SkYds",
      "Pass_Stats.Rate",
      "Rush_Stats.Yds",
      "Rush_Stats.Y.A",
      "Rush_Stats.TD",
      "Rush_Stats.Att",
      "Penalties.Pen",
      "Penalties.Yds"
  )
  return(x)
}

new_week <- read.csv(fileName, header = T) %>%
  mutate(ID = paste(Team, Opponent, Season, Week, sep = '|')) %>%
  select(keep_update_columns()) %>%
  filter(Season == 2021) %>%
  mutate(HomeOrAway = ifelse(HomeOrAway == 'HOME',1,0))

names(new_week) <- new_column_names()

complete_update <- new_week %>%
  select(-c("Tm", "Year", "Week", "Loc", "Opp", 'Tm.Score', )) %>%
  rename_with(~paste0('Opp.', .)) %>%
  inner_join(new_week, by = c('Opp.ID'='ID'), keep = T) %>%
  mutate(ID = paste(Tm, Opp, Year, Week, sep = '|'))

keep_columns <- function(){
  x <- c(
    "ID",
    "Tm", 
    "Opp", 
    "Year", 
    "Week", 
    "Loc", 
    "Tm.Score", 
    "Total_Yards.Tot", 
    "Total_Yards.Ply", 
    "Total_Yards.Y.P", 
    "Total_Yards.TO", 
    "Total_Yards.ToP",
    "Pass_Stats.Cmp", 
    "Pass_Stats.Att", 
    "Pass_Stats.Cmp.", 
    "Pass_Stats.Yds",
    "Pass_Stats.TD", 
    "Pass_Stats.Int",
    "Pass_Stats.Sk", 
    "Pass_Stats.SkYds", 
    "Pass_Stats.Rate", 
    "Rush_Stats.Yds", 
    "Rush_Stats.Y.A", 
    "Rush_Stats.TD", 
    "Rush_Stats.Att", 
    "Penalties.Pen", 
    "Penalties.Yds", 
    "Opp.Total_Yards.Tot", 
    "Opp.Total_Yards.Ply", 
    "Opp.Total_Yards.Y.P", 
    "Opp.Total_Yards.TO", 
    "Opp.Total_Yards.ToP",
    "Opp.Pass_Stats.Cmp", 
    "Opp.Pass_Stats.Att", 
    "Opp.Pass_Stats.Cmp.", 
    "Opp.Pass_Stats.Yds", 
    "Opp.Pass_Stats.TD", 
    "Opp.Pass_Stats.Int", 
    "Opp.Pass_Stats.Sk", 
    "Opp.Pass_Stats.SkYds", 
    "Opp.Pass_Stats.Rate", 
    "Opp.Rush_Stats.Yds", 
    "Opp.Rush_Stats.Y.A", 
    "Opp.Rush_Stats.TD", 
    "Opp.Rush_Stats.Att", 
    "Opp.Penalties.Pen", 
    "Opp.Penalties.Yds")
  return(x)
}

complete_update <- complete_update[keep_columns()]

new_games_data<- select(complete_update, -c('ID', 'Tm', 'Year', 'Week', 'Opp'))
new_games_data <- as.data.frame(sapply(new_games_data, as.numeric))

# Modeling
model <- load_model_tf('/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions')



