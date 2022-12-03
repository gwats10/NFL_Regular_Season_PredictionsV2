# scoring script
library(stringr)

setwd("/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions")

 NFL_week <- 19
NFL_score_week <- NFL_week - 1
score_week_start <- week_start - 7

# get last predictions
# last_predictions_name <- paste0('Predictions/Predictions_', score_week_start, '_W', NFL_score_week, '.csv')
# last_predictions <- read.csv(last_predictions_name, stringsAsFactors = F)

# last_predictions <- prediction_df
 last_predictions <- read.csv('Predictions/Predictions_2022-01-10_W18.csv', stringsAsFactors = F)

split <- as.data.frame(str_split_fixed(last_predictions$Matchup, '-', 2))
last_predictions$Matchup <- NULL
last_predictions <- cbind(split, last_predictions)
names(last_predictions) <- c("AwayTm", "HomeTm", "T1.Score", "T2.Score")
last_predictions$AwayTm <- as.character(last_predictions$AwayTm)
last_predictions$HomeTm <- as.character(last_predictions$HomeTm)

last_predictions <- last_predictions %>% mutate(P.Winner = ifelse(T1.Score > T2.Score, AwayTm, HomeTm))

# check for bad team names
last_predictions$AwayTm <- sapply(last_predictions$AwayTm, team_rename)
last_predictions$HomeTm <- sapply(last_predictions$HomeTm, team_rename)

# create scoring dataset
scoring_data <- last_predictions %>%
  left_join(new_games_data[new_games_data$Week == NFL_score_week,], by = c('AwayTm' = 'Tm')) %>%
  left_join(new_games_data[new_games_data$Week == NFL_score_week,], by = c('HomeTm' = 'Tm')) %>%
  select(AwayTm, HomeTm, T1.Score, T2.Score, Tm.Score.x, Tm.Score.y) %>%
  mutate(A1.Diff = round(abs(Tm.Score.x - T1.Score),2), 
         A2.Diff = round(abs(Tm.Score.y - T2.Score),2),
         P.Winner = ifelse(T1.Score > T2.Score, AwayTm, HomeTm),
         A.Winner = ifelse(Tm.Score.x > Tm.Score.y, AwayTm, HomeTm),
         Correct.Winner = ifelse(P.Winner == A.Winner, 1, 0))
names(scoring_data) <- c('AwayTm', 'HomeTm', 'P.Score1', 'P.Score2', 'Actual.Score1', 'Actual.Score2',  
                         'A1.Diff', 'A2.Diff', 'P.Winner', 'A.Winner', 'C.Winner')

# performance
mean(scoring_data$A1.Diff)
mean(scoring_data$A2.Diff)
sum(scoring_data$C.Winner)
sum(scoring_data$C.Winner) / nrow(scoring_data)

write.csv(scoring_data, 'Scoring_Week_18.csv', row.names = F)

