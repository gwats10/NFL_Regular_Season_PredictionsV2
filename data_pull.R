rm(list = ls())

# load libraries
library(tidyverse)
library(readxl)
library(XML)

source('/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions/my_functions.R')

wd <- "/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions/Weekly_Update"
setwd(wd)

# read in all data
folders <- c('Total_Yards', 'Pass_Stats', 'Rush_Stats', 'Penalties')

read_xml <- function(x){
  doc <- htmlParse(x)
  tableNode <- getNodeSet(doc, '//table')
  data <- XML::readHTMLTable(tableNode[[1]])
  names(data) <- make.unique(names(data))
  if("" %in% names(data)){
    data <- rename(data, Loc = "")
  }
  data$ID <- paste(data$Tm, data$Opp, data$Year, data$Week, sep = '|')

  return(data)
}

read_files <- function(folder){
  swd <- paste0(wd, '/', folder)
  filenames <- list.files(swd, pattern = '*.xls', full.names = T)
  ldf <- lapply(filenames, read_xml)
  ldf <- bind_rows(ldf)
  names(ldf) <- paste(folder, names(ldf), sep = '.')
  return(ldf)
}


combined_files_list <- sapply(folders, read_files, USE.NAMES = T)

# merge files
a <- inner_join(combined_files_list[['Total_Yards']], combined_files_list[['Pass_Stats']], by = c('Total_Yards.ID' = 'Pass_Stats.ID'))
b <- inner_join(a, combined_files_list[['Rush_Stats']], by = c('Total_Yards.ID' = 'Rush_Stats.ID'))
new_stats <- inner_join(b, combined_files_list[['Penalties']], by = c('Total_Yards.ID' = 'Penalties.ID'))
remove('a','b')

keep <- c("Total_Yards.ID", "Total_Yards.Tm", "Total_Yards.Year", "Total_Yards.Week", 
          "Total_Yards.Loc", "Total_Yards.Opp", "Total_Yards.Result", "Total_Yards.Tot", 
          "Total_Yards.Ply", "Total_Yards.Y/P", "Total_Yards.DPly", "Total_Yards.DY/P", 
          "Total_Yards.TO", "Total_Yards.ToP", 
  
          "Pass_Stats.Cmp", "Pass_Stats.Att", "Pass_Stats.Cmp%", "Pass_Stats.Yds", 
          "Pass_Stats.TD", "Pass_Stats.Int", "Pass_Stats.Sk", "Pass_Stats.Yds.1", # sacks
          "Pass_Stats.Rate", 
  
          "Rush_Stats.Yds", "Rush_Stats.Y/A", "Rush_Stats.TD", "Rush_Stats.Att", 
  
          "Penalties.Pen", "Penalties.Yds", "Penalties.OppPen", "Penalties.OppYds")

new_stats <- new_stats %>%
  select(all_of(keep))

# find missing values
apply(new_stats, 2, function(x){sum(is.na(x))})
new_stats$Rush_Stats.Att <- as.numeric(new_stats$Rush_Stats.Att)
new_stats$Rush_Stats.Att[is.na(new_stats$Rush_Stats.Att)]<-mean(new_stats$Rush_Stats.Att,na.rm=TRUE)

# rename
new_names <- c("Tm.ID", "Tm", "Year", "Week", "Loc", "Opp", "Result", 
               "Total_Yards.Tot", "Total_Yards.Ply", "Total_Yards.Y.P", "Total_Yards.DPly", 
               "Total_Yards.DY.P", "Total_Yards.TO", "Total_Yards.ToP", 
  
               "Pass_Stats.Cmp", "Pass_Stats.Att", "Pass_Stats.Cmp.", "Pass_Stats.Yds", 
               "Pass_Stats.TD", "Pass_Stats.Int", "Pass_Stats.Sk", "Pass_Stats.SkYds", # sacks
               "Pass_Stats.Rate", 
              
               "Rush_Stats.Yds", "Rush_Stats.Y.A", "Rush_Stats.TD", "Rush_Stats.Att", 
              
               "Penalties.Pen", "Penalties.Yds", "Penalties.OppPen", "Penalties.OppYds")

names(new_stats) <- new_names
new_stats <- new_stats %>%
  mutate(Loc = ifelse(Loc == '',1,0), Total_Yards.TO = ifelse(Total_Yards.TO == '',0,Total_Yards.TO))

# results parsing
t <- as.character(new_stats$Result)
split <- as.data.frame(str_split_fixed(t, ' ', 2))
split2 <- as.data.frame(str_split_fixed(split$V2, '-', 2))

Results <- data.frame(split$V1, split2$V1, split2$V2)
names(Results) <- c('Result', 'Tm.Score', 'Opp.Score')

new_stats <- cbind(Results, select(new_stats, -c('Result')))
remove(t, split, split2, Results)

# Opponent stats
#new_stats$ID <- paste(new_stats$Opp, new_stats$Tm, data$Date, sep = '|') # Opp. will be added to column name later
opp_table <- new_stats %>%
  mutate(ID = paste(new_stats$Opp, new_stats$Tm, new_stats$Year, new_stats$Week, sep = '|')) %>%
  select(-c("Tm.ID", "Tm", "Year", "Week", "Loc", "Opp", "Result", 'Tm.Score', 
            'Opp.Score', 'Penalties.OppPen', 'Penalties.OppYds')) %>%
  rename_with(~paste0('Opp.', .)) %>%
  inner_join(new_stats, by = c('Opp.ID'='Tm.ID'), keep = T) %>%
  mutate(ID = paste(Tm, Opp, Year, Week, sep = '|')) %>%
  rename()

opp_table <- opp_table[keep_columns()]

new_games_data<- select(opp_table, -c('ID',  'Opp'))
new_games_data[2:ncol(new_games_data)] <- as.data.frame(sapply(new_games_data[2:ncol(new_games_data)], fac_to_num))

#write.csv(new_games_data, '2018-2021_games_data.csv', row.names = F)

past_games_data <- read.csv('/Users/m31781/Documents/DataLyfe/NFL SB Prediction/NFL_Reg_Season_Predictions/2018-2021_games_data.csv')

games_data <- distinct(rbind(past_games_data, new_games_data))
tm_hold <- games_data$Tm
wk_hold <- games_data$Week
yr_hold <- games_data$Year
tm.score_hold <- games_data$Tm.Score
games_data <- as.data.frame(scale(select(games_data, -c(Tm, Week, Year, Tm.Score))))
games_data$Tm <- tm_hold
games_data$Week <- wk_hold
games_data$Year <- yr_hold
games_data$Tm.Score <- tm.score_hold

#write.csv(new_games_data, "2018-2021_games_data.csv", row.names = F)


