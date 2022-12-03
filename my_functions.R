fac_to_num <- function(x){as.numeric(as.character(x))}

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

team_rename <- function(teamName){
  
  if(teamName == 'NO'){
    x = 'NOR'
  }else if (teamName == 'KC'){
    x = 'KAN'
  }else if (teamName == 'SF'){
    x = 'SFO'
  }else if (teamName == 'NE'){
    x = 'NWE'
  }else if (teamName == 'LV'){
    x = 'LVR'
  }else if (teamName == 'GB'){
    x = 'GNB'
  }else if (teamName == 'TB'){
    x = 'TAM'
  }else{
    x = teamName
  }
  return(x)
}

