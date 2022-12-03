# create season schedule file

library(readr)
schedule_2021 <- read_csv("schedule_2021.csv")
team_names <- read_csv("team_names.csv")
team_names$ABR <- toupper(team_names$ABR)

m1 <- left_join(schedule_2021, team_names, by = c('Winner/tie' = 'FULL_TEAM_NAME'))
m2 <- left_join(m1, team_names, by = c('Loser/tie' = 'FULL_TEAM_NAME'))

m2 <- m2 %>%
  select(Week, Date, ABR.x, Loc, ABR.y) %>%
  rename('Tm1' = ABR.x, 'Tm2' = ABR.y) %>%
  mutate(Loc = ifelse(is.na(Loc), 1,0))

write.csv(m2, 'cleaned_schedule.csv', row.names = F)
