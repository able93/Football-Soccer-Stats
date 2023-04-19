library(readxl)
prem <- read_xlsx("data/pleague.xlsx")

pclub <- function(team_name,year){
  prem$Week <- as.integer(prem$Week)
  prem$home <- as.integer(prem$home)
  prem$away <- as.integer(prem$away)
  result <- filter(prem, (Home_team == team_name | Away_team == team_name) 
                   & Season == year)

  result <- select(result, Home_team, home, away, Away_team, Week, Comment)
  colnames(result) <- c("Home team", "home score", "away score", "Away team", "Week", "Comment")
  result
}
