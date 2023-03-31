library(readxl)
prem <- read_xlsx("pleague.xlsx")
#colnames(prem) <- c("Season",	"Winner",	"Home team",	"home",
#"away",		"Away team", "Comment")
pclub <- function(team_name,year){
  prem$Week <- as.integer(prem$Week)
  prem$home <- as.integer(prem$home)
  prem$away <- as.integer(prem$away)
  result <- filter(prem, (Home_team == team_name | Away_team == team_name) 
                   & Season == year)
  #if (is.null(result) == FALSE){
    #print("The team did not participate in this season")
  #}
  result <- select(result, Home_team, home, away, Away_team, Week, Comment)
  colnames(result) <- c("Home team", "home score", "away score", "Away team", "Week", "Comment")
  result
}
