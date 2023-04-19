
champs <- read.csv("champions_league.csv", header = FALSE)
colnames(champs) <- c("Season",	"Year",	"Winner",	"Winner_Country",	"Winner_Score",
                      "Opponent_Score",		"Opponent",	 "Opponent_Country",	
                      "Venue",	"Attendance")
winner <- function(team_name){
  result <- filter(champs, Winner == team_name)
result <- select(result, Season, Winner, Winner_Score, Opponent_Score, 
                   Opponent, Venue)
colnames(result) <- c("Season", "Winner", "Winner score", "Opponent score", "Opponent", "Venue")
result
}

