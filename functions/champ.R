
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


#return(result)
  #datatable(result)
#result


#R> substr("leftright",1,4)
#[1] "left"                     #To pick substrings
#R> str_sub("leftright",-5,-1)
#[1] "right"  

#To convert from lowercase to uppercase
#toupper("ade")

#to join strings
#paste("ade","fola",sep = "")
#adefola