
champs2 <- read.csv("champions_league.csv", header = FALSE)
colnames(champs2) <- c("Season",	"Year",	"Winner",	"Winner_Country",	"Winner_Score",
                      "Opponent_Score",		"Opponent",	 "Opponent_Country",	
                      "Venue",	"Attendance")


season <- function(year1,year2){
    result <- filter(champs2, Year >= year1 & Year <= year2)
    result <- select(result, Season, Winner, Winner_Score, Opponent_Score,
                     Opponent, Venue)
    colnames(result) <- c("Season", "Winner", "Winner score", "Opponent score", "Opponent", "Venue")
    print(result)
  }

graph <- function(year1,year2){
  result <- filter(champs2, Year >= year1 & Year <= year2)
  result <- group_by(result, Winner)
  pl <- ggplot(data = result) +
     geom_bar(mapping = aes(y = Winner, fill = Year)) 
  pl <- pl+labs(x="Number of Trophies",y="Winners",title = paste("Champions League Winners between", year1, "and", year2)) 
  pl + theme(plot.title = element_text(
    colour="Blue", size=20,face="bold", hjust = 0.5),
    
    axis.title.x = element_text(
      colour="Purple",size=16,face="bold"),
    
    axis.title.y = element_text(
      colour="Purple",size=16,face="bold"),
    
    axis.text = element_text(colour="black",size=14,face="bold")
  )
}




champtage <- function(year1,year2){
  result <- filter(champs2, Year >= year1 & Year <= year2)
  result <- select(result, Winner)
  result <- count(result, Winner)
  result$ptage <- round(result$n/sum(result$n)*100)
  ggplot(data = result,mapping = aes(x = reorder(Winner, -n), y = n, fill = Winner)) +
    geom_col()+
    labs(x="Winners",y="Number of Trophies",title = paste("Champions League Percentage Winners between", year1, "and", year2))+
    geom_text(aes(label = scales::percent(ptage/100), vjust = -0.2))+
    theme(plot.title = element_text(
      colour="Blue", size=20,face="bold", hjust = 0.5),
      
      axis.title.x = element_text(
        colour="Purple",size=16,face="bold"),
      
      axis.title.y = element_text(
        colour="Purple",size=16,face="bold"),
      
      axis.text = element_text(colour="black",size=14,face="bold"),
      
      axis.text.x = element_text(angle = 90),
      
      legend.position = "none"
    )
}
