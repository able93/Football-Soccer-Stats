library(readxl)
library(reshape2)

world <- read_xlsx("world_cup2.xlsx")
worldd <- read_xlsx("worldd.xlsx")
worldrank <- read_xlsx("wcrank.xlsx")

#colnames(world) <- c(opponent1, opponent1_score, 
                      #opponent2_score, opponent2, winner_team)


wcwinner <- function(team_name){
  world$Winner_score <- as.integer(world$Winner_score)
  world$Opponent_score <- as.integer(world$Opponent_score)
  world$Attendance_number <- as.integer(world$Attendance_number)
  world$Year <- as.integer(world$Year)
  world <- as.data.frame(world)
  result <- filter(world, Winner == team_name)
  result <- select(result, Year, Winner, Winner_score,
                   Opponent_score, Opponent, Attendance_number, Referee_name)
  colnames(result) <- c("Year", "Winner", "Winner score", "Opponent score", "Opponent",
                        "Attendance", "Referee")
  result
}


# 
# wcwinner2 <- function(team_name){
#   world$Winner_score <- as.integer(world$Winner_score)
#   world$Opponent_score <- as.integer(world$Opponent_score)
#   world$Year <- as.integer(world$Year)
#   result <- filter(world, Winner == team_name)
#   result <- as.data.frame(result)
#   result <- select(result, Winner_score,Opponent_score,Opponent,Winner,Year)
#   k <- data.frame(Year = c(result$Year,result$Year),
#                   score = c(result$Opponent_score, result$Winner_score),
#                   country = c(result$Opponent, result$Winner))
#   #result <- gather(result, key = "country1", value = "score", 1:2)
#   #result <- gather(result, key = "who", value = "country", 1:2)
# ggplot(data = k,aes(x = factor(Year), y = score, fill = country))+
#   geom_col(position = "dodge")+
#   #geom_text(aes(label = score, vjust = 2.5, hjust = 1.5))+
#   labs(x="Year",y="Final score",title = paste("World Cup finals played and won by",team_name))+
#  theme(plot.title = element_text(
#   colour="Blue", size=20,face="bold", hjust = 0.5),
# 
#   axis.text = element_text(colour="black",size=16,face="bold"),
# 
#   axis.title.x = element_text(
#     colour="Purple",size=16,face="bold"),
# 
#   axis.title.y = element_text(
#     colour="Purple",size=16,face="bold")
# )
# 
# }




# wcwinner2 <- function(team_name){
#   result <- filter(world, Winner == team_name)
#   resultt <- tail(result, 1)
#   yearr <- as.vector(resultt[1,1])
#   tab <- filter(world, Year <= yearr)
# pl <- ggplot(data = tab) +
#   geom_bar(mapping = aes(Winner, fill = Year))
# 
# pl <- pl+labs(x="Country",y="Number of Trophies",title = paste("Number of trophies won by countries as at when",team_name, "
#       won the World Cup in", yearr))
# pl + theme(plot.title = element_text(
#   colour="Blue", size=20,face="bold", hjust = 0.5),
# 
#   axis.title.x = element_text(
#     colour="Purple",size=16,face="bold"),
# 
#   axis.title.y = element_text(
#     colour="Purple",size=16,face="bold")
# )
# }


# ranking <- function(rank,year2){
#   if (rank == 1)
#  { worldf <- filter(worldd, Year <= year2)
# worldf <- worldf[complete.cases(worldf), ]
# 
# pl <- ggplot(data = worldf) +
#   geom_bar(mapping = aes(y = winner_team))
# pl <- pl+labs(x="Number of Games won",y="Match Winners",title = paste("Ranking of countries by matches won as at", year2)) 
# pl + theme(plot.title = element_text(
#   colour="Blue", size=20,face="bold", hjust = 0.5),
#   
#   axis.title.x = element_text(
#     colour="Purple",size=16,face="bold"),
#   
#   axis.title.y = element_text(
#     colour="Purple",size=16,face="bold")
# )
#   }
#  else 
#  {
#      worldf <- filter(worldd, Year <= year2)
#      worldf <- worldf[complete.cases(worldf), ]
#      
#    pl <-  ggplot(data = worldf) +
#        geom_bar(mapping = aes(y = Country))
#    pl <- pl+labs(x="Number of Appearances",y="Country",title = paste("Ranking of countries by World Cup appearances as at", year2)) 
#    pl + theme(plot.title = element_text(
#      colour="Blue", size=20,face="bold", hjust = 0.5),
#      
#      axis.title.x = element_text(
#        colour="Purple",size=16,face="bold"),
#      
#      axis.title.y = element_text(
#        colour="Purple",size=16,face="bold")
#    )
#    }
#  }
# 
# 
# ##############FIFA men ranking
rankk <- function(year){
  wr <- filter(worldrank, Year == year)
  wr <- as.data.frame(wr)
  wr <- wr[order(wr$Ratio, decreasing= TRUE),]
  wr$Position <- as.character(wr$Position)
  pl <- ggplot(data = wr, mapping = aes(x = (reorder(Position, Rank)),
                                        y = Ratio, fill = "orange")) +
    geom_col()+
    coord_cartesian(ylim = c(0, 1.0))+
    scale_x_discrete(labels=as.character(c(1:50)))+
    geom_text(aes(label = Country), hjust = 1.0, vjust = 1.0,
              color = "blue", angle = 75)

  pl <- pl+labs(y="Point Ratio",x="Ranking",title = paste("FIFA Men's Ranking in", year))
  pl + theme(plot.title = element_text(
    colour="Blue", size=20,face="bold", hjust = 0.5),

    axis.text = element_text(colour="black",size=10,face="bold"),

    axis.title.x = element_text(
      colour="Purple",size=16,face="bold"),

    axis.title.y = element_text(
      colour="Purple",size=16,face="bold"),

    legend.position = "none"

  )

}
