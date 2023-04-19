library(readxl)
library(reshape2)

world <- read_xlsx("world_cup2.xlsx")
worldd <- read_xlsx("worldd.xlsx")
worldrank <- read_xlsx("wcrank.xlsx")

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
# ##############FIFA men ranking
rankk <- function(year){
  wr <- filter(worldrank, Year == year & Position >= 1 & Position <= 50)
  wr <- as.data.frame(wr)
  wr <- wr[order(wr$ptsf, decreasing= TRUE),]
  wr$Position <- as.character(wr$Position)
  pl <- ggplot(data = wr, mapping = aes(x = (reorder(Position, Rankf)),
                                        y = ptsf, fill = "orange")) +
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
