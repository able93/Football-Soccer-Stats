library(readxl)
library(reshape2)

world <- read_xlsx("world_cup2.xlsx")
worldd <- read_xlsx("worldrk.xlsx")
worldrank <- read_xlsx("wcrank.xlsx")

# 
# 
# ##############FIFA men ranking
rankk <- function(year){
  wr <- filter(worldrank, Year == year)
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
