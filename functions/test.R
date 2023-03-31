library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
worldd <- read_xlsx("worldd.xlsx")
worldrank <- read_xlsx("wcrank.xlsx")




rank2 <- function(year1,year2,country1,country2,country3){
  
  if (is.na(country3) == TRUE){
    wr <- filter(worldrank, Country == country1 | Country == country2 &
                   Year >= year1 & Year <= year2)
  }
  else if (is.na(country2) == TRUE){
    
    wr <- filter(worldrank, Country == country1 &
                   Year >= year1 & Year <= year2)
    wr <- as.data.frame(wr)
  }
  else {
    wr <- filter(worldrank, Country == country1 | Country == country2 |
                   Country == country3 &
                   Year >= year1 & Year <= year2)
    wr <- as.data.frame(wr)
    wr[complete.cases(wr), ]
  }
  
  ggplot(data = wr,aes(x = Year, y = Ratio, color = Country))+
    geom_line(size = 1)+
    scale_x_continuous(limits=c(year1, year2), breaks = seq(year1, year2, 1),
                       expand = c(0, 0)) +
    scale_y_continuous(limits=c(0.0,1.2), breaks=seq(0.0, 1.0, 0.2), 
                       expand = c(0, 0))+
    
    geom_text(aes(label = Rank, vjust = 2.5, hjust = 1.5))+
    labs(x="Year",y="Point Ratio",title = paste("Ranking of",country1,',' 
                                                ,country2,',', country3))+
   theme(plot.title = element_text(
    colour="blue", size=18,face="bold", hjust = 0.5),
    
    legend.text = element_text(size = 14, face = "bold"),

    axis.text = element_text(colour="black",size=14,face="bold"),

    axis.title.x = element_text(
      colour="Purple",size=16,face="bold"),

    axis.title.y = element_text(
      colour="Purple",size=16,face="bold"))
  
}



# ranking <- function(year2){
# worldf <- filter(worldd,  Year >= 1986 & Year <= year2)
#   worldf <- worldf[complete.cases(worldf), ]
#   worldf <- group_by(worldf, Year, Country)
#   f <- count(worldf, winner_team)
#   g <- count(worldf, Country)
#   f <- aggregate(f$n, list(f$winner_team), FUN=sum)
#   #g <- aggregate(g$n, list(g$Country), FUN=sum)
#   #k <- merge(f, g, by=c("Group.1"))
#   f <- f[order(f$x, decreasing= TRUE),]
#   f <- mutate(f, ratio = x/max(x))
#   a <- nrow(f)
#   k <- 1:a
#   f <- data.frame(f, k)
#   f
# }


# rank2 <- function(year1,year2,country1){
# 
# 
#       wr <- filter(worldrank, Country == country1 &
#                      Year >= year1 & Year <= year2)
#       
#   worldrank$Year <- as.integer(worldrank$Year)
#   wr <- as.data.frame(wr)
#   ggplot(data = wr,aes(x = Year, y = Ratio, color = Country))+
#     geom_line()+
#     scale_x_continuous(limits=c(year1, year2), breaks = seq(year1, year2, 1),
#                        expand = c(0, 0)) + 
#     scale_y_continuous(limits=c(0.0,1.0), breaks=seq(0.0, 1.0, 0.2), expand = c(0, 0))
# }


