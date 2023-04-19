library(readxl)
library(reshape2)
library(data.table)

world <- read_xlsx("world_cup2.xlsx")
worldd <- read_xlsx("worldrk.xlsx")
worldrank <- read_xlsx("wcrank.xlsx")
ptssa <-read.csv("ptsa.csv", header = TRUE)

ttest1 <- function(year1, year2, name){
  worlda <- filter(ptssa, Year >= year1 & Year <= year2)
  worlda <- filter(worlda, Country == name)
  worlda <- select(worlda, Ranka, ptsa, Year)

  worldf <- filter(worldrank, Year >= year1 & Year <= year2)
  worldf <- filter(worldf, Country == name)
  worldf <- select(worldf, Rankf, ptsf, Year)
  dat <- full_join(worldf,worlda, by = "Year")
  datt <- select(dat, Year, ptsf, ptsa)
  dattt <- select(dat, ptsf, ptsa)
  datt <- arrange(datt, Year)
  colnames(datt) <- c("Year", "FIFA pts","Appearance pts")
  #colnames(dat) <- c("FIFA Ranking","FIFA pts", "Year", "WC Appearance Ranking", "Appearance pts")
  print(datt)
  print(t.test(dattt$ptsf,dattt$ptsa))


  ggplot(data = dat)+
    geom_line(mapping = aes(x = Year, y = ptsf, color = "FIFA"), lwd = 2.0)+
    geom_line(mapping = aes(x = Year, y = ptsa,  color = "World Cup Appearances"), lwd = 2.0)+
    geom_text(mapping = aes(x = Year, y = ptsa, label = Ranka, vjust = 2.5, hjust = 1.5))+
    geom_text(mapping = aes(x = Year, y = ptsf, label = Rankf, vjust = 2.5, hjust = 1.5))+
    scale_x_continuous(limits=c(year1, year2), breaks = seq(year1, year2, 1),
                       expand = c(0, 0)) +
    scale_y_continuous(limits=c(min(dattt$ptsf,dattt$ptsa)-0.1,max(dattt$ptsf,dattt$ptsa)+0.1), breaks=seq(0.0, 1.0, 0.2),
                       expand = c(0, 0))+
  labs(y="Point Ratio",x="Year", colour = "Ranking by", title = paste("Comparing of ranking by FIFA, and World cup appearances between", year1, "and",year2))+
    theme(plot.title = element_text(
    colour="Blue", size=15,face="bold", hjust = 0.5),

    axis.text = element_text(colour="black",size=14,face="bold"),

    legend.text = element_text(size = 10, face = "bold"),

    axis.title.x = element_text(
      colour="Purple",size=16,face="bold"),

    axis.title.y = element_text(
      colour="Purple",size=16,face="bold"),

  )

}

ttest2 <- function(year1, year2, name){
  worlda <- filter(ptssa, Year >= year1 & Year <= year2)
  worlda <- filter(worlda, Country == name)
  worlda <- select(worlda, Ranka, ptsa, Year)
  
  worldf <- filter(worldrank, Year >= year1 & Year <= year2)
  worldf <- filter(worldf, Country == name)
  worldf <- select(worldf, Rankf, ptsf, Year)
  dat <- full_join(worldf,worlda, by = "Year")
  #dat <- data.frame(dat)
  datt <- select(dat, Year, ptsf, ptsa)
  datt <- arrange(datt, Year)
  datt$Year <- as.integer(datt$Year)
  dattt <- c("Mean", mean(datt$ptsf), mean(datt$ptsa))
  dattp <- c()
  datt <- rbind(datt,dattt)
  colnames(datt) <- c("Year", "FIFA pts","Appearance pts")
  print(datt)
  
}



ttest3 <- function(year1, year2, name){
  worlda <- filter(ptssa, Year >= year1 & Year <= year2)
  worlda <- filter(worlda, Country == name)
  worlda <- select(worlda, Ranka, ptsa, Year)
  
  worldf <- filter(worldrank, Year >= year1 & Year <= year2)
  worldf <- filter(worldf, Country == name)
  worldf <- select(worldf, Rankf, ptsf, Year)
  dat <- full_join(worldf,worlda, by = "Year")
  dattt <- select(dat, ptsf, ptsa)
  pvalue <- (t.test(dattt$ptsf,dattt$ptsa))$p.value
  print(paste("p-value = ",pvalue))
}



ttest4 <- function(year1, year2, name){
  worlda <- filter(ptssa, Year >= year1 & Year <= year2)
  worlda <- filter(worlda, Country == name)
  worlda <- select(worlda, Ranka, ptsa, Year)
  
  worldf <- filter(worldrank, Year >= year1 & Year <= year2)
  worldf <- filter(worldf, Country == name)
  worldf <- select(worldf, Rankf, ptsf, Year)
  dat <- full_join(worldf,worlda, by = "Year")
  dattt <- select(dat, ptsf, ptsa)
  pvalue <- (t.test(dattt$ptsf,dattt$ptsa))$p.value
  if (pvalue > 0.05)
  {print(paste("Since the p-value is greater than 0.05, then the World Cup strongly influenced the FIFA ranking of", name, "between", year1, "and", year2))}
  else{ 
    print(paste("Since the p-value is less than 0.05, then the Continental tournaments and International friendlies strongly influenced the FIFA ranking of", name, "between", year1, "and", year2))
  }
}



ttest5 <- function(year1, year2, name){
  worlda <- filter(ptssa, Year >= year1 & Year <= year2)
  worlda <- filter(worlda, Country == name)
  worlda <- select(worlda, Ranka, ptsa, Year)
  
  worldf <- filter(worldrank, Year >= year1 & Year <= year2)
  worldf <- filter(worldf, Country == name)
  worldf <- select(worldf, Rankf, ptsf, Year)
  dat <- full_join(worldf,worlda, by = "Year")
  dattt <- select(dat, ptsf, ptsa)
  result <- t.test(dattt$ptsf,dattt$ptsa)
}
