library(readxl)
prem1 <- read_xlsx("plwinner.xlsx")
#colnames(prem) <- c("Season",	"Winner",	"Home team",	"home",
                      #"away",		"Away team", "Comment")
pwinner <- function(year1,year2){
  prem1$Year <- as.integer(prem1$Year)
  result <- filter(prem1, Year >= year1 & Year <= year2 
                   & is.na(Winner) == FALSE)
  result <- select(result, Year, Season, Winner)
  result
}



premgraph <- function(year1,year2){
  result <- filter(prem1, Year >= year1 & Year <= year2)
  #result <- group_by(result, Winner)
  pl <- ggplot(data = result) +
    geom_bar(mapping = aes(y = Winner, fill = Season)) 
  pl <- pl+labs(x="Number of Trophies",y="Winners",title = paste("Premier League Winners between", year1, "and", year2)) 
  pl + theme(plot.title = element_text(
    colour="Blue", size=20,face="bold", hjust = 0.5),
    
    axis.title.x = element_text(
      colour="Purple",size=16,face="bold"),
    
    axis.title.y = element_text(
      colour="Purple",size=16,face="bold"),
    
    axis.text = element_text(colour="black",size=14,face="bold")
  )
}
