library(readxl)
prem1 <- read_xlsx("plwinner.xlsx")
pwinner <- function(year1,year2){
  prem1$Year <- as.integer(prem1$Year)
  result <- filter(prem1, Year >= year1 & Year <= year2 
                   & is.na(Winner) == FALSE)
  result <- select(result, Year, Season, Winner)
  result
}



premgraph <- function(year1,year2){
  result <- filter(prem1, Year >= year1 & Year <= year2)
  result1 <- count(result, Winner)
  pl <- ggplot(data = result) +
    geom_bar(mapping = aes(y = Winner, fill = Season)) 
  pl <- pl+labs(x="Number of Trophies",y="Winners",title = paste("Premier League Winners between", year1, "and", year2)) 
  pl + 
    theme(plot.title = element_text(
    colour="Blue", size=20,face="bold", hjust = 0.5),
    
    axis.title.x = element_text(
      colour="Purple",size=16,face="bold"),
    
    axis.title.y = element_text(
      colour="Purple",size=16,face="bold"),
    
    axis.text = element_text(colour="black",size=14,face="bold")
  )
}


premtage <- function(year1,year2){
  result <- filter(prem1, Year >= year1 & Year <= year2)
  result <- select(result, Winner, Year, Season)
  result <- count(result, Winner)
  result$ptage <- round(result$n/sum(result$n)*100)
  ggplot(data = result,mapping = aes(x = reorder(Winner, -n), y = n, fill = Winner)) +
    geom_col()+
  labs(x="Winners",y="Number of Trophies",title = paste("Premier League Percentage Winners between", year1, "and", year2))+
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
