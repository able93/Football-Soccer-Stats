library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(forcats)
options(shiny.sanitize.errors = FALSE)



champs <- read.csv("data/champions_league.csv")
champs2 <- read.csv("data/champions_league.csv")
prem <- read_xlsx("data/pleague.xlsx")
prem1 <- read_xlsx("data/plwinner.xlsx")
world <- read_xlsx("data/world_cup2.xlsx")
worldd <- read_xlsx("data/worldd.xlsx")
worldrank <- read_xlsx("data/wcrank.xlsx")
ptssa <-read.csv("data/ptsa.csv", header = TRUE)

source("functions/plwin.R")
source("functions/plteam.R")
source("functions/champ2.R")
source("functions/champ.R")
source("functions/wcwinner.R")
source("functions/test.R")
source("functions/ttest.R")


shinythemes::themeSelector()
ui <- fluidPage(
  
  navbarPage(
    theme = shinytheme("united"),
    h4("Soccer/Football Stats:",br(),h5("Built by Abiola Adebiyi")),
    
    tabPanel(h4("World Cup"),
             sidebarPanel(
               helpText(h3("Winner stats"),
                        selectInput("world1", h5("Winner"),
                                    choices = unique(sort(world$Winner))),
                        actionButton("actionw1", "Search")),
               br(),
               br(),
               helpText(h3("Country Ranking")),
               selectizeInput("wcyear", h5("Year"), choices = worldrank$Year),
               actionButton("actionw2", "Search"),
               
               selectizeInput("country1", h3("Ranking"),
                              choices = unique(sort(worldrank$Country),
                                               options = list(maxOptions = 3)),
                              multiple = TRUE),
               sliderInput("country2", h4(""),
                           min = 1993, max = 2022, value = c(1999, 2012), 
                           sep = ""),
               actionButton("actionw3", "Search")),
             
             
             mainPanel(
               #plotOutput("world2"),
               tableOutput("world1"),
               br(),
               plotOutput("world3"),
               br(),
               plotOutput("wcrankk")
               
             )
    ),
    
    
    tabPanel(h4("Ranking Test"),
             sidebarPanel(
               helpText(h5("The Ranking by FIFA is based on the participation of countries in the World Cup 
               tournament, Continental tournaments, and the International Friendlies. Some countries have a very
                           good FIFA ranking, however, they don't do well in the World Cup tournament. Therefore, their
                           participation in the Continental tournaments and International friendlies must have been very 
                           good to aid their FIFA ranking. The test done here therefore checks if the World Cup tournament 
                           aided a country's ranking more than the other two circumstances."),
                        selectInput("worldrt", h4("Country"),
                                    choices = unique(sort(ptssa$Country)), selected = "Argentina"),
                        
                        sliderInput("years", h4(""),
                                    min = 1993, max = 2022, value = c(2005, 2012), 
                                    sep = ""),
                        actionButton("actionwrt", "Search"))),
             
             mainPanel(
               #tableOutput("world1"),
               #br(),
               plotOutput("rankt1"),
               tableOutput("rankt2"),
               textOutput("rankt3"),
               br(),
               textOutput("rankt4"),
               br(),
               verbatimTextOutput("rankt5"),
               br(),
               br()
             )
    ),
    
    
    
    tabPanel(h4("Champions League"),
             sidebarPanel(
               radioButtons("what", h3(""),
                            choices = list("Winner" = 1)),
               
               selectInput("club", h4("Club name"), 
                           choices = unique(sort(champs$Winner)), selected = "Ajax"),
               actionButton("actionbar_champ", "Search"),
               br(),
               br(),
               br(),
               sliderInput("champe", h4("Season winners"),
                           min = 1956, max = 2022, value = c(1999, 2012), 
                           sep = ""),
               actionButton("actionbar_champ2", "Search")
             ),
             mainPanel(
               tableOutput("champe"),
               plotOutput("champe3"),
               br(),
               plotOutput("champe4"),
               tableOutput("champe2")
             ),
    ),
    
    tabPanel(h4("Premier League"),
             sidebarPanel(
               selectInput("prclub", h3("Club search"),
                           choices = unique(sort(prem$Home_team))),
               selectInput("premseason", h5("season"),
                           choices = unique(sort(prem$Season))),
               actionButton("action_prclub", "Search"),
               br(),
               br(),
               
               sliderInput("premwin", h3("Winner"), min = 1889, max = 2022, 
                           value = c(1985, 2018), sep = ""),
               actionButton("action_prem_win", "search")
               
             ),
             mainPanel(
               plotOutput("premgraph"),
               br(),
               plotOutput("premtage"),
               tableOutput("pwin"),
               tableOutput("pclub")
               
             )
             
    ),
  )
)




server <- function(input, output, session) {
  
  ###############################   Champions league club search
  
  dataInput_champ <- eventReactive(input$actionbar_champ,{
    source("functions/champ.R")
    winner(input$club)
  })
  dataInput_champ <- data.table::copy(dataInput_champ)
  output$champe<- renderTable({
    (dataInput_champ())
  })
  
  ###############################  champions league winner search
  
  dataInput_champ2 <- eventReactive(input$actionbar_champ2,{
    source("functions/champ2.R")
    season(input$champe[1],input$champe[2])
  })
  dataInput_champ2 <- data.table::copy(dataInput_champ2)
  output$champe2<- renderTable({
    (dataInput_champ2())
  })
  
  dataInput_champ3 <- eventReactive(input$actionbar_champ2,{
    source("functions/champ2.R")
    graph(input$champe[1],input$champe[2])
  })
  
  output$champe3<- renderPlot({
    (dataInput_champ3())
  })
  
  dataInput_champ4 <- eventReactive(input$actionbar_champ2,{
    source("functions/champ2.R")
    champtage(input$champe[1],input$champe[2])
  })
  
  output$champe4<- renderPlot({
    (dataInput_champ4())
  })
  
  #################################   World cup
  dataInput_world1 <- eventReactive(input$actionw1,{
    source("functions/wcwinner.R")
    wcwinner(input$world1)
  })
  dataInput_world1 <- data.table::copy(dataInput_world1)
  output$world1<- renderTable({
    (dataInput_world1())
  })
  
  
  ###graph###
  dataInput_world2 <- eventReactive(input$actionw1,{
    source("functions/wcwinner.R")
    wcwinner2(input$world1)
  })
  output$world2<- renderPlot({
    (dataInput_world2())
  })
  
  
  ###country graph###
  dataInput_world3 <- eventReactive(input$actionw2,{
    source("functions/wcwinner.R")
    #ranking(input$rank,input$wcyear)
    rankk(input$wcyear)
  })
  output$world3<- renderPlot({
    (dataInput_world3())
  })
  
  
  #####FIFA Ranking######
  dataInput_wcrankk <- eventReactive(input$actionw3,{
    source("functions/test.R")
    rank2(input$country2[1],input$country2[2],input$country1[1],
          input$country1[2],input$country1[3])
  })
  output$wcrankk<- renderPlot({
    (dataInput_wcrankk())
  })
  
  
  
  #####Ranking Test######
  dataInput_rankt1 <- eventReactive(input$actionwrt,{
    source("functions/ttest.R")
    ttest1(input$years[1],input$years[2],input$worldrt)
  })
  output$rankt1<- renderPlot({
    (dataInput_rankt1())
  })
  
  dataInput_rankt2 <- eventReactive(input$actionwrt,{
    source("functions/ttest.R")
    ttest2(input$years[1],input$years[2],input$worldrt)
  })
  dataInput_rankt2 <- data.table::copy(dataInput_rankt2)
  output$rankt2<- renderTable({
    (dataInput_rankt2())
  })
  
  
  
  dataInput_rankt3 <- eventReactive(input$actionwrt,{
    source("functions/ttest.R")
    ttest3(input$years[1],input$years[2],input$worldrt)
  })
  output$rankt3<- renderText({
    (dataInput_rankt3())
  })
  
  dataInput_rankt4 <- eventReactive(input$actionwrt,{
    source("functions/ttest.R")
    ttest4(input$years[1],input$years[2],input$worldrt)
  })
  output$rankt4<- renderText({
    (dataInput_rankt4())
  })
  
  
  dataInput_rankt5 <- eventReactive(input$actionwrt,{
    source("functions/ttest.R")
    ttest5(input$years[1],input$years[2],input$worldrt)
  })
  output$rankt5<- renderPrint({
    (dataInput_rankt5())
  })
  
  ####################################### premier league win
  
  dataInput_pclub <- eventReactive(input$action_prclub,{
    source("functions/plteam.R")
    pclub(input$prclub, input$premseason)
  })
  dataInput_pclub <- data.table::copy(dataInput_pclub)
  output$pclub<- renderTable({
    (dataInput_pclub())
  })
  
  
  
  ##########################################  premier league team
  
  dataInput_pwin <- eventReactive(input$action_prem_win,{
    source("functions/plwin.R")
    pwinner(input$premwin[1],input$premwin[2])
  })
  dataInput_pwin <- data.table::copy(dataInput_pwin)
  output$pwin<- renderTable({
    (dataInput_pwin())
  })
  
  dataInput_premgraph <- eventReactive(input$action_prem_win,{
    source("functions/plwin.R")
    premgraph(input$premwin[1],input$premwin[2])
  })
  
  output$premgraph<- renderPlot({
    (dataInput_premgraph())
  })
  
  dataInput_premtage <- eventReactive(input$action_prem_win,{
    source("functions/plwin.R")
    premtage(input$premwin[1],input$premwin[2])
  })
  
  output$premtage<- renderPlot({
    (dataInput_premtage())
  })
  
}
shinyApp(ui, server)
