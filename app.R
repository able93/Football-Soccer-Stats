library(shiny)
library(readxl)
library(tidyverse)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
options(shiny.sanitize.errors = FALSE)



champs <- read.csv("champions_league.csv")
champs2 <- read.csv("champions_league.csv")
prem <- read_xlsx("pleague.xlsx")
prem1 <- read_xlsx("plwinner.xlsx")
world <- read_xlsx("world_cup2.xlsx")
worldd <- read_xlsx("worldd.xlsx")
worldrank <- read_xlsx("wcrank.xlsx")

source("plwin.R")
source("plteam.R")
source("champ2.R")
source("champ.R")
source("wcwinner.R")
source("test.R")


shinythemes::themeSelector()
ui <- fluidPage(
  
  navbarPage(
    theme = shinytheme("united"),
    h4("Football Stats:",br(),h5("Built by Able Lincoln")),
    
    tabPanel(h4("World Cup"),
             sidebarPanel(
               helpText(h3("Winner stats"),
                        selectInput("world1", h5("Winner"),
                                    choices = unique(sort(world$Winner))),
                        actionButton("actionw1", "Search")),
               br(),
               br(),
               helpText(h3("Country Ranking")),
               # radioButtons("rank", h6(""),
               #              choices = list("Ranking by number of games won" = 1, 
               #                             "Ranking by appearances" = 2), selected = 1),
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
               tableOutput("pwin"),
               tableOutput("pclub")
               
             )
             
    ),
  )
)



server <- function(input, output, session) {
  
  ###############################   Champions league club search
  
  dataInput_champ <- eventReactive(input$actionbar_champ,{
    source("champ.R")
    winner(input$club)
  })
  dataInput_champ <- data.table::copy(dataInput_champ)
  output$champe<- renderTable({
    (dataInput_champ())
  })
  
  ###############################  champions league winner search
  
  dataInput_champ2 <- eventReactive(input$actionbar_champ2,{
    source("champ2.R")
    season(input$champe[1],input$champe[2])
  })
  dataInput_champ2 <- data.table::copy(dataInput_champ2)
  output$champe2<- renderTable({
    (dataInput_champ2())
  })
  
  dataInput_champ3 <- eventReactive(input$actionbar_champ2,{
    source("champ2.R")
    graph(input$champe[1],input$champe[2])
  })
  
  output$champe3<- renderPlot({
    (dataInput_champ3())
  })
  
  # output$champee2 <- renderText({
  #   paste("you have selected", input$champe[1],
  #         input$champe[2])
  # })
  
  #################################   World cup
  dataInput_world1 <- eventReactive(input$actionw1,{
    source("wcwinner.R")
    wcwinner(input$world1)
  })
  dataInput_world1 <- data.table::copy(dataInput_world1)
  output$world1<- renderTable({
    (dataInput_world1())
  })
  
  
  ###graph###
  dataInput_world2 <- eventReactive(input$actionw1,{
    source("wcwinner.R")
    wcwinner2(input$world1)
  })
  output$world2<- renderPlot({
    (dataInput_world2())
  })
  
  
  ###country graph###
  dataInput_world3 <- eventReactive(input$actionw2,{
    source("wcwinner.R")
    #ranking(input$rank,input$wcyear)
    rankk(input$wcyear)
  })
  output$world3<- renderPlot({
    (dataInput_world3())
  })
  
  
  #####FIFA Ranking######
  dataInput_wcrankk <- eventReactive(input$actionw3,{
    source("test.R")
    rank2(input$country2[1],input$country2[2],input$country1[1],
          input$country1[2],input$country1[3])
  })
  output$wcrankk<- renderPlot({
    (dataInput_wcrankk())
  })
  
  ####################################### premier league win
  
  dataInput_pclub <- eventReactive(input$action_prclub,{
    source("plteam.R")
    pclub(input$prclub, input$premseason)
  })
  dataInput_pclub <- data.table::copy(dataInput_pclub)
  output$pclub<- renderTable({
    (dataInput_pclub())
  })
  
  
  ##########################################  premier league team
  
  dataInput_pwin <- eventReactive(input$action_prem_win,{
    source("plwin.R")
    pwinner(input$premwin[1],input$premwin[2])
  })
  dataInput_pwin <- data.table::copy(dataInput_pwin)
  output$pwin<- renderTable({
    (dataInput_pwin())
  })
  
  dataInput_premgraph <- eventReactive(input$action_prem_win,{
    source("plwin.R")
    premgraph(input$premwin[1],input$premwin[2])
  })
  
  output$premgraph<- renderPlot({
    (dataInput_premgraph())
  })
  
}
shinyApp(ui, server)