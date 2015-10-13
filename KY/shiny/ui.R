
library(shiny)
#inicializando os valores dos campos
rh<-93
tmin<-15
topt<-27
tmax<-35
# startingDate <- Sys.Date()
# endDate <- Sys.Date()
startingDate <- "2004-04-01"
startingDate2 <- "2011-04-01"
shinyUI(fluidPage(
  titlePanel("Weather-Based Wheat Blast Risk Factors"),
  sidebarLayout(
    sidebarPanel(   
      selectInput("station1", "Brazil", 
                  choices=choicesStationBrasil),  
      dateInput("startingDate1", "Starting Date:", value=startingDate),             
      selectInput("station2", "USA", 
                  choices=choicesStationEUA),      
      dateInput("startingDate2", "Starting Date:", value=startingDate2)            
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Paraná, Brazil", plotOutput("grafico1",width = "100%", height = "700px")),                   
                  tabPanel("Kentucky, USA", plotOutput("grafico2",width = "100%", height = "700px")),     
                  tabPanel("Data Paraná", dataTableOutput("tabelaGrafico1"))
                  ,tabPanel("Data Kentucky", dataTableOutput("tabelaGrafico2"))
                  
      )
    )   
  )))