library(shiny)
library(ggplot2)
library(DBI)
library(RPostgreSQL)
library(ff)
library(reshape2)

shinyServer(function(input, output) {
  
  ## brasil
  output$grafico1 <- renderPlot({  
    withProgress(message = 'Making plot', value = 0, {
    ## capturando o código da estação que foi selecionada      
    codigo_station<-input$station1
    incProgress(1, detail = paste("Reading the database"))    

    df <- carregaDadosClimaHCP(codigo_station,input$startingDate1)
    
    df$day <- as.Date(df$datahora,"%Y-%m-%d")
    df$time <- format(df$datahora, "%H:%M:%S")
    year <- format(df$day, "%Y") # year
    year<-year[1]
    
    # Function that calculates hcp
#     hcp<- function(temp,rh) { # verificar
#       ifelse(temp >= 15 && temp < 27 && rh > 93,
#              1/(14.35-0.25*temp),
#              ifelse(temp >= 27 && temp < 35 && rh > 93,1/(-8.5+0.59*temp),0))
#     }
#     hcp<- function(temp,rh) { # verificar
#       ifelse(temp >= input$tmin && temp < input$topt && rh > input$rh,
#              1/(14.35-0.25*temp),
#              ifelse(temp >= input$topt && temp < input$tmax && rh > 93,1/(-8.5+0.59*temp),0))
#     }
  # Increment the progress bar, and update the detail text.
  incProgress(2, detail = paste("Calculating HCP"))
  
    dfTable <- df    
    dfTable <- dfTable[,c("datahora","temp","rh","rain","hcp","chcp")]
    df <- melt(df, id.vars=c("day", "time"))
    
    f<-subset(df,variable %in% c("temp", "rh", "rain","hcp", "chcp"))
    #dadosTabela1 <<- f
    dadosTabela1 <<- dfTable
    x<-paste(f$day,f$time)
    z<<-strptime(x, "%Y-%m-%d %H") 
    
    xlab<-year
    mt <- ggplot(f, aes(z, value)) +  geom_line()
    mt + facet_grid(variable~., scales = "free_y")+
      theme(text = element_text(size=20),
            axis.text.x = element_text(angle=90, vjust=1)) +
      xlab(paste("Day of Year", year)) +
      ylab("Value") +      
      #ggtitle(paste("Location of -",station$nome,"- in the state of ",station$state,",", station$country))
      ggtitle(paste("Location of -",names(choicesStationBrasil[choicesStationBrasil==input$station1])))
    })
  })
  
   output$grafico2 <- renderPlot({  
     withProgress(message = 'Making plot', value = 0, {
       ## capturando o código da estação que foi selecionada      
       codigo_station<-input$station2
       incProgress(1, detail = paste("Reading the database"))    
       
       df <- carregaDadosClimaHCP(codigo_station,input$startingDate2)
       
       df$day <- as.Date(df$datahora,"%Y-%m-%d")
       df$time <- format(df$datahora, "%H:%M:%S")
       year <- format(df$day, "%Y") # year
       year<-year[1]
       
       # Function that calculates hcp
       #     hcp<- function(temp,rh) { # verificar
       #       ifelse(temp >= 15 && temp < 27 && rh > 93,
       #              1/(14.35-0.25*temp),
       #              ifelse(temp >= 27 && temp < 35 && rh > 93,1/(-8.5+0.59*temp),0))
       #     }
       #     hcp<- function(temp,rh) { # verificar
       #       ifelse(temp >= input$tmin && temp < input$topt && rh > input$rh,
       #              1/(14.35-0.25*temp),
       #              ifelse(temp >= input$topt && temp < input$tmax && rh > 93,1/(-8.5+0.59*temp),0))
       #     }
       # Increment the progress bar, and update the detail text.
       incProgress(2, detail = paste("Calculating HCP"))
       
       dfTable <- df    
       dfTable <- dfTable[,c("datahora","temp","rh","rain","hcp","chcp")]
       df <- melt(df, id.vars=c("day", "time"))
       
       f<-subset(df,variable %in% c("temp", "rh", "rain","hcp", "chcp"))
       #dadosTabela1 <<- f
       dadosTabela2 <<- dfTable
       x<-paste(f$day,f$time)
       z2<<-strptime(x, "%Y-%m-%d %H") 
       
       xlab<-year
       mt <- ggplot(f, aes(z2, value)) +  geom_line()
       mt + facet_grid(variable~., scales = "free_y")+
         theme(text = element_text(size=20),
               axis.text.x = element_text(angle=90, vjust=1)) +
         xlab(paste("Day of Year", year)) +
         ylab("Value") +      
         #ggtitle(paste("Location of -",station$nome,"- in the state of ",station$state,",", station$country))
         ggtitle(paste("Location of -",names(choicesStationEUA[choicesStationEUA==input$station2])))
     })
   })  
  
  ## gerando a tabela com os dados
  output$tabelaGrafico1 <- renderDataTable({
    dadosTabela1
  })
   output$tabelaGrafico2 <- renderDataTable({
     dadosTabela2
   })  
})