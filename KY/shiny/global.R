library(DBI)
library(RPostgreSQL)
source('/home/jorge/Documents/brusone_model/brusoneModel.R')

experimentoBrasil <- 93
experimentoKY <- 112

executaConsulta <- function(sql){  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect("PostgreSQL", user="postgres", dbname="agrodb",password="postgres",host="127.0.0.1",port="5432")     
  rs = dbSendQuery(con,sql)
  df <- fetch(rs,n=-1)
  dbClearResult(rs)
  dbDisconnect(con)  
  return (df)
}



executaSQL <- function(sql){  
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect("PostgreSQL", user="postgres", dbname="agrodb",password="postgres",host="127.0.0.1",port="5432")     
  rs = dbSendQuery(con,sql)
  #df <- fetch(rs)
  dbClearResult(rs)
  dbDisconnect(con)  
  ## return (df)
}


carregaEstacoes <- function(experiment){
  sql <- paste("select s.description as station, es.weather_disease as weather from experiment_station es
               join station s on es.station = s.id where es.experiment = ",experiment," order by station")
  df <- executaConsulta(sql)
  return (df)
}

# usa a função
#dt <- carregaEstacoes(112)


carregaDadosClimaHCP <- function(weather,floweringDate){
  sqlWeather <- paste("select * from crosstab ('select cast(date || '' '' || time as timestamp) as datahora,
                        variable, (data_value).value_double from weather_simulation_data where weather_simulation=",
                      weather,"and (variable=1 or variable=2 or variable=1266) and date >= (to_date(''",
                      floweringDate,"''::text, ''YYYY/MM/DD''::text)- 60) and date <= (to_date(''",floweringDate,
                      "''::text, ''YYYY/MM/DD''::text)+30 )order by datahora,variable') 
                        as dcv (datahora timestamp, temp double precision ,rh  double precision,  rain double precision)",sep="")  
  df <- executaConsulta(sqlWeather)  
  df$hcp <- round(mapply('hcp', df$temp, df$rh), 3)
  # Accumulated hcp across all days during the season
  df$chcp<-cumsum(df$hcp)
  return (df)
}

## usa a função
# carregando as estações
stationsBrasil <-  carregaEstacoes(experimentoBrasil)
stationsEUA <-  carregaEstacoes(experimentoKY)
## gerando a estrutura para popular o combo de estações
choicesStationBrasil <- as.character(unique(stationsBrasil$weather))
names(choicesStationBrasil) <- stationsBrasil$station
choicesStationEUA <- as.character(unique(stationsEUA$weather))
names(choicesStationEUA) <- stationsEUA$station

