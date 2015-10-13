# Carregar somente onde vai usar
#library(DBI)
#library(RPostgreSQL)

## parametros fixos 
## taxa de infecção
taxa <- 0.10
## numero de espigas
numberHeads <-300
## valorLimite para o IP ser considerado perigoso
limiteIP <- 40

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

hcp<- function(temp,rh) { # verificar
  ifelse(temp >= 15 && temp < 27 && rh > 93,
         1/(14.35-0.25*temp),
         ifelse(temp >= 27 && temp < 35 && rh > 93,1/(-8.5+0.59*temp),0))
}

cb<- function(temp,rh) {  
  ifelse(temp >= 15 && temp < 35 && rh > 93,1,0)
}

risk<- function(ip,rain,cb){
  #ifelse(ip>0,ifelse(rain>5,min(1,(-0.5+cb*0.05)),min(1,(-0.5+cb*0.1))),0) 
  ifelse(ip>0,ifelse(rain>5,((0.15*exp(0.19*cb))/58),((0.47*exp(0.20*cb))/58)),0) 
}

headsDisease<- function(numberHeads,risk){
  min(numberHeads,(risk*(0.25*numberHeads)))
}

calculaIPBrusone <- function(experiment){
  ## ler os tratamentos do experimento
  #print("vai recuperar os tratamentos")
  sql <- paste("select t.id as treatment, t.start_simulation, 
               (t.start_simulation+(tout.data_value_simulated).value_integer) as heading_date, t.station as station, 
               t.weather_simulation as weather_simulation, t.soil as soil, t.cultivar as cultivar 
               from treatment_output tout 
               join treatment t on t.id = tout.treatment 
               join experiment e on e.id = t.experiment 
               where experiment = '",experiment,"' and tout.variable = 1335",sep="")
  #print(sql)
  expTreatment <- executaConsulta(sql)
  
  ## apagar os registros de treatment_output 1340 e 1341 deve ser o id da variavel do IP (inoculo potencial) 
  ## apaga todos os treatment_output do experimento e variavel do IP
  sql <- paste("delete from treatment_output tout using variable v, treatment t, experiment e, station s, 
               weather_simulation ws where tout.variable = v.id and t.id = tout.treatment and e.id = 
               t.experiment and t.station = s.id and t.weather_simulation = ws.id and v.id = 1340 
               and t.experiment = ",experiment,sep="")    
  #print("vai apagar treatment_output")
  #print(sql)
  executaSQL(sql)   
  sql <- paste("delete from treatment_output tout using variable v, treatment t, experiment e, station s, 
               weather_simulation ws where tout.variable = v.id and t.id = tout.treatment and e.id = 
               t.experiment and t.station = s.id and t.weather_simulation = ws.id and v.id = 1341 
               and t.experiment = ",experiment,sep="")    
  #print("vai apagar treatment_output")
  #print(sql)
  executaSQL(sql)   
  for (i in 1:nrow(expTreatment)){
    linha <- expTreatment[i,]
    
    ## recuperar o ID do weather_disease
    sqlWD <- paste("select weather_disease from experiment_station where 
                 experiment = ",experiment," and station = ",linha$station,sep="")
    dfWD <- executaConsulta(sqlWD)
    #print(paste("SQL WD: ",sqlWD))
    idWeatherDisease<- dfWD[1,1]   
    #print(paste("ID WD: ",idWeatherDisease))
    
    #print(paste("linha: ",i, "experiment: ",linha$experiment,
    #            " station: ",linha$station," weather: ",linha$weather_simulation))
    ## montando a consulta para recuperar os dados climáticos
    sqlWeather <- paste("select * from crosstab ('select cast(date || '' '' || time as timestamp) as datahora, 
              variable, (data_value).value_double from weather_simulation_data where 
              weather_simulation=", idWeatherDisease," and (variable=1 or variable=2 or variable=1266) 
              and date >= (to_date(''", linha$heading_date,"''::text, ''YYYY/MM/DD''::text)- 60) and date <= (to_date(''",
              linha$heading_date, "''::text, ''YYYY/MM/DD''::text) -1) order by datahora,variable') as dcv (datahora timestamp,
              temp double precision ,rh  double precision,  rain double precision)",sep="")
    #print(sqlWeather)
    #print("vai consultar os dados climaticos")
    dfWeather <- executaConsulta(sqlWeather)
    ## omitindo valores NA
    dfWeather <- na.omit(dfWeather)    
    ##print(sqlWeather)
 #   print(paste("linhas clima: ",nrow(dfWeather)))
    ip <- 0
    ## variavel que armazena o inóculo potencial
    if (nrow(dfWeather) > 0){ ## teste para somente aplicar a função se tiver dados do banco, senão gera erro
      # Calculate and insert hcp in the dataset 
      dfWeather$hcp <- round(mapply('hcp', dfWeather$temp, dfWeather$rh), 3)      
      # Accumulated hcp across all days during the season
      #df$chcp<-cumsum(df$hcp)    
      ip <- sum(dfWeather$hcp)        
    } 
    #print(paste("ip:",ip))
   
    ## inserir um treatment_output
    ##print(paste("treatment=",linha$treatment))
    sql <- paste("insert into treatment_output (treatment,variable,data_type, 
                 data_value_simulated.value_double,line,date_julian) values (",
                 linha$treatment,",1340,'7',",round(ip,digits = 2),",1,0)",sep="")
    #print(paste("sql treatment_output: ",sql))
    #print("vai inserir em treatment output")
    #print(sql)
    executaSQL(sql)   
        
    ## segunda parte. Inicia o calculo com os 30 dias posteires a data de semeadura
    print(paste("IP antes ajuste: ",ip))
    #ip<-ifelse(ip>=limiteIP,1,0)
    ip<-ifelse(ip>=limiteIP,1,ifelse(ip < 10,0,(-0.24+0.032*ip)))
    if (ip > 1){
      ip <- 1
    }
    print(paste("IP: ",ip))
    ## consultar os próximos 30 dias a partir da data de semeadura
    incidence <- 0
    for (i in 1:30) {      
      ## montando a consulta para recuperar os dados climáticos de dia em dia apóso espigamento
      sqlWeather <- paste("select * from crosstab ('select cast(date || '' '' || time as timestamp) as datahora, 
                          variable, (data_value).value_double from weather_simulation_data where 
                          weather_simulation=", idWeatherDisease," and (variable=1 or variable=2 or variable=1266) 
                          and date >= (to_date(''", linha$heading_date,"''::text, ''YYYY/MM/DD''::text)+",i,") 
                          and date <= (to_date(''", linha$heading_date,"''::text, ''YYYY/MM/DD''::text)+",i,") 
                          order by datahora,variable') as dcv (datahora timestamp,
                          temp double precision ,rh  double precision,  rain double precision)",sep="")
      #print(sqlWeather)
      #print("vai consultar os dados climaticos")
      dfWeather <- executaConsulta(sqlWeather) 
      ## omitindo valores NA
      dfWeather <- na.omit(dfWeather)
      if (nrow(dfWeather) > 0){ ## teste para somente aplicar a função se tiver dados do banco, senão gera erro
        dfWeather$cb <- round(mapply('cb', dfWeather$temp, dfWeather$rh),3)
        dfWeather$ip <- ip
        dfWeather$risk <- round(mapply('risk', dfWeather$ip, dfWeather$rain,dfWeather$cb),3) 
        riscoDiario <- sum(dfWeather$risk)
        #print(paste("risco diário: ",riscoDiario))
        incidence <- incidence + (riscoDiario * taxa * numberHeads)
        #print(paste("Incidence ",incidence))
      }      
    }    
    
    ## inserir um treatment_output
    ##print(paste("treatment=",linha$treatment))
    ## Calculando a incicendia percentual
    incidence <- (incidence/numberHeads)*ip
    print(paste("Incidence ",incidence))
    sql <- paste("insert into treatment_output (treatment,variable,data_type, 
                 data_value_simulated.value_double,line,date_julian) values (",
                 linha$treatment,",1341,'7',",round(incidence,digits = 2),",1,0)",sep="")
    #print(paste("sql treatment_output: ",sql))
   # print("vai inserir em treatment output incidence")
    #print(sql)
    executaSQL(sql)          
  }  
}

recuperaDadosCidadesExperimento <- function(experimentID){
  df <- recuperaDataEspigamentoCidadesExperimento(experimentID)
  dfIP <- recuperaIpCidadesExperimento(experimentID)
  #df$tIP <- dfIP$treatment
  df$ip <- dfIP$ip
  dfInc <- recuperaIncidenceCidadesExperimento(experimentID)
  #df$tInc <- dfInc$treatment
  df$incidence <- dfInc$incidence
  df$status <- ifelse(df$heading_date < df$semeadura,"Error","OK")
  df$status <- ifelse(df$ip == 0,"Error",df$status)
  return (df)
}

recuperaIpIncidenceCidadesExperimento <- function(experimentID){
  df <- recuperaIpCidadesExperimento(experimentID)
  dfInc <- recuperaIncidenceCidadesExperimento(experimentID)
  df$incidence <- dfInc$incidence
  df <- df[,c("treatment","city","latitude","longitude","ip","incidence")]
  return (df)
}

recuperaDataEspigamentoCidadesExperimento <- function(experimentID){
  sql <- paste("select t.id as treatment, t.start_simulation as semeadura, 
      extract(year from t.start_simulation) as ano,
               (extract(day from t.start_simulation) || '-' || extract(month from t.start_simulation)) as dia,  
               c.description as city, s.latitude as latitude, s.longitude as longitude, 
               cul.description as cultivar, (t.start_simulation+(tout.data_value_simulated).value_integer) as heading_date
               from treatment_output tout 
               join treatment t on t.id = tout.treatment
               join experiment e on e.id = t.experiment
               join station s on t.station = s.id
               join city c on c.id = s.city
               join cultivar cul on t.cultivar = cul.id
               where e.id = ",experimentID," and tout.variable = 1335 order by treatment",sep="") 
  df <- executaConsulta(sql)
  return (df)
}



recuperaIpCidadesExperimento <- function(experimentID){
  sql <- paste("select t.id as treatment, t.start_simulation as semeadura, 
      extract(year from t.start_simulation) as ano,
      (extract(day from t.start_simulation) || '-' || extract(month from t.start_simulation)) as dia,  
      c.description as city, s.latitude as latitude, s.longitude as longitude, 
      cul.description as cultivar, (tout.data_value_simulated).value_double as ip 
               from treatment_output tout 
               join treatment t on t.id = tout.treatment
               join experiment e on e.id = t.experiment
               join station s on t.station = s.id
               join city c on c.id = s.city
               join cultivar cul on t.cultivar = cul.id
               where e.id = ",experimentID," and tout.variable = 1340 order by treatment",sep="") 
  df <- executaConsulta(sql)
  return (df)
}

recuperaIncidenceCidadesExperimento <- function(experimentID){
  sql <- paste("select t.id as treatment,  t.start_simulation as semeadura, 
      extract(year from t.start_simulation) as ano,
      (extract(day from t.start_simulation) || '-' || extract(month from t.start_simulation)) as dia,  
      c.description as city, s.latitude as latitude, s.longitude as longitude, 
      cul.description as cultivar, (tout.data_value_simulated).value_double as incidence 
      from treatment_output tout 
      join treatment t on t.id = tout.treatment
      join experiment e on e.id = t.experiment
      join station s on t.station = s.id
      join city c on c.id = s.city
      join cultivar cul on t.cultivar = cul.id
      where e.id = ",experimentID," and tout.variable = 1341 order by treatment",sep="")    
  df <- executaConsulta(sql)
  return (df)
}

## Chamado na execuçao da aplicacao shiny
executarPorInterface <- function(experimentID,floweringDate){
  calculaIPBrusoneShiny(experimentID,floweringDate)
  df <- recuperaIpCidadesExperimento(experimentID)
  return (df)
} 

## Metodo que calcula o Inoculo potencial da Brusone
## Recebe o ID do experimento e a data de floração
## Utilizado pela aplicação shiny onde o usuário informa a data de floração
calculaIPBrusoneShiny <- function(experiment,floweringDate){
  ## ler experiment_station
  sql <- paste("select * from experiment_station where experiment = '",experiment,"'",sep="")
  expStat <- executaConsulta(sql)
  ## apagar os registros de treatment_output
  sql <- paste("delete from treatment_output tout
                 using variable v, treatment t, experiment e, station s, weather_simulation ws
                 where tout.variable = v.id and t.id = tout.treatment and e.id = t.experiment and t.station = s.id 
                 and t.weather_simulation = ws.id 
                 and v.id = 1340 and t.experiment = ",experiment,sep="")    
  executaSQL(sql)
  sql <- paste("delete from treatment_output tout
                 using variable v, treatment t, experiment e, station s, weather_simulation ws
                 where tout.variable = v.id and t.id = tout.treatment and e.id = t.experiment and t.station = s.id 
                 and t.weather_simulation = ws.id 
                 and v.id = 1341 and t.experiment = ",experiment,sep="")    
  executaSQL(sql)    
  
  ## apagar os registros de treatment
  sql <- paste("delete from treatment t
                 using station s, experiment e, weather_simulation ws
                 where t.station = s.id and t.experiment = e.id and t.weather_simulation = ws.id
                 and t.experiment = ",experiment,sep="")
  executaSQL(sql)  
  for (i in 1:nrow(expStat)){
    linha <- expStat[i,]    
    
    print(paste("linha: ",i, "experiment: ",linha$experiment,
                " station: ",linha$station," weather disease: ",linha$weather_disease))
    ## montando a consulta para recuperar os dados climáticos
    sqlWeather <- paste("select * from crosstab ('select cast(date || '' '' || time as timestamp) as datahora,
                        variable, (data_value).value_double from weather_simulation_data where weather_simulation=",
                        linha$weather_disease,"and (variable=1 or variable=2 or variable=1266) and date >= (to_date(''",
                        floweringDate,"''::text, ''YYYY/MM/DD''::text)- 60) and date <= (to_date(''",floweringDate,
                        "''::text, ''YYYY/MM/DD''::text)-1 )order by datahora,variable') 
                        as dcv (datahora timestamp, temp double precision ,rh  double precision,  rain double precision)",sep="")
    dfWeather <- executaConsulta(sqlWeather)
    #print(sqlWeather)    
    dfWeather <- na.omit(dfWeather)
    #print(paste("Total de linhas de dados 60 dias antes:",nrow(dfWeather)))
    ## variavel que armazena o inóculo potencial
    if (nrow(dfWeather) > 0){ ## teste para somente aplicar a função se tiver dados do banco, senão gera erro
      # Calculate and insert hcp in the dataset 
      dfWeather$hcp <- round(mapply('hcp', dfWeather$temp, dfWeather$rh), 3)      
      # Accumulated hcp across all days during the season
      #df$chcp<-cumsum(df$hcp)    
      ip <- sum(dfWeather$hcp)        
    } 
    #print(paste("ip:",ip))
    ## inserir um novo treatment    
    sql <- paste("insert into treatment (experiment,station,weather_simulation,start_simulation,soil,cultivar)
                 values (",linha$experiment,",",linha$station,",",linha$weather_disease,",'",
                 floweringDate,"',",linha$soil,",",3,")" ,sep="")
    executaSQL(sql)
    ## inserir um treatment_output
    ## primeiro recupera o id do treatment inserido
    sql <- paste("select id from treatment where 
                 experiment = ",linha$experiment," and station = ",linha$station," and 
                 weather_simulation = ",linha$weather_disease,sep="")
    df <- executaConsulta(sql)
    idTreatment <- df[1,1]
    ## sql de inserção treatment
    sql <- paste("insert into treatment_output 
            (treatment,variable,data_type, data_value_simulated.value_double,line,date_julian) 
            values (",idTreatment,",1340,'7',",round(ip,digits = 2),",1,0)",sep="")
    executaSQL(sql)    
    
    ## segunda parte. Inicia o calculo com os 30 dias posteires a data de semeadura

    #ip<-ifelse(ip>=limiteIP,1,0)
    ip<-ifelse(ip>=limiteIP,1,ifelse(ip < 10,0,(-0.24+0.032*ip)))
    if (ip > 1){
      ip <- 1
    }
    print(paste("IP: ",ip))    
    ## consultar os próximos 30 dias a partir da data de semeadura
    incidence <- 0
    for (i in 1:30) {      
      ## montando a consulta para recuperar os dados climáticos de dia em dia apóso espigamento
      sqlWeather <- paste("select * from crosstab ('select cast(date || '' '' || time as timestamp) as datahora, 
                          variable, (data_value).value_double from weather_simulation_data where 
                          weather_simulation=", linha$weather_disease," and (variable=1 or variable=2 or variable=1266) 
                          and date >= (to_date(''", floweringDate,"''::text, ''YYYY/MM/DD''::text)+",i,") 
                          and date <= (to_date(''", floweringDate,"''::text, ''YYYY/MM/DD''::text)+",i,") 
                          order by datahora,variable') as dcv (datahora timestamp,
                          temp double precision ,rh  double precision,  rain double precision)",sep="")
      #print(sqlWeather)
      #print("vai consultar os dados climaticos")
      dfWeather <- executaConsulta(sqlWeather) 
      ## omitindo valores NA
      dfWeather <- na.omit(dfWeather)
      if (nrow(dfWeather) > 0){ ## teste para somente aplicar a função se tiver dados do banco, senão gera erro
        dfWeather$cb <- round(mapply('cb', dfWeather$temp, dfWeather$rh),3)
        dfWeather$ip <- ip
        dfWeather$risk <- round(mapply('risk', dfWeather$ip, dfWeather$rain,dfWeather$cb),3) 
        riscoDiario <- sum(dfWeather$risk)
        #print(paste("risco diário: ",riscoDiario))
        incidence <- incidence + (riscoDiario * taxa * numberHeads)
        #print(paste("Incidence ",incidence))
      }      
    }    
    ## inserir um treatment_output
    ##print(paste("treatment=",linha$treatment))
    ## Calculando a incicendia percentual
    incidence <- (incidence/numberHeads)*ip
    print(paste("Incidence ",incidence))    
    sql <- paste("insert into treatment_output (treatment,variable,data_type, 
                 data_value_simulated.value_double,line,date_julian) values (",
                 idTreatment,",1341,'7',",round(incidence,digits = 2),",1,0)",sep="")
    #print(paste("sql treatment_output: ",sql))
   #print("vai inserir em treatment output incidence")
    #print(sql)
    executaSQL(sql)      
  }  
}