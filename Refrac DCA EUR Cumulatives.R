library("RODBC")
library("rJava")
library("ggplot2")
library("grid")
library("gridExtra")
library("dplyr")
library("stringr")
library("aRpsDCA")
library("stringdist")

if(!exists("master_data")){
  {
    myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
    #input < sqlFetch(myConn, [esp_stage].[dbo].[RefracIncrementalInput]
    master_data <- sqlQuery(myConn,"Select * from [esp_stage].[dbo].[RefracIncrementalInput]")
    close(myConn)
  }
}

count = 0 
input = master_data

if(!exists("o")||!exists("Time_Current")||!exists("input_reduced")){
  { 
    input_reduced = subset(input, input$Time2Refrac > 3)
    input_reduced = subset(input_reduced, !is.na(input_reduced$Time2Refrac))
    refracAPI = unique(input_reduced$api)
    o = data.frame()
    currentTime = numeric(length(refracAPI))
    Time_Current = data.frame(api = refracAPI)
    count = 0 
    for(i in 1:length(refracAPI)) {
      count = count+1 ; print(count)
      d = filter(input_reduced, api == refracAPI[i]) 
      d = filter(d, Time2Refrac == min(Time2Refrac))
      d = filter(d, RefraccompletionDate == min(RefraccompletionDate))
      d = d[complete.cases(d[,6:8]),]
      maxtime = max(d$ProductionMonth)
      Time_Current$Max.Month[Time_Current$api == refracAPI[i]] = maxtime
    }
    count = 0
    for(i in 1:length(refracAPI)) {
      count = count+1 ; print(count)
      d = filter(input_reduced, api == refracAPI[i]) 
      d = filter(d, Time2Refrac == min(Time2Refrac))
      d = filter(d, RefraccompletionDate == min(RefraccompletionDate))
      d = filter(d, ProductionMonth <= Time2Refrac[1]) 
      o = rbind(o,d)
    }
  }
}

count = 0 

# Install JAVA
### http://www.java.com/en/download/manual.jsp
### if R is 64 bit then you need the 64 bit JAVA. By default, the download page gives a 32 bit version 

#Install RJava
###install.packages('rJava')

# MOVE .jar to local file
###current path: Shared Drive\Projects\JAVA\DCA <- DCA documantation
###example path: C:\javatest\DCA

# Set JAVA Path
### rJava, the R<->Java bridge, will need jvm.dll, but R will have trouble finding that DLL. It resides in a folder like
### example: C:\Program Files\Java\jre6\jre\bin\client
### add that directory to your windows PATH variable. (Windows -> "Path" -> "Edit environment variables to for your account" -> PATH -> edit the value.)


#add JAVA to PATH under comperuter properties
#R.Version()# 64 or 32 Progam Files refers to 64
#rm(list=ls())

forecastDCA <- function(input,oilunit,gasunit, oilsegment,gassegment, currentTime,oilEconlimit,gasEconLimit) {
  input = input[order(input$api),]
  api_list = unique(input$api)
  
  #Create columns for the output vector
  oilEUR = numeric(length(api_list))
  gasEUR = numeric(length(api_list))
  oilCum3Months= numeric(length(api_list))
  gasCum3Months= numeric(length(api_list))
  oilCum6Months = numeric(length(api_list))  
  gasCum6Months = numeric(length(api_list))
  oilCumCurrent = numeric(length(api_list))  
  gasCumCurrent = numeric(length(api_list))
  oilmodelQi = numeric(length(api_list))
  gasmodelQi = numeric(length(api_list))
  oilmodelDi = numeric(length(api_list))
  gasmodelDi = numeric(length(api_list))
  oilmodelB = numeric(length(api_list))
  gasmodelB = numeric(length(api_list))
  oilmodelTime = numeric(length(api_list))
  gasmodelTime = numeric(length(api_list))
  oilinitialDi = numeric(length(api_list))
  gasinitialDi = numeric(length(api_list))
  oilrefracDi = numeric(length(api_list))
  gasrefracDi = numeric(length(api_list))
  output = data.frame(api = api_list,'Oil EUR' = oilEUR, 'Gas EUR' = gasEUR, 'Oil Cum. 3M' = oilCum3Months, 'Gas Cum. 3M' = oilCum6Months, 
                      'Oil Cum. 6M' = oilCum6Months, 'Gas Cum. 6M' = gasCum6Months, 'Oil Cum. Current' = oilCumCurrent, 'Gas Cum. Current' = gasCumCurrent,
                      'Oil Model Qi' = oilmodelQi,'Gas Model Qi' = gasmodelQi, 'Oil Model Di' = oilmodelDi,'Gas Model Di' = gasmodelDi, 
                      'Oil Model B'= oilmodelB, 'Gas Model B'= gasmodelB, 'Oil Model Time' = oilmodelTime, 'Gas Model Time' = gasmodelTime,
                      'Oil Di Initial' = oilinitialDi,'Gas Di Initial' = gasinitialDi,'Oil Refrac Di' = oilrefracDi,'Gas Refrac Di' = gasrefracDi)
  
  #api = 3302501061
  #i = match(api, api_list)
  #i = 2080
  for(i in 1:length(api_list)) {
    count = count + 1
    print(count)
    well = subset(input, input$api == api_list[i])
    
    ############Check if multiple completion dates, then picks earlier one
    if(length(unique(well$RefraccompletionDate))>1)
    {
      well= subset(well, well$RefraccompletionDate==min(well$RefraccompletionDate))
    }
    
    if(length(unique(well$Time2Refrac))>1) 
    {
      well = subset(well, well$Time2Refrac==min(well$Time2Refrac))
    }
    
    #Remove rows with NA from three columns, oilProduction,gasproduction, and Production date
    well = well[complete.cases(well[,6:8]),]
    
    date = well$productionDate
    oilProduction = well$oilProduction
    gasProduction = well$gasProduction
    currentMonth = currentTime[currentTime$api == well$api[1],][,2]
    date <- data.matrix((date));date <- data.matrix(as.integer(date))
    oilProduction <- data.matrix(oilProduction);oilProduction <- as.numeric(oilProduction)
    gasProduction <- data.matrix(gasProduction);gasProduction <- as.numeric(gasProduction)
    
    
    if(length(well$api)<=5)
    {
      oilDCAeur = 111111111
      oilCum3Months = 111111111
      oilCum6Months = 111111111  
      oilCumCurrent = 111111111
      oilmodelQi = 111111111
      oilmodelDi = 111111111
      oilmodelB = 111111111
      oilmodelTime = 111111111
      oilinitialDi  = 111111111
      oilrefracDi  = 111111111
      gasDCAeur = 111111111
      gasCum3Months = 111111111
      gasCum6Months = 111111111
      gasCumCurrent = 111111111
      gasmodelQi = 111111111
      gasmodelDi = 111111111
      gasmodelB = 111111111
      gasmodelTime = 111111111
      gasinitialDi  = 111111111
      gasrefracDi  = 111111111
      
      output = saveOutput(output,i,oilDCAeur,oilCum3Months,oilCum6Months,oilCumCurrent,oilmodelQi,oilmodelDi,oilmodelB,
                          oilmodelTime, oilinitialDi, oilrefracDi, gasDCAeur,gasCum3Months,gasCum6Months,gasCumCurrent, gasmodelQi, 
                          gasmodelDi , gasmodelB, gasmodelTime, gasinitialDi, gasrefracDi,api_list = api_list)
    } else  {
      gasDCAdriver = createDriver(date,gasProduction,gasunit); segmentation(gasDCAdriver, TRUE,3.0); setTailDecline(gasDCAdriver, taildecline)
      #setMinimumProduction(gasDCAdriver, 0.0)
      oilDCAdriver = createDriver(date,oilProduction,oilunit); segmentation(oilDCAdriver, TRUE,3.0); setTailDecline(oilDCAdriver, taildecline)
      #setMinimumProduction(oilDCAdriver, 0.0)
      
      if (((length(oilProduction[oilProduction != 0])<5) || (((length(oilProduction[oilProduction != 0]))/(length(oilProduction))) <= .25)) & ((length(gasProduction[gasProduction != 0])<5) || (((length(gasProduction[gasProduction !=0]))/(length(gasProduction))) <= .25))) {
        
        oilDCAeur = 2222222222
        oilCum3Months = 2222222222
        oilCum6Months = 2222222222  
        oilCumCurrent = 2222222222
        oilmodelQi = 2222222222
        oilmodelDi = 2222222222
        oilmodelB = 2222222222
        oilmodelTime = 2222222222
        oilinitialDi  = 2222222222
        oilrefracDi  = 2222222222
        gasDCAeur = 2222222222
        gasCum3Months = 2222222222
        gasCum6Months = 2222222222
        gasCumCurrent = 2222222222
        gasmodelQi = 2222222222
        gasmodelDi = 2222222222
        gasmodelB = 2222222222
        gasmodelTime = 2222222222
        gasinitialDi  = 2222222222
        gasrefracDi  = 2222222222
        
        output = saveOutput(output,i,oilDCAeur,oilCum3Months,oilCum6Months,oilCumCurrent,oilmodelQi,oilmodelDi,oilmodelB,
                            oilmodelTime, oilinitialDi, oilrefracDi, gasDCAeur,gasCum3Months,gasCum6Months,gasCumCurrent, gasmodelQi, 
                            gasmodelDi , gasmodelB, gasmodelTime, gasinitialDi, gasrefracDi,api_list = api_list)
      } else if (((length(oilProduction[oilProduction != 0]))<5) || (((length(oilProduction[oilProduction != 0]))/(length(oilProduction))) <= .25)) {
        
        oilDCAeur = 2222222222
        oilCum3Months = 2222222222
        oilCum6Months = 2222222222
        oilCumCurrent = 2222222222
        oilmodelQi = 2222222222
        oilmodelDi = 2222222222
        oilmodelB = 2222222222
        oilmodelTime = 2222222222
        oilinitialDi  = 2222222222
        oilrefracDi  = 2222222222
        
        gasDCAeur = getEUR(gasDCAdriver,"First",modelYears)
        gasreport = getReport(gasDCAdriver)
        gasdcaParams = getDCAValues(gasreport, c("Qi","Di","b"))
        gasmodelQi = gasdcaParams[1];gasmodelDi = gasdcaParams[2];gasmodelB = gasdcaParams[3]; gasMonths = getDCAmonths(gasreport,"Number of months"); gasmodelDiValues = getDiValues(gasreport, "Di") 
        gasinitialDi = gasmodelDiValues[1]
        gasrefracDi = gasmodelDiValues[min(which(cumsum(gasMonths)>(well$Time2Refrac[1])+3))]
        
        #gasrefracDi = gasmodelDiValues[length(gasmodelDiValues)]
        if(length(gasMonths) > 1){
          lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1-amatch(gasmodelQi,getPrediction(gasDCAdriver,max(well$ProductionMonth, na.rm = TRUE)+1),maxDist = 13)+1
        } else {lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1 - which.max(na.omit(gasProduction))+1}
        
        gasmodelTime = lastsegmentstart
        rate_time_current = arps(gasmodelQi,gasmodelDi,gasmodelB,seq(lastsegmentstart,lastsegmentstart+currentMonth-max(well$ProductionMonth))); rate_time_current[rate_time_current<gasEconLimit]<-0
        rate_time_3M <- arps(gasmodelQi,gasmodelDi,gasmodelB,seq(lastsegmentstart,lastsegmentstart+3)); rate_time_3M[rate_time_3M<gasEconLimit] <- 0
        rate_time_6M <- arps(gasmodelQi,gasmodelDi,gasmodelB,seq(lastsegmentstart,lastsegmentstart+6)); rate_time_6M[rate_time_6M<gasEconLimit] <- 0
        gasCum3Months = sum(rate_time_3M[1:3]) + well$PreRefracCumGas[1]
        gasCum6Months = sum(rate_time_6M[1:6]) + well$PreRefracCumGas[1]
        gasCumCurrent = sum(rate_time_current) + well$PreRefracCumGas[1]
        
        output = saveOutput(output,i,oilDCAeur,oilCum3Months,oilCum6Months,oilCumCurrent,oilmodelQi,oilmodelDi,oilmodelB,
                            oilmodelTime, oilinitialDi, oilrefracDi, gasDCAeur,gasCum3Months,gasCum6Months,gasCumCurrent, gasmodelQi, 
                            gasmodelDi , gasmodelB, gasmodelTime, gasinitialDi, gasrefracDi,api_list = api_list)
      } else if (((length(gasProduction[gasProduction != 0]))<5) || (((length(gasProduction[gasProduction != 0]))/(length(gasProduction))) <= .25)) {
        
        gasDCAeur = 2222222222
        gasCum3Months = 2222222222
        gasCum6Months = 2222222222
        gasCumCurrent = 2222222222
        gasmodelQi = 2222222222
        gasmodelDi = 2222222222
        gasmodelB = 2222222222
        gasmodelTime = 2222222222
        gasinitialDi  = 2222222222
        gasrefracDi  = 2222222222
        
        oilDCAeur = getEUR(oilDCAdriver, "First", modelYears)
        oilreport = getReport(oilDCAdriver)
        oildcaParams = getDCAValues(oilreport, c("Qi","Di","b"))
        oilmodelQi = oildcaParams[1];oilmodelDi = oildcaParams[2];oilmodelB = oildcaParams[3] ;oilMonths = getDCAmonths(oilreport,"Number of months"); oilmodelDiValues = getDiValues(oilreport, "Di")
        oilinitialDi = oilmodelDiValues[1]
        oilrefracDi = oilmodelDiValues[min(which(cumsum(oilMonths)>(well$Time2Refrac[1])+3))]
        
        #oilrefracDi = oilmodelDiValues[length(oilmodelDiValues)]
        if(length(oilMonths) > 1){
          lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1-amatch(oilmodelQi,getPrediction(oilDCAdriver,max(well$ProductionMonth, na.rm = TRUE)+1),maxDist = 13)+1
        } else {lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1 - which.max(na.omit(oilProduction))+1}
        
        oilmodelTime = lastsegmentstart
        rate_time_current = arps(oilmodelQi, oilmodelDi,oilmodelB,seq(lastsegmentstart,lastsegmentstart+currentMonth-max(well$ProductionMonth))); rate_time_current[rate_time_current<oilEconlimit] <- 0
        rate_time_3M <- arps(oilmodelQi, oilmodelDi,oilmodelB,seq(lastsegmentstart,lastsegmentstart+3)); rate_time_3M[rate_time_3M<oilEconlimit] <- 0
        rate_time_6M <- arps(oilmodelQi, oilmodelDi,oilmodelB,seq(lastsegmentstart,lastsegmentstart+6)); rate_time_6M[rate_time_6M<oilEconlimit] <- 0
        oilCum3Months = sum(rate_time_3M[1:3]) + well$PreRefracCumOil[1]
        oilCum6Months = sum(rate_time_6M[1:6]) + well$PreRefracCumOil[1]
        oilCumCurrent = sum(rate_time_current) + well$PreRefracCumOil[1]
        
        output = saveOutput(output,i,oilDCAeur,oilCum3Months,oilCum6Months,oilCumCurrent,oilmodelQi,oilmodelDi,oilmodelB,
                            oilmodelTime, oilinitialDi, oilrefracDi, gasDCAeur,gasCum3Months,gasCum6Months,gasCumCurrent, gasmodelQi, 
                            gasmodelDi , gasmodelB, gasmodelTime, gasinitialDi, gasrefracDi,api_list = api_list)
      } else {  
        
        oilDCAeur = getEUR(oilDCAdriver, "First", modelYears)
        oilEstimatedReserves = getEstimatedReserves(oilDCAdriver)
        oilCumulativeProduction = getCumulativeProduction(oilDCAdriver)
        oilreport = getReport(oilDCAdriver)
        oildcaParams = getDCAValues(oilreport, c("Qi","Di","b"))
        oilmodelQi = oildcaParams[1];oilmodelDi = oildcaParams[2];oilmodelB = oildcaParams[3] ;oilMonths = getDCAmonths(oilreport,"Number of months"); oilmodelDiValues = getDiValues(oilreport, "Di")
        oilinitialDi = oilmodelDiValues[1]
        oilrefracDi = oilmodelDiValues[min(which(cumsum(oilMonths)>(well$Time2Refrac[1])+3))]
        #oilrefracDi = oilmodelDiValues[length(oilmodelDiValues)]
        if(length(oilMonths) > 1){
          lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1-amatch(oilmodelQi,getPrediction(oilDCAdriver,max(well$ProductionMonth, na.rm = TRUE)+1),maxDist = 13)+1
        } else {lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1 - which.max(na.omit(oilProduction))+1}
        
        oilmodelTime = lastsegmentstart
        rate_time_current = arps(oilmodelQi, oilmodelDi,oilmodelB,seq(lastsegmentstart,lastsegmentstart+currentMonth-max(well$ProductionMonth))); rate_time_current[rate_time_current<oilEconlimit] <- 0
        rate_time_3M <- arps(oilmodelQi, oilmodelDi,oilmodelB,seq(lastsegmentstart,lastsegmentstart+3)); rate_time_3M[rate_time_3M<oilEconlimit] <- 0
        rate_time_6M <- arps(oilmodelQi, oilmodelDi,oilmodelB,seq(lastsegmentstart,lastsegmentstart+6)); rate_time_6M[rate_time_6M<oilEconlimit] <- 0
        oilCum3Months = sum(rate_time_3M[1:3]) + well$PreRefracCumOil[1]
        oilCum6Months = sum(rate_time_6M[1:6]) + well$PreRefracCumOil[1]
        oilCumCurrent = sum(rate_time_current) + well$PreRefracCumOil[1]
        
        gasDCAeur = getEUR(gasDCAdriver, "First", modelYears)
        gasEstimatedReserves = getEstimatedReserves(gasDCAdriver)
        gasCumulativeProduction = getCumulativeProduction(gasDCAdriver)
        gasreport = getReport(gasDCAdriver)
        gasdcaParams = getDCAValues(gasreport, c("Qi","Di","b"))
        gasmodelQi = gasdcaParams[1];gasmodelDi = gasdcaParams[2];gasmodelB = gasdcaParams[3] ;gasMonths = getDCAmonths(gasreport,"Number of months");gasmodelDiValues = getDiValues(gasreport, "Di")
        gasinitialDi = gasmodelDiValues[1]
        gasrefracDi = gasmodelDiValues[min(which(cumsum(gasMonths)>(well$Time2Refrac[1])+3))]
        
        #gasrefracDi = gasmodelDiValues[length(gasmodelDiValues)]
        if(length(gasMonths) > 1){
          lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1-amatch(gasmodelQi,getPrediction(gasDCAdriver,max(well$ProductionMonth, na.rm = TRUE)),maxDist = 13)+1
        } else {lastsegmentstart = max(well$ProductionMonth, na.rm = TRUE)+1 - which.max(na.omit(gasProduction))+1}
        
        gasmodelTime = lastsegmentstart
        rate_time_current = arps(gasmodelQi,gasmodelDi,gasmodelB,seq(lastsegmentstart,lastsegmentstart+currentMonth-max(well$ProductionMonth))); rate_time_current[rate_time_current<gasEconLimit]<-0
        rate_time_3M <- arps(gasmodelQi,gasmodelDi,gasmodelB,seq(lastsegmentstart,lastsegmentstart+3)); rate_time_3M[rate_time_3M<gasEconLimit] <- 0
        rate_time_6M <- arps(gasmodelQi,gasmodelDi,gasmodelB,seq(lastsegmentstart,lastsegmentstart+6)); rate_time_6M[rate_time_6M<gasEconLimit] <- 0
        gasCum3Months = sum(rate_time_3M[1:3]) + well$PreRefracCumGas[1]
        gasCum6Months = sum(rate_time_6M[1:6]) + well$PreRefracCumGas[1]
        gasCumCurrent = sum(rate_time_current) + well$PreRefracCumGas[1]
        
        output = saveOutput(output,i,oilDCAeur,oilCum3Months,oilCum6Months,oilCumCurrent,oilmodelQi,oilmodelDi,oilmodelB,
                            oilmodelTime, oilinitialDi, oilrefracDi, gasDCAeur,gasCum3Months,gasCum6Months,gasCumCurrent, gasmodelQi, 
                            gasmodelDi , gasmodelB, gasmodelTime, gasinitialDi, gasrefracDi,api_list = api_list)
      }
    }
  }
  return(output)
}

plotForecast <- function(DCAdriver) {
  prediction = getPredict(DCAdriver)
  production = getProduct(DCAdriver)
  xaxis <- matrix(c(1:length(prediction)), nrow=length(prediction), ncol=1)
  df2 <- data.frame(xaxis,production,prediction)
  
  ggplot(df2, aes(xaxis, y = value, color = variable)) +
    geom_point(aes(y = production, col = "Production")) +
    geom_point(aes(y = prediction, col = "Predicted"))
  
  
  #  + 
  #  geom_line(aes(y = production, col = "Production")) +
  #  geom_line(aes(y = prediction, col = "Predicted"))
  ###########Plotting
  #xaxisOil <- matrix(c(1:length(oilProduction)), nrow=length(oilProduction), ncol=1) ; print(length(oilProduction)); print(length(oilPrediction))
  #Time = xaxisOil
  #df1 <- data.frame(xaxisOil,oilProduction,oilPrediction)
  
  #gOil = ggplot(df1, aes(Time, y = Bbl, color = Flowstreams)) + 
  #  geom_line(aes(y = oilProduction, col = "Oil Production")) +
  #  geom_line(aes(y = oilPrediction, col = "Oil Predicted")) ; print(gOil)
  
  #xaxisGas <- matrix(c(1:length(gasProduction)), nrow=length(gasProduction), ncol=1) ; print(length(gasProduction)); print(length(gasPrediction))
  #Time = xaxisGas
  #df2 <- data.frame(xaxisGas,gasProduction,gasPrediction)
  
  #gGas = ggplot(df2, aes(Time, y = gasProduction, color = Flowstreams)) + 
  #  geom_line(aes(y = gasProduction, col = "Gas Production")) +
  #  geom_line(aes(y = gasPrediction, col = "Gas Predicted")) +  scale_fill_manual(values=c("#CC6666", "#9999CC")); print(gGas)
  
  #grid.arrange(gOil,gGas, nrow = 2, top = toString(api))
  
}

getEUR <- function(DCAdriver,reference , modelYears)
  #new class Arraylist 
  #Creates a Java class reference or calls a Java method
  # J(class, method, ...)
  #J( gasDCAdriver, "getModelTypes")
  tryCatch({J( DCAdriver, "model")
    #.jcall( gasDCAdriver, "S", "getReport")
    J(DCAdriver, "setForecastReference", "First" )
    J(DCAdriver, "setForecastYears", as.double(modelYears))
    DCAeur <- J( DCAdriver, "getEUR")
    #gasPrediction <- .jcall( gasDCAdriver, "[D", "getPrediction")
    #gasProduction <- .jcall(gasDCAdriver, "[D", "getProduction") 
    return(DCAeur)
  },warning = function(w) {
    DCAeur = 3333333333  ### EUR error
    return(DCAeur)
  }, error = function(e) {
    DCAeur = 3333333333
    return(DCAeur)
  }, finally = {
    
  })

createDriver <- function(date,production,unit)     #Units for constructor are either "bbl" or "Mcf"
{
  driver <- .jnew("com.drillinginfo.dca.DCAdriver",date,production,unit) #main (dates[S],values[D],units[L] )
  .jcall(driver, "V", "setModelTypes", modelList )
  return(driver)     
}

segmentation <- function(DCAdriver, boolean, threshold) #integer value typically b/w 2-5
{J(DCAdriver, "setSegmentation", boolean ,threshold)}
setMinimumProduction <- function(DCAdriver, minrate){J(DCAdriver, "setMinimumProduction", minrate)}
getEstimatedReserves <- function(DCAdriver){J(DCAdriver, "getEstimatedReserves")}

setModel <- function(modelType)
{
  modelList = new(J("java.util.ArrayList"))
  a = J( modelList, "add", modelType)
  return(modelList)
}
getProduct <- function(DCAdriver){.jcall(DCAdriver,"[D", "getProduction" )}
getProduction <- function(DCAdriver,months){.jcall(DCAdriver,"[D", "getProduction",as.integer(months ))}
getCumProduction <- function(DCAdriver) {  .jcall( DCAdriver,"[D", "getCumulativeProductionStream" )}
##getCumulativeProduction 
getPredict <- function(DCAdriver){.jcall(DCAdriver,"[D", "getPrediction")}
getPrediction <- function(DCAdriver, months){  .jcall(DCAdriver,"[D", "getPrediction", as.integer(months))}
#getCumulativePrediction <- function(DCAdriver)
#{
#  .jcall(DCAdriver,"[D", "getCumulativePrediction")
#}
getCumPrediction <- function(DCAdriver){.jcall( DCAdriver,"[D", "getCumulativePrediction" )}
getCumulativePrediction <- function(DCAdriver, months){  .jcall(DCAdriver,"[D", "getCumulativePrediction", as.integer(months))}
getReport <- function(DCAdriver) {.jcall(DCAdriver, "S", "getReport")}

getDiValues <- function(string, x){
  regularexpression = ": [+]?[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?"
  output = c()
  pattern = paste(x,regularexpression,sep = "")
  qiString = unlist(str_extract_all(string,pattern))
  value= as.numeric(str_sub(qiString,start = 3+nchar(x)))
  output = value
  return(output)
}

getDCAValues <- function(string,x)
{
  regularexpression = ": [+]?[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?"
  output = c()
  for(i in 1:length(x))
  {
    pattern = paste(x[i],regularexpression,sep = "")
    qiString = unlist(str_extract_all(string,pattern))
    value= as.numeric(str_sub(qiString[length(qiString)],start = 3+nchar(x[i])))
    output[i] = value
  }
  return(output)
}

getDCAmonths <- function(string,x)
{
  regularexpression = ":   [+]?[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?"
  output = c()
  pattern = paste(x,regularexpression,sep = "")
  qiString = unlist(str_extract_all(string,pattern))
  value= as.numeric(str_sub(qiString,start = 3+nchar(x)))
  output = value
  return(output)
}

arps <- function(qi,di,b,t)
{
  output = c()
  if(length(t) ==1){
    return(qi/(1+di*b*t)^(1/b))
  } else {
    for (i in 1:length(t)){
      output[i] = qi/(1+di*b*t[i])^(1/b)
    }
    return(output)
  } 
}

setTailDecline <- function(DCAdriver, taildecline)
{
  .jcall(DCAdriver, "V", "setTailDecline", taildecline)
}

getCumulativeProduction <- function(DCAdriver){J(DCAdriver, "getCumulativeProduction")}
getEstimatedReserves <- function(DCAdriver) {J(DCAdriver, "getEstimatedReserves")}

saveOutput <- function(output,i,oilDCAeur, oilCum3Months,oilCum6Months, oilCumCurrent, oilmodelQi, oilmodelDi,oilmodelB, oilmodelTime, 
                       oilinitialDi, oilrefracDi, gasDCAeur, gasCum3Months, gasCum6Months, gasCumCurrent, gasmodelQi,gasmodelDi,gasmodelB,
                       gasmodelTime, gasinitialDi,gasrefracDi, api_list, ...){
  if (oilDCAeur == 3333333333 || is.na(oilDCAeur) || is.null(oilmodelQi) || is.null(oilmodelDi) || is.null(oilmodelB) || is.null(oilinitialDi) || is.null(oilrefracDi) || is.null(oilmodelTime)){
    oilDCAeur = 3333333333
    oilCum3Months = 3333333333
    oilCum6Months = 3333333333
    oilCumCurrent = 3333333333
    oilmodelQi = 3333333333
    oilmodelDi = 3333333333
    oilmodelB = 3333333333
    oilmodelTime = 3333333333
    oilinitialDi  = 3333333333
    oilrefracDi  = 3333333333
    
  }
  if (gasDCAeur == 3333333333 || is.na(gasDCAeur) || is.null(gasmodelQi) || is.null(gasmodelDi) || is.null(gasmodelB) || is.null(gasinitialDi) || is.null(gasrefracDi) || is.null(gasmodelTime)){
    gasDCAeur = 3333333333
    gasCum3Months = 3333333333
    gasCum6Months = 3333333333
    gasCumCurrent = 3333333333
    gasmodelQi = 3333333333
    gasmodelDi = 3333333333
    gasmodelB = 3333333333
    gasmodelTime = 3333333333
    gasinitialDi  = 3333333333
    gasrefracDi  = 3333333333
  } 
  
  output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
  output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
  output$Oil.Cum..3M[output$api == api_list[i]] = oilCum3Months
  output$Gas.Cum..3M[output$api == api_list[i]] = gasCum3Months
  output$Oil.Cum..6M[output$api == api_list[i]] = oilCum6Months
  output$Gas.Cum..6M[output$api == api_list[i]] = gasCum6Months
  output$Oil.Cum..Current[output$api == api_list[i]] = oilCumCurrent
  output$Gas.Cum..Current[output$api == api_list[i]] = gasCumCurrent
  output$Oil.Model.Qi[output$api == api_list[i]] = oilmodelQi
  output$Gas.Model.Qi[output$api == api_list[i]] = gasmodelQi
  output$Oil.Model.Di[output$api == api_list[i]] = oilmodelDi
  output$Gas.Model.Di[output$api == api_list[i]] = gasmodelDi
  output$Oil.Model.B[output$api == api_list[i]] = oilmodelB
  output$Gas.Model.B[output$api == api_list[i]] = gasmodelB
  output$Oil.Model.Time[output$api == api_list[i]] = oilmodelTime
  output$Gas.Model.Time[output$api == api_list[i]] = gasmodelTime
  output$Oil.Di.Initial[output$api == api_list[i]] = oilinitialDi
  output$Gas.Di.Initial[output$api == api_list[i]] = gasinitialDi
  output$Oil.Refrac.Di[output$api == api_list[i]] = oilrefracDi
  output$Gas.Refrac.Di[output$api == api_list[i]] = gasrefracDi
  
  return(output)
}



#setForecast <- function(years)
#{
#  J( as.double(years), "setForecastYears")
#}

#######################Script for DCA

## Essentially JNI interface
#.jinit initializes the Java Virtual Machine (JVM). This function must be called before any rJava
#functions can be used.
.jinit()
.jaddClassPath(dir("C:/Users/gordon.tsai/Documents/DCA Model", full.names =TRUE))
#jaddClassPath adds directories or JAR files to the class path.
#.jclassPath returns a vector containg the current entries in the class path
print(.jclassPath())
#.jinit(classpath = .jclassPath())


#### Error 111111111 = less than points of production
#### Error 333333333 = getEUR error probably one model() part from java

modelList = setModel("Arps")
modelYears = 35.0
taildecline = "Exponential"
gasEconLimit = 25.0 #Mcf/day
oilEconlimit = 1 #bbl/day

output_total = forecastDCA(input_reduced,"bbl","Mcf", oilsegmentation,gassegmentation, Time_Current,oilEconlimit,gasEconLimit)
output_prerefrac = forecastDCA(o,"bbl","Mcf",oilsegmentation,gassegmentation, Time_Current,oilEconlimit,gasEconLimit)
output_total  = output_total[,c(1,2,3,18,19,20,21)]
output_prerefrac  = output_prerefrac[,1:17]
names(output_total) = c("api", "TotalOilEURForecast", "TotalGasEURForecast", "TotalOilDiInitial","TotalGasDiInitial","TotalOilDiRefrac",
                        "TotalGasDiRefrac")
names(output_prerefrac) = c("api", "PreRefracOilEURForecast", "PreRefracGasEURForecast","ForecastCumOil_ThreeMonth",
                            "ForecastCumGas_ThreeMonth","ForecastCumOil_SixMonth","ForecastCumGas_SixMonth",
                            "ForecastCumOil","ForecastCumGas","OilModelQi","GasModelQi","OilModelDi","GasModelDi","OilModelB",
                            "GasModelB","OilModelTime","GasModelTime")
output = merge(output_total,output_prerefrac, by = "api")

write.csv(output, "C:/Users/gordon.tsai/Documents/DCA Model/output.csv", row.names = FALSE) 

