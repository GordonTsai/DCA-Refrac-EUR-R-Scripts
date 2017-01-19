library("RODBC")
library("rJava")
library("ggplot2")
library("grid")
library("gridExtra")
library("dplyr")

if(!exists("master_data")){
  {
    myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
    #input < sqlFetch(myConn, [esp_stage].[dbo].[RefracIncrementalInput]
    master_data <- sqlQuery(myConn,"Select * from [esp_stage].[dbo].[RefracIncrementalInput]")
    close(myConn)
  }
}
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

count = 0 
input = master_data

forecastDCA <- function(input,oilunit,gasunit, oilsegment,gassegment) {
  input = input[order(input$api),]
  api_list = unique(input$api)
  
  #Create columns for the output vector
  oilEUR = numeric(length(api_list))
  gasEUR = numeric(length(api_list))
  oilCum3Months= numeric(length(api_list))
  gasCum3Months= numeric(length(api_list))
  oilCum6Months = numeric(length(api_list))  
  gasCum6Months = numeric(length(api_list))
  output = data.frame(api = api_list,'Oil EUR' = oilEUR, 'Gas EUR' = gasEUR, 'Oil Cum. 3M' = oilCum3Months, 'Gas Cum. 3M' = oilCum6Months, 'Oil Cum. 6M' = oilCum6Months, 'Gas Cum. 6M' = gasCum6Months)
  
  #api = 4212131438
  #i = match(api, api_list)
  #i = 1
  for(i in 1:length(api_list)) {
    count = count + 1
    print(count)
    well = subset(input, input$api == api_list[i])
    date = well$productionDate
    oilProduction = well$oilProduction
    gasProduction = well$gasProduction
    #View(well)
    
    ##########  Data Pull  ################### 
    #date
    #date = read.csv("DCA_date.csv", header = FALSE)
    date <- data.matrix((date));date <- data.matrix(as.integer(date))
    
    #production 
    #production = read.csv("DCA_production.csv", header = FALSE)
    oilProduction <- data.matrix(oilProduction);oilProduction <- as.numeric(oilProduction)
    gasProduction <- data.matrix(gasProduction);gasProduction <- as.numeric(gasProduction)
    
    if(length(well$api)<5)
    {
      oilDCAeur = 111111111
      gasDCAeur = 111111111
      oilCum3Months = 111111111
      gasCum3Months = 111111111
      oilCum6Months = 111111111
      gasCum6Months = 111111111
      
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      output$Oil.Cum..3M[output$api == api_list[i]] = oilCum3Months
      output$Gas.Cum..3M[output$api == api_list[i]] = gasCum3Months
      output$Oil.Cum..6M[output$api == api_list[i]] = oilCum6Months
      output$Gas.Cum..6M[output$api == api_list[i]] = gasCum6Months
      
    } else  {
      gasDCAdriver = createDriver(date,gasProduction,gasunit); segmentation(gasDCAdriver, TRUE,3.0)
      oilDCAdriver = createDriver(date,oilProduction,oilunit); segmentation(oilDCAdriver, TRUE,3.0)
    }
    
    if(length(oilProduction[oilProduction != 0])<5 || length(oilProduction[oilProduction == 0]/length(oilProduction) < .25)) {
      
      oilDCAeur = 2222222222
      oilCum3Months = 2222222222
      oilCum6Months = 2222222222     
      
      gasDCAeur = getEUR(gasDCAdriver,"First",modelYears)
      gasCum3Months = tail(getCumulativePrediction(gasDCAdriver, max(well$ProductionMonth)+1+3), n = 1)
      gasCum6Months = tail(getCumulativePrediction(gasDCAdriver, max(well$ProductionMonth)+1+3), n = 1)
      
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      output$Oil.Cum..3M[output$api == api_list[i]] = oilCum3Months
      output$Gas.Cum..3M[output$api == api_list[i]] = gasCum3Months
      output$Oil.Cum..6M[output$api == api_list[i]] = oilCum6Months
      output$Gas.Cum..6M[output$api == api_list[i]] = gasCum6Months
      
      ###################################################################################################print(J(gasDCAdriver, "getTransitionMonth"))
      
      
    } else if(length(gasProduction[gasProduction != 0])<5 || length(gasProduction[gasProduction==0]/length(gasProduction<.25))) {
      
      gasDCAeur = 2222222222
      gasCum3Months = 2222222222
      gasCum6Months = 2222222222
      
      oilDCAeur = getEUR(oilDCAdriver, "First", modelYears)
      oilCum3Months = tail(getCumulativePrediction(oilDCAdriver, max(well$ProductionMonth)+1+3), n = 1)
      oilCum6Months = tail(getCumulativePrediction(oilDCAdriver, max(well$ProductionMonth)+1+6), n = 1)
      
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      output$Oil.Cum..3M[output$api == api_list[i]] = oilCum3Months
      output$Gas.Cum..3M[output$api == api_list[i]] = gasCum3Months
      output$Oil.Cum..6M[output$api == api_list[i]] = oilCum6Months
      output$Gas.Cum..6M[output$api == api_list[i]] = gasCum6Months
      
    } else {  
      
      oilDCAeur = getEUR(oilDCAdriver, "First", modelYears)
      gasDCAeur = getEUR(gasDCAdriver, "First", modelYears)      
      oilCum3Months = tail(getCumulativePrediction(oilDCAdriver, max(well$ProductionMonth)+1+3), n = 1)
      oilCum6Months = tail(getCumulativePrediction(oilDCAdriver, max(well$ProductionMonth)+1+6), n = 1)
      gasCum3Months = tail(getCumulativePrediction(gasDCAdriver, max(well$ProductionMonth)+1+3), n = 1)
      gasCum6Months = tail(getCumulativePrediction(gasDCAdriver, max(well$ProductionMonth)+1+6), n = 1)
      
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      output$Oil.Cum..3M[output$api == api_list[i]] = oilCum3Months
      output$Gas.Cum..3M[output$api == api_list[i]] = gasCum3Months
      output$Oil.Cum..6M[output$api == api_list[i]] = oilCum6Months
      output$Gas.Cum..6M[output$api == api_list[i]] = gasCum6Months
      
    }
  }
  return(output)
}



plotForecast <- function(oilDCAdriver, gasDCAdriver) {
  
  ###########Plotting
  xaxisOil <- matrix(c(1:length(oilProduction)), nrow=length(oilProduction), ncol=1) ; print(length(oilProduction)); print(length(oilPrediction))
  Time = xaxisOil
  df1 <- data.frame(xaxisOil,oilProduction,oilPrediction)
  
  gOil = ggplot(df1, aes(Time, y = Bbl, color = Flowstreams)) + 
    geom_line(aes(y = oilProduction, col = "Oil Production")) +
    geom_line(aes(y = oilPrediction, col = "Oil Predicted")) ; print(gOil)
  
  xaxisGas <- matrix(c(1:length(gasProduction)), nrow=length(gasProduction), ncol=1) ; print(length(gasProduction)); print(length(gasPrediction))
  Time = xaxisGas
  df2 <- data.frame(xaxisGas,gasProduction,gasPrediction)
  
  gGas = ggplot(df2, aes(Time, y = gasProduction, color = Flowstreams)) + 
    geom_line(aes(y = gasProduction, col = "Gas Production")) +
    geom_line(aes(y = gasPrediction, col = "Gas Predicted")) +  scale_fill_manual(values=c("#CC6666", "#9999CC")); print(gGas)
  
  grid.arrange(gOil,gGas, nrow = 2, top = toString(api))
  
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
{
  J(DCAdriver, "setSegmentation", boolean ,threshold)
}

getEstimatedReserves <- function(DCAdriver)
{
  J(DCAdriver, "getEstimatedReserves")
}


setModel <- function(modelType)
{
  modelList = new(J("java.util.ArrayList"))
  a = J( modelList, "add", modelType)
  return(modelList)
}

getProduct <- function(DCAdriver)
{
  .jcall(DCAdriver,"[D", "getProduction" )
}

getProduction <- function(DCAdriver,months)
{
  .jcall(DCAdriver,"[D", "getProduction",as.integer(months ))
}


getCumProduction <- function(DCAdriver)
{
  .jcall( DCAdriver,"[D", "getCumulativeProductionStream" )
}

##getCumulativeProduction 


getPredict <- function(DCAdriver)
{
  .jcall(DCAdriver,"[D", "getPrediction")
}

getPrediction <- function(DCAdriver, months)
{
  .jcall(DCAdriver,"[D", "getPrediction", as.integer(months))
}

#getCumulativePrediction <- function(DCAdriver)
#{
#  .jcall(DCAdriver,"[D", "getCumulativePrediction")
#}

getCumPrediction <- function(DCAdriver)
{
  .jcall( DCAdriver,"[D", "getCumulativePrediction" )
}


getCumulativePrediction <- function(DCAdriver, months)
{
  .jcall(DCAdriver,"[D", "getCumulativePrediction", as.integer(months))
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

#output_total = forecastDCA(a,"bbl","Mcf", oilsegmentation,gassegmentation)


output_total = forecastDCA(input,"bbl","Mcf", oilsegmentation,gassegmentation)

input_reduced = subset(input, input$Time2Refrac > 2)
refracAPI = unique(input_reduced$api)
o = data.frame()
count = 0 
for(i in 1:length(refracAPI)) {
  count = count+1 ; print(count)
  c = filter(input_reduced, api == refracAPI[i]) 
  c = filter(c, Time2Refrac == min(Time2Refrac))
  c = filter(c, RefraccompletionDate == min(RefraccompletionDate))
  c = filter(c, ProductionMonth <= Time2Refrac[1]) 
  
  o = rbind(o,c)
}
count = 0 

output_prerefrac = forecastDCA(o,"bbl","Mcf",oilsegmentation,gassegmentation)

names(output_total) = c("api", "Total Oil EUR", "Total Gas EUR")
names(output_prerefrac) = c("api", "Pre-Refrac Oil EUR", "Pre-Refrac Gas EUR")

output = merge(output_total,output_prerefrac, by = "api")

write.csv(output, "C:/Users/gordon.tsai/Documents/DCA Model/output.csv", row.names = FALSE) 

