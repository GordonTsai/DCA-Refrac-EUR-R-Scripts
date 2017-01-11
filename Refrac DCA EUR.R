library("RODBC")
library("rJava")
library("ggplot2")
library("grid")
library("gridExtra")
library("dplyr")
#{
#  myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
#  #input < sqlFetch(myConn, [esp_stage].[dbo].[RefracIncrementalInput]
#  data <- sqlQuery(myConn,"Select * from [esp_stage].[dbo].[RefracIncrementalInput]")
#  close(myConn)
#}

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
##R.Version() 64 or 32 Progam Files refers to 64
#rm(list=ls())

count = 0 
input = data

forecastDCA <- function(input) {
  input = input[order(input$api),]
  api_list = unique(input$api)
  
  #Create columns for the output vector
  oilEUR = numeric(length(api_list))
  gasEUR = numeric(length(api_list))
  output = data.frame(api = api_list,'Oil EUR' = oilEUR, 'Gas EUR' = gasEUR)
  
  api = 4212131438
  i = match(api, api_list)
  #i = 27
#  for(i in 1:length(api_list)) {
    count = count + 1
    print(count)
    well = subset(input, input$api == api_list[i])
    date = well$productionDate
    oilProduction = well$oilProduction
    gasProduction = well$gasProduction
    
    ##########  Data Pull  ################### 
    #date
    #date = read.csv("DCA_date.csv", header = FALSE)
    date <- data.matrix((date));date <- data.matrix(as.integer(date))
    
    #production 
    #production = read.csv("DCA_production.csv", header = FALSE)
    oilProduction <- data.matrix(oilProduction);oilProduction <- as.numeric(oilProduction)
    gasProduction <- data.matrix(gasProduction);gasProduction <- as.numeric(gasProduction)
    
    #Units for constructor are either "bbl" or "Mcf"
    oilunit <- 'bbl'
    gasunit <- 'Mcf'
    
    ## Essentially JNI interface
    #.jinit initializes the Java Virtual Machine (JVM). This function must be called before any rJava
    #functions can be used.
    .jinit()
    .jaddClassPath(dir( "C:/Users/gordon.tsai/Documents/DCA Model", full.names=TRUE ))
    #jaddClassPath adds directories or JAR files to the class path.
    #.jclassPath returns a vector containg the current entries in the class path
    #print(.jclassPath())
    
    modelList <- new( J("java.util.ArrayList") )
    #new class Arraylist 
    #Creates a Java class reference or calls a Java method
    # J(class, method, ...)
    J( modelList, "add", "Arps")
    
    if(length(well$api)<5)
    {
      oilDCAeur = 111111111
      gasDCAeur = 111111111
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      
    
    } else if(length(oilProduction[oilProduction != 0])<5 || length(oilProduction[oilProduction == 0]/length(oilProduction) < .25)) {
      
      oilDCAeur = 2222222222
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      
      gasDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,gasProduction,gasunit) #main (dates[S],values[D],units[L] )
      .jcall( gasDCAdriver, "V", "setModelTypes", modelList )
      J( gasDCAdriver , "setSegmentation", TRUE, 5.0)
      #J( gasDCAdriver, "getModelTypes")
      tryCatch({J( gasDCAdriver, "model")
      #.jcall( gasDCAdriver, "S", "getReport")
      gasDCAeur <- J( gasDCAdriver, "getEUR")
      #gasPrediction <- .jcall( gasDCAdriver, "[D", "getPrediction")
      #gasProduction <- .jcall(gasDCAdriver, "[D", "getProduction") 
      
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur}, warning = function(w) {
        gasDCAeur = 3333333333
        return(gasDCAeur)
      }, error = function(e) {
        gasDCAeur = 3333333333
        return(gasDCAeur)
      }, finally = {
        
      })
      print(J(gasDCAdriver, "getTransitionMonth"))
      
      
    } else if(length(gasProduction[gasProduction != 0])<5) {
    
      gasDCAeur = 2222222222
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      
      oilDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,oilProduction,oilunit) #main (dates[S],values[D],units[L] )
      .jcall( oilDCAdriver, "V", "setModelTypes", modelList )
      J( oilDCAdriver , "setSegmentation", TRUE, 5.0)
      J( oilDCAdriver, "getModelTypes")
      
      tryCatch({
      J( oilDCAdriver, "model")
      .jcall( oilDCAdriver, "S", "getReport")
      oilDCAeur <- J( oilDCAdriver, "getEUR")
      #oilPrediction <- .jcall( oilDCAdriver, "[D","getPrediction")
      #oilProduction <- .jcall(oilDCAdriver, "[D", "getProduction") 
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur}, warning = function(w) {
        oilDCAeur = 3333333333
        return(oilDCAeur)
      }, error = function(e) {
        oilDCAeur = 3333333333
        return(oilDCAeur)
      }, finally = {
        
      })
      print(J(oilDCAdriver, "getTransitionMonth"))
      
        
    } else {  
      oilDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,oilProduction,oilunit) #main (dates[S],values[D],units[L] )
      .jcall( oilDCAdriver, "V", "setModelTypes", modelList )
      J( oilDCAdriver , "setSegmentation", TRUE, 5.0)
      J( oilDCAdriver, "getModelTypes")
      tryCatch({J( oilDCAdriver, "model")
      .jcall( oilDCAdriver, "S", "getReport")
      oilDCAeur <- J( oilDCAdriver, "getEUR")
      #oilDCAfit <- J( oilDCAdriver, "getFitCC")
      #oilDCAmodel <- J( oilDCAdriver, "getLastSegmentModel")
      #oilDCAtype <- J( oilDCAmodel, "getType" )
      #oilParms <- J( oilDCAmodel, "getModel" )
      #oilModels <- J( oilDCAdriver, "getModel")
      #oilPrediction <- .jcall( oilDCAdriver, "[D","getPrediction")
      #.jcall(oilDCAdriver, "S", "getLastSegmentModelType" )
      #.jinstanceof(oilDCAdriver,"com.drillinginfo.dca.DCAdriver")
      #oilProduction <- .jcall(oilDCAdriver, "[D", "getProduction") 
      #.jcall(oilDCAdriver, "S", "getIOunits")
      #.jcall(oilDCAdriver, "[J", "getMonths") 
      },warning = function(w) {
        oilDCAeur = 3333333333
        return(oilDCAeur)
      }, error = function(e) {
        oilDCAeur = 3333333333
        return(oilDCAeur)
      }, finally = {
      })
      
        
      gasDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,gasProduction,gasunit) #main (dates[S],values[D],units[L] )
      .jcall( gasDCAdriver, "V", "setModelTypes", modelList )
      J( gasDCAdriver , "setSegmentation", TRUE, 5.0)
      J( gasDCAdriver, "getModelTypes")
      tryCatch({
      J( gasDCAdriver, "model")
      .jcall( gasDCAdriver, "S", "getReport")
      gasDCAeur <- J( gasDCAdriver, "getEUR")
      #gasDCAfit <- J( gasDCAdriver, "getFitCC")
      #gasDCAmodel <- J( gasDCAdriver, "getLastSegmentModel")
      #gasDCAtype <- J( gasDCAmodel, "getType" )
      #gasParms <- J( gasDCAmodel, "getModel" )
      #gasModels <- J( gasDCAdriver, "getModel")
      #gasPrediction <- .jcall( gasDCAdriver, "[D", "getPrediction")
      #.jcall(gasDCAdriver, "S", "getLastSegmentModelType" )
      #.jinstanceof(gasDCAdriver,"com.drillinginfo.dca.DCAdriver")
      #gasProduction <- .jcall(gasDCAdriver, "[D", "getProduction") 
      #.jcall(gasDCAdriver, "S", "getIOunits")
      #.jcall(gasDCAdriver, "[J", "getMonths") 
      },warning = function(w) {
        gasDCAeur = 3333333333
        return(gasDCAeur)
      }, error = function(e) {
        gasDCAeur = 3333333333
        return(gasDCAeur)
      }, finally = {
      })
      
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      print(J(oilDCAdriver, "getTransitionMonth"))
      print(J(gasDCAdriver, "getTransitionMonth"))
      
    }
#  }
  return(output)
}



plotForecast <- function(oilDriverDCA, gasDriverDCA) {
  
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


output_total = forecastDCA(input)

#input_reduced = subset(input, input$Time2Refrac > 4)
#refracAPI = unique(input_reduced$api)
#o = data.frame()
#count = 0 
#for(i in 1:length(refracAPI)) {
#  count = count+1 ; print(count)
#  c = filter(input_reduced, api == refracAPI[i]) 
#  c = filter(c, Time2Refrac == min(Time2Refrac))
#  c = filter(c, RefraccompletionDate == min(RefraccompletionDate))
#  c = filter(c, ProductionMonth <= Time2Refrac[1]) 
#  
#  o = rbind(o,c)
#}

#output_prerefrac = forecastDCA(o)

#names(output_total) = c("api", "Total Oil EUR", "Total Gas EUR")
#names(output_prerefrac) = c("api", "Pre-Refrac Oil EUR", "Pre-Refrac Gas EUR")

#output = merge(output_total,output_prerefrac, by = "api")

#write.csv(output, "C:/Users/gordon.tsai/Documents/DCA Model/output.csv", row.names = FALSE) 
