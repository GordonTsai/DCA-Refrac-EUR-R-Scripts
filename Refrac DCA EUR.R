library("RODBC")
library("rJava")
library("ggplot2")
library("grid")
library("gridExtra")
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
  
 # for(i in 1:length(api_list)) {
    count = count + 1
    print(count)
    well = subset(input, input$api == api_list[i])
    date = well$productionDate
    oilProduction = well$oilProduction
    gasProduction = well$gasProduction
    
    if(length(well$api)<5)
    {
      oilDCAeur = 111111111
      gasDCAeur = 111111111
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
    
    } else {
    
    
      ##########  Data Pull  ################### 
      #date
      #date = read.csv("DCA_date.csv", header = FALSE)
      date <- data.matrix((date))
      date <- data.matrix(as.integer(date))
    
      #production 
      #production = read.csv("DCA_production.csv", header = FALSE)
      oilProduction <- data.matrix(oilProduction)
      oilProduction <- as.numeric(oilProduction)
      gasProduction <- data.matrix(gasProduction)
      gasProduction <- as.numeric(gasProduction)
    
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
      print(.jclassPath())
    
    
      modelList <- new( J("java.util.ArrayList") )
      #new class Arraylist
      #Creates a Java class reference or calls a Java method
      # J(class, method, ...)
      J( modelList, "add", "Arps")
    
      oilDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,oilProduction,oilunit) #main (dates[S],values[D],units[L] )
      gasDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,gasProduction,gasunit) #main (dates[S],values[D],units[L] )
      
      
      .jcall( oilDCAdriver, "V", "setModelTypes", modelList )
      .jcall( gasDCAdriver, "V", "setModelTypes", modelList )
      #J(oilDCAdriver, "setForecastYears", 10)
      J( oilDCAdriver , "setSegmentation", TRUE, 5.0)
      J( gasDCAdriver , "setSegmentation", TRUE, 5.0)
      J( oilDCAdriver, "getModelTypes")
      J( gasDCAdriver, "getModelTypes")
      J( oilDCAdriver, "model")
      J( gasDCAdriver, "model")
      print(.jcall( oilDCAdriver, "S", "getReport"))
      .jcall( gasDCAdriver, "S", "getReport")
      
      
      oilDCAeur <- J( oilDCAdriver, "getEUR")
      gasDCAeur <- J( gasDCAdriver, "getEUR")
      oilDCAfit <- J( oilDCAdriver, "getFitCC")
      gasDCAfit <- J( gasDCAdriver, "getFitCC")
      oilDCAmodel <- J( oilDCAdriver, "getLastSegmentModel")
      gasDCAmodel <- J( gasDCAdriver, "getLastSegmentModel")
      oilDCAtype <- J( oilDCAmodel, "getType" )
      gasDCAtype <- J( gasDCAmodel, "getType" )
      oilParms <- J( oilDCAmodel, "getModel" )
      gasParms <- J( gasDCAmodel, "getModel" )
      
      
      oilModels <- J( oilDCAdriver, "getModel")
      gasModels <- J( gasDCAdriver, "getModel")
      oilPrediction <- .jcall( oilDCAdriver, "[D","getPrediction")
      gasPrediction <- .jcall( gasDCAdriver, "[D", "getPrediction")
      .jcall(oilDCAdriver, "S", "getLastSegmentModelType" )
      .jcall(gasDCAdriver, "S", "getLastSegmentModelType" )
      
      .jinstanceof(oilDCAdriver,"com.drillinginfo.dca.DCAdriver")
      .jinstanceof(gasDCAdriver,"com.drillinginfo.dca.DCAdriver")
      
      oilProduction <- .jcall(oilDCAdriver, "[D", "getProduction") 
      gasProduction <- .jcall(gasDCAdriver, "[D", "getProduction") 
      .jcall(oilDCAdriver, "S", "getIOunits")
      .jcall(gasDCAdriver, "S", "getIOunits")
      .jcall(oilDCAdriver, "[J", "getMonths") 
      .jcall(gasDCAdriver, "[J", "getMonths") 
      
      output$Oil.EUR[output$api == api_list[i]] = oilDCAeur
      output$Gas.EUR[output$api == api_list[i]] = gasDCAeur
      
    }
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
    
    
    
  #}
}

output = forecastDCA(input)