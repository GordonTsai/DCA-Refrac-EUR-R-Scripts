library("RODBC")
library("rJava")
library("ggplot2")
{
  myConn <- odbcDriverConnect('driver={SQL Server};server=AUS2-CIS-DDB02V;trusted_connection=true')
  #input < sqlFetch(myConn, [esp_stage].[dbo].[RefracIncrementalInput]
  data <- sqlQuery(myConn,"Select * from [esp_stage].[dbo].[RefracIncrementalInput]")
  close(myConn)
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
##R.Version() 64 or 32 Progam Files refers to 64
#rm(list=ls())

input = data


forecastDCA(input){


  input = input[order(input$api),]
  api_list = unique(input$api)
  
  #Create columns for the output vector
  oilEUR = numeric(length(api_list))
  gasEUR = numeric(length(api_list))
  output = data.frame(api = api_list,'Oil EUR' = oilEUR, 'Gas EUR' = gasEU)R
  
  
  
  for(i in 1:length(api_list))
    well = subset(input, input$api == api_list[i])
    date = well$productionDate
    oilproduction = welll$oilProduction
    gasproduction = well$gasProduction
    
    ##########  Data Pull  ################### 
    #date
    #date = read.csv("DCA_date.csv", header = FALSE)
    date <- data.matrix((date))
    date <- data.matrix(as.integer(date))
  
    #production 
    #production = read.csv("DCA_production.csv", header = FALSE)
    oilproduction <- data.matrix(production)
    oilproduction <- as.numeric(production)
    gasproduction <- data.matrix(production)
    gasproduction <- as.numeric(production)
  
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
    J( modelList, "add", "Arps")
  
    oilDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,oilproduction,oilunit) #main (dates[S],values[D],units[L] )
    gasDCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,gasproduction,gasunit) #main (dates[S],values[D],units[L] )
    
    
    .jcall( DCAdriver, "V", "setModelTypes", modelList )
    J( DCAdriver, "getModelTypes")
    J( DCAdriver, "model")
    .jcall(DCAdriver, "S", "getReport")
  
    oilDCAeur <- J( oilDCAdriver, "getEUR")
    gasDCAeur <- J( gasDCAdriver, "getEUR")
    oilDCAfit <- J( oilDCAdriver, "getFitCC")
    gasDCAfit <- J( gasDCAdriver, "getFitCC")
    oilDCAmodel <- J( oilDCAdriver, "getLastSegmentModel")
    gasDCAmodel <- J( gaslDCAdriver, "getLastSegmentModel")
    oilDCAtype <- J( oilDCAmodel, "getType" )
    gasDCAtype <- J( gasDCAmodel, "getType" )
    oilParms <- J( oilDCAmodel, "getModel" )
    gasParms <- J( gasDCAmodel, "getModel" )
    
    
    oilModels <- J( oilDCAdriver, "getModel")
    gasModels <- J( gasDCAdriver, "getModel")
    oilPrediction <- .jcall( oilDCAdriver, "[D", "getPrediction")
    gasPrediction <- .jcall( gasDCAdriver, "[D", "getPrediction")
    .jcall(oilDCAdriver, "S", "getLastSegmentModelType" )
    .jcall(gasDCAdriver, "S", "getLastSegmentModelType" )
    
    .jinstanceof(oilDCAdriver,"com.drillinginfo.dca.DCAdriver")
    .jinstanceof(gasDCAdriver,"com.drillinginfo.dca.DCAdriver")
    
    oilproduction <- .jcall(oilDCAdriver, "[D", "getProduction") 
    gasproduction <- .jcall(gasDCAdriver, "[D", "getProduction") 
    .jcall(oilDCAdriver, "S", "getIOunits")
    .jcall(oilDCAdriver, "S", "getIOunits")
    .jcall(gasDCAdriver, "[J", "getMonths") 
    .jcall(gasDCAdriver, "[J", "getMonths") 
  
    
    
    
    
    
    
  ###########Plotting
  #xaxis <- matrix(c(1:198), nrow=198, ncol=1)
  #df2 <- data.frame(xaxis,production,prediction)
  
  #ggplot(df2, aes(xaxis, y = value, color = variable)) + 
  #  geom_line(aes(y = production, col = "Production")) +
  #  geom_line(aes(y = prediction, col = "Predicted"))
}

output = forecastDCA(input)