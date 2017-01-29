



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
rm(list=ls())
library("RODBC")
library("rJava")
library("ggplot2")


##########  Data Pull  ################### 
#date
date = read.csv("DCA_date.csv", header = FALSE)
date <- data.matrix((date))
date <- data.matrix(as.integer(date))

#production 
production = read.csv("DCA_production.csv", header = FALSE)
production <- data.matrix(production)
production <- as.numeric(production)

unit <- 'Mcf'

## Essentially JNI interface
.jinit()
.jaddClassPath(dir( "C:/Users/gordon.tsai/Documents/DCA Model", full.names=TRUE ))
print(.jclassPath())


modelList <- new( J("java.util.ArrayList") )
J( modelList, "add", "Arps")

DCAdriver <- .jnew("com.drillinginfo.dca.DCAdriver",date,production,unit) #main (dates[S],values[D],units[L] )

.jcall( DCAdriver, "V", "setModelTypes", modelList )
J( DCAdriver, "getModelTypes")
J(DCAdriver, "setSegmentation", TRUE ,1.0)
J( DCAdriver, "model")
.jcall(DCAdriver, "S", "getReport")

DCAeur <- J( DCAdriver, "getEUR")
DCAfit <- J(DCAdriver, "getFitCC")
DCAmodel <- J( DCAdriver, "getLastSegmentModel")
DCAtype <- J( DCAmodel, "getType" )
parms <- J( DCAdriver, "getModel" )

models <- J(DCAdriver, "getModel")
prediction <- .jcall(DCAdriver, "[D", "getPrediction")
.jcall(DCAdriver, "S", "getLastSegmentModelType" )

.jinstanceof(DCAdriver,"com.drillinginfo.dca.DCAdriver")
production <- .jcall(DCAdriver, "[D", "getProduction") 
.jcall(DCAdriver, "S", "getIOunits")
.jcall(DCAdriver, "[J", "getMonths") 

xaxis <- matrix(c(1:198), nrow=198, ncol=1)
df2 <- data.frame(xaxis,production,prediction)

ggplot(df2, aes(xaxis, y = value, color = variable)) + 
  geom_line(aes(y = production, col = "Production")) +
  geom_line(aes(y = prediction, col = "Predicted"))
