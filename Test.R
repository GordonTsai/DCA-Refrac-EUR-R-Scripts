library("stringr")
library("aRpsDCA")


#teststring = "Number of model segments: 1\nArps' Equation Model\n    Qi: 22395.923432061933 (Mcf)\n    Di: 0.18455931415583487\n     b: 0.01624352226720648\n    Secant decline:     0.886483298892329\n    Number of months:   7\n    Number of outliers: 0\n    Correlation coeff:  0.827580451965332\n    Rank correl coeff:  0.9402561783790588\n  Segment 1 start date: 2013-11-30\n\n"
teststring = "Number of model segments: 6\nArps' Equation Model\n Qi: 46048.830101569714 (Mcf)\n Di: 0.11273598068044603\n b: 0.9977371664654213\n Secant decline: 0.5752503072573583\n Number of months: 10\n Number of outliers: 0\n Correlation coeff: 0.9888899326324463\n Rank correl coeff: 1.0\n Segment 1 start date: 2009-11-30\n\nArps' Equation Model\n Qi: 24297.224696356272 (Mcf)\n Di: 0.08361474465516015\n b: 1.0E-4\n Secant decline: 0.6333423136605215\n Number of months: 5\n Number of outliers: 0\n Correlation coeff: 0.9828383922576904\n Rank correl coeff: 1.0\n Segment 2 start date: 2010-09-30\n\nArps' Equation Model\n Qi: 14972.744086937993 (Mcf)\n Di: 0.05216362518644789\n b: 1.9473684210526316\n Secant decline: 0.33588177102261574\n Number of months: 7\n Number of outliers: 0\n Correlation coeff: 0.7656564712524414\n Rank correl coeff: 0.20000000298023224\n Segment 3 start date: 2011-02-28\n\nArps' Equation Model\n Qi: 11365.800411961074 (Mcf)\n Di: 0.527973192722021\n b: 0.08138855032317636\n Secant decline: 0.9939602078694286\n Number of months: 19\n Number of outliers: 0\n Correlation coeff: 0.8909915089607239\n Rank correl coeff: 0.8492053747177124\n Segment 4 start date: 2011-09-30\n\nArps' Equation Model\n Qi: 459.40784146601317 (Mcf)\n Di: 0.8\n b: 0.803524227572981\n Secant decline: 0.9324083358968605\n Number of months: 16\n Number of outliers: 0\n Correlation coeff: 0.9501703381538391\n Rank correl coeff: 0.800000011920929\n Segment 5 start date: 2013-04-30\n\nArps' Equation Model\n Qi: 2867.036011080332 (Mcf)\n Di: 0.8\n b: 1.0E-4\n Secant decline: 0.9999319586498391\n Number of months: 25\n Number of outliers: 0\n Correlation coeff: 0.9625533819198608\n Rank correl coeff: 0.9149914383888245\n Segment 6 start date: 2014-08-31\n\n"

getDCAmonths <- function(string,x)
{
  regularexpression = ": [+]?[0-9]?"
  #regularexpression = ": [+]?[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?"
  
  output = c()
  pattern = paste(x,regularexpression,sep = "")
  qiString = unlist(str_extract_all(string,pattern))
  value= as.numeric(str_sub(qiString,start = 3+nchar(x)))
  output = value
  return(output)
}


output = getDCAmonths(teststring, "Number of months")
