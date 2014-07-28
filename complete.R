make.calculate <- function (){
  cleanValues <- 0
  calculate <- function(dataset){
  
    myfundata <- dataset[,c('sulfate','nitrate')]
    myfundata <- myfundata[!is.na(myfundata$sulfate),]
    myfundata <- myfundata[!is.na(myfundata$nitrate),]
    cleanValues <- length(myfundata$sulfate)
    return (list(cleanValues = cleanValues))
  }
  return(calculate)
}


complete <- function(directory, id = 1:332) {
  values <- "##"
  f1 <- make.calculate()
  loopIndex <- 1
  finalResult <- data.frame()
  for (i in id) {
    fixedid <- sprintf("%03d",i)
    filepath <- file.path(getwd(),directory,paste(fixedid,".csv",sep=""))
    mydata <- read.table(filepath,header=TRUE,sep=',')
    result <- f1(mydata)
    columnValues <- cbind("##",loopIndex,i,result$cleanValues)
    solution <- result$cleanValues
    finalResult <- rbind(finalResult,columnValues)
    loopIndex <- loopIndex + 1
  }
 rowHeader <- c("##"," ","id","nobs")
 names(finalResult) <- rowHeader
 print (finalResult, include.rownames = FALSE)
}

