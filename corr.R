make.calculate <- function (){
  calculate <- function(dataset,treshold){
    myfundata <- dataset[,c('sulfate','nitrate')]
    myfundata <- myfundata[!is.na(myfundata$nitrate),]
    myfundata <- myfundata[!is.na(myfundata$sulfate),]
    cleanValues <- length(myfundata$sulfate)
    if (cleanValues > treshold){
     return (list(myfundata=myfundata))
    }
  }
  return(calculate)
}

corr <- function(directory, treshold = 0) {
  f1 <- make.calculate()
  crResults <- numeric()
  for (i in 1:332){
  fixedid <- sprintf("%03d",i)
  filepath <- file.path(getwd(),directory,paste(fixedid,".csv",sep=""))
  mydata <- read.table(filepath,header=TRUE,sep=',')
  result <- f1(mydata,treshold)
  lengthValue <- nrow(result$myfundata)
  if (!is.null(lengthValue)){
    print (lengthValue)
    correlation <- cor(result$myfundata$sulfate, result$myfundata$nitrate)
    crResults <- c(crResults,correlation)
    }
  
  }
  return(crResults)
}