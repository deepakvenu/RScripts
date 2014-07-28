##the function calculate calculates the sum of a given pollutant
##in one particular file and it also calculates the total number of
##entries in the file. The function pollutantmean is the main function
##that invokes the calculate function for each file and calculates
##the mean by dividing the sum of values by number of values

make.calculate <- function (){
  sumTotal <- 0
  values <- 0
  calculate <- function(dataset, pollutant){
    testset <- dataset[,c(pollutant)]
    testset <- testset[!is.na(testset)]
    sumTotal <- sum(testset)
    values <- length(testset)
    return (list(sumTotal = sumTotal,values = values))
  }
  return(calculate)
}


pollutantmean <- function(directory, pollutant, id = 1:332) {
  total <- 0
  value <- 0
  for (i in id) {
    fixedid <- sprintf("%03d",i)
    filepath <- file.path(getwd(),directory,paste(fixedid,".csv",sep=""))
    mydata <- read.table(filepath,header=TRUE,sep=',')
    f1 <- make.calculate()
    result <- f1(mydata,pollutant)
    total <- total + result$sumTotal
    value <- value + result$values
  }
  mean <- total / value
  print (mean)
}