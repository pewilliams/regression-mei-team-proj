# function to read in the data
readData <- function() {
  #setwd("C:\\Dev\\projects\\isye-6414")
  dataFrame <- read.table(file = "data_manipulation/alldata_nolabels.csv", sep=",")
  # colnames(data) <- c("Y1", "Y2", "Y3","X1","X2","X3", "X4","X5","X6", "X7") 
  colnames(dataFrame) <- c("smf", 
                           "sm", 
                           "sf",
                           "hepg",
                           "fmlpr",
                           "gdp", 
                           "la",
                           "sps",
                           "pimh", 
                           "mh") 
  
  #add country labels for reference
  c_labels <- fread('data_manipulation/country_labels.csv',
                    header=F,sep = ',')
  dataFrame <- transform(dataFrame,
                   clab = c_labels$V1)
  
  #Y1 = suicide_male_and_female
  #Y2 = suicide_male
  #Y3 = suicide_female
  #X1 = Current_health_expenditure_percentage_of_GDP
  #X2 = female_male_labor_participation
  #X3 = gdp/capita ppp
  #X4 = litres_alcohol_per_capita
  #X5 = strategy, binary
  #X6 = Psychiatrists working in mental health sector (per 100 000 population)
  #X7 = Mental hospitals (per 100 000 population)
  
  return(dataFrame)
}

# function for zeroing negative predictions
setZero <- function(x) {
  if(x < 0) {
    return(0)
  } else {
    return(x)
  }
}

# function to compute error statistics of model predictions
computeError <- function(results, dataColumn) {
  
  colnames(results) <- c('predicted', 'actual')
  results <- as.data.frame(results)
  # head(results)
  # min(results) # contains a negative prediction
  
  # mean squared error
  e <- results$actual - results$predicted
  MSE <- mean((e)^2)
  message("MSE:", MSE)
  
  # root mean squared error
  message("Root MSE:" , sqrt(MSE))
  
  # R-squared for predicted values
  SSE <- t(e)%*%e 
  SST <- sum((mean(data$smf) - results$actual)^2)
  rSquared <- 1 - SSE/SST
  message("R-squared: " , rSquared)
  return(results)
}
