library(MASS) # Box-Cox transformation
# library(lattice) # splom
library(ggplot2)
library(ggthemes)
# library(dplyr)
# library(corrgram)
# library(corrplot)
library(caTools) # sample splitting
library(data.table)

setwd("C:\\Dev\\projects\\isye-6414")
source('analysis\\util.R')
source('analysis\\prep-data.R')



######################################################################################### 
# Ordinary Least Squares Regression on smf (suicide_male_and_female):
######################################################################################### 
message("Ordinary Least Squares Regression on smf (suicide_male_and_female)")

# train and build the model
mfSuicideModel <- lm(smf ~ hepg + fmlpr + gdp + la + sps + pimh + mh, data = trainingData)
summary(mfSuicideModel)
# par(mfrow = c(2,2))
# plot(mfSuicideModel) 

# plot the residuals, should look like a gaussian distribution
res <- residuals(mfSuicideModel)
res <- as.data.frame(res)
print(ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5))

# test the model's predictions
mfSuicidePredictions <- predict(mfSuicideModel, testData)
results <- cbind(mfSuicidePredictions, testData$smf)
results <- computeError(results, data$smf)
# min(results) # full model contains a negative prediction

if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message("Zeroing negative predicted values...")
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$smf)
}

##
# RESULTS:
#   Adjusted R-squared = 0.2283, model is only explaining about 22.8% of the variability
#   Heteroscedasticity - transformation of variables or different modeling is called for
#   Predicts negative suicide rates
#   R-squared on predictions: -0.3818
#   Correcting for negative predictions improved R-squared from -0.3818 to 0.0291
##
# CONCLUSION: 
#   The model is a poor fit for the data. Will compare results to poisson regression.
##
message()
message()
par(mfrow = c(2,2))
plot(mfSuicideModel) 

# from the plot, remove 3 outlier observations
#  #65 Guyana
#  #88 Lesotho
# #127 Russian Federation
# print(trainingData["65",]) 
#rTrainingData1 <- subset(trainingData, clab != "Colombia") # example of removing by subset
rTrainingData1 <- trainingData[!rownames(trainingData) %in% c("65", "88", "127"), ]
rmfSuicideModel <- lm(smf ~ hepg + fmlpr + gdp + la + sps + pimh + mh, data = rTrainingData1)
summary(rmfSuicideModel) # Adjusted R-squared:  0.2765
# test the model's predictions
rmfSuicidePredictions <- predict(rmfSuicideModel, testData)
results <- cbind(rmfSuicidePredictions, testData$smf)
results <- computeError(results, data$smf) # predicted negative values, R-squared: -0.305631818007266
if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message("Zeroing negative predicted values...")
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$smf)
} # R-squared: 0.00074217208276417
plot(rmfSuicideModel)
##
# RESULTS:
#   Adjusted R-squared:  0.2765, model is only explaining about 27.65% of the variability
#   Heteroscedasticity - transformation of variables or different modeling is called for
#   Predicts negative suicide rates
#   R-squared on predictions: -0.3056
#   Correcting for negative predictions improved R-squared from -0.3056 to 0.0007
##
# CONCLUSION: 
#   The model is a poor fit for the data. Transform some variables. 
#   Will compare results to poisson regression.
##



##
# Model with interaction terms
##
# mfSuicideModel2 <- lm(smf ~ hepg * fmlpr * gdp + la + sps + pimh * mh, data = trainingData)
# summary(mfSuicideModel2) # Adjusted R-squared:  0.1998
# 
# mfSuicideModel3 <- lm(smf ~ hepg + fmlpr * gdp + la + sps + pimh + mh, data = trainingData)
# summary(mfSuicideModel3) # Adjusted R-squared:  0.2214
# 
# mfSuicideModel4 <- lm(smf ~ hepg + fmlpr + gdp * la + sps + pimh + mh, data = trainingData)
# summary(mfSuicideModel4) # Adjusted R-squared:  0.2211
# 
# mfSuicideModel5 <- lm(smf ~ hepg + fmlpr + gdp + la + sps + pimh * mh, data = trainingData)
# summary(mfSuicideModel5) # Adjusted R-squared:  0.2217
# 
# mfSuicideModel6 <- lm(smf ~ hepg * fmlpr * gdp * la * sps * pimh * mh, data = trainingData)
# summary(mfSuicideModel6) # Adjusted R-squared:  0.3198
# 
# # analyze the model with interaction terms
# res <- residuals(mfSuicideModel6)
# res <- as.data.frame(res)
# # head(res)
# print(ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5))
# 
# # test the model's predictions
# mfSuicidePredictions6 <- predict(mfSuicideModel6, testData)
# results <- cbind(mfSuicidePredictions6, testData$smf)
# results <- computeError(results, data$smf) # R-squared: -301779.899654406
# #plot(mfSuicideModel6)
# 
# if(min(results) < 0) {
#   # zero any negative predictions
#   # improve accuracy without altering the model
#   message("Zeroing negative predicted values...")
#   results$predicted <- sapply(results$predicted, setZero)
#   results <- computeError(results, data$smf) # R-squared: -287168.733916664
# } 
##
# CONCLUSION:
#  These models are not a good fit for the data
##


######################################################################################### 
# Ordinary Least Squares Regression on sm (suicide_male):
######################################################################################### 
message("Ordinary Least Squares Regression on sm (suicide_male)")

# train and build the model
mSuicideModel <- lm(sm ~ hepg + fmlpr + gdp + la + sps + pimh + mh, data = trainingData)
#summary(mSuicideModel)
# plot(mSuicideModel) 
res <- residuals(mSuicideModel)
res <- as.data.frame(res)
# head(res)
print(ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5))

# test the model's predictions
mSuicidePredictions <- predict(mSuicideModel, testData)
results <- cbind(mSuicidePredictions, testData$sm)
results <- computeError(results, data$sm)
# min(results) # full model contains a negative prediction

if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message("Zeroing negative predicted values...")
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$sm)
}

##
# RESULTS:
#   Adjusted R-squared = 0.2846, model is only explaining about 28.46% of the variability
#   Heteroscedasticity - transformation of variables or different modeling is called for
#   Predicts negative suicide rates
#   R-squared on predictions: 0.1666
#   Correcting for negative predictions improved R-squared from 0.1666 to 0.3715
##
# CONCLUSION: 
#   The model fit can be improved. Will compare results to poisson regression.
##
message()
message()
######################################################################################### 
# Ordinary Least Squares Regression on sf (suicide_female):
######################################################################################### 
message("Ordinary Least Squares Regression on sf (suicide_female)")

# train and build the model
fSuicideModel <- lm(sf ~ hepg + fmlpr + gdp + la + sps + pimh + mh, data = trainingData)
#summary(fSuicideModel)
# plot(fSuicideModel) 
res <- residuals(fSuicideModel)
res <- as.data.frame(res)
# head(res)
print(ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5))

# test the model's predictions
fSuicidePredictions <- predict(fSuicideModel, testData)
results <- cbind(fSuicidePredictions, testData$sf)
results <- computeError(results, data$sf)
# min(results) # full model contains a negative prediction

if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message("Zeroing negative predicted values...")
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$sf)
}

##
# RESULTS:
#   Adjusted R-squared = 0.115, model is only explaining about 11.5% of the variability
#   Heteroscedasticity - transformation of variables or different modeling is called for
#   Predicts negative suicide rates
#   R-squared on predictions: 0.4720
#   Correcting for negative predictions improved R-squared from 0.4720 to 0.6033
##
# CONCLUSION: 
#   The model fit can be improved. Will compare results to poisson regression.
##
