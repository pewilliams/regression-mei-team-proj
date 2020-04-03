
setwd("C:\\Dev\\projects\\isye-6414")
source('analysis\\libraries.R')
source('analysis\\util.R')
source('analysis\\prep-model-data.R')


######################################################################################### 
# Ordinary Least Squares Regression on smf (suicide_male_and_female):
######################################################################################### 
message("Ordinary Least Squares Regression on smf (suicide_male_and_female)")
message()

# train and build the model
 
# par(mfrow = c(2,2))
# plot(mfSuicideModel) 

# plot the residuals, should look like a gaussian distribution
res <- residuals(mfSuicideModel)
res <- as.data.frame(res)
print(ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.5))

# test the model's predictions
mfSuicidePredictions <- predict(mfSuicideModel, testData)
results <- cbind(mfSuicidePredictions, testData$smf)
results <- computeError(results, data$smf) # R-squared: -0.381804324664092
# min(results) # full model contains a negative prediction

if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message()
  message("Zeroing negative predicted values...")
  message()
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$smf)
} # R-squared: 0.0290596803188109

bptest(mfSuicideModel) # BP = 12.148, df = 7, p-value = 0.09579

##
# RESULTS:
#   Adjusted R-squared = 0.2283, model is only explaining about 22.8% of the variability
#   Heteroscedasticity: p-value = 0.09579, Weighted LS might be useful
#   Predicts negative suicide rates
#   R-squared on predictions: -0.3818
#   Correcting for negative predictions improved R-squared from -0.3818 to 0.0291
##
# CONCLUSION: 
#   The model is a poor fit for the data.
#   Try removing outliers.
#   Try Model/Variable selection.
#   Transform some variables.
#   Try modeling with interation terms.
#   Try poisson regression.
##
message()
message()
#par(mfrow = c(2,2))
plot(mfSuicideModel) 

##
# IDENTIFY AND HANDLE OUTLIERS
##
# from the plot, remove 3 outlier observations
#  #65 Guyana
#  #88 Lesotho
# #127 Russian Federation
# print(trainingData["65",]) 
#rTrainingData1 <- subset(trainingData, clab != "Colombia") # example of removing by subset
#rTrainingData1 <- trainingData[!rownames(trainingData) %in% c("65", "88", "127"), ]
rTrainingData1 <- trainingData[!rownames(trainingData) %in% c("65", "77", "79", "143"), ]
rmfSuicideModel <- lm(smf ~ hepg + fmlpr + gdp + la + sps + pimh + mh, data = rTrainingData1)
summary(rmfSuicideModel) # Adjusted R-squared:  0.2765
# test the model's predictions
rmfSuicidePredictions <- predict(rmfSuicideModel, testData)
results <- cbind(rmfSuicidePredictions, testData$smf)
results <- computeError(results, data$smf) # predicted negative values, R-squared: -0.305631818007266
if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message()
  message("Zeroing negative predicted values...")
  message()
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$smf)
} # R-squared: 0.00074217208276417
#plot(rmfSuicideModel)

bptest(rmfSuicideModel) # BP = 14.852, df = 7, p-value = 0.03794

##
# RESULTS:
#   Adjusted R-squared:  0.2765, model is only explaining about 27.65% of the variability
#   Heteroscedasticity:  p-value = 0.03794, suggests Weighted LS might be useful
#   Predicts negative suicide rates
#   R-squared on predictions: -0.3056
#   Correcting for negative predictions improved R-squared from -0.3056 to 0.0007
##
# CONCLUSION: 
#   The model is a poor fit for the data. 
#   Try Model/Variable selection.
#   Transform some variables.
#   Try modeling with interation terms.
#   Try poisson regression.
##

##
# MODEL/VARIABLE SELECTION
# Use Best subset algorithm to search all possible models and choose the one with 
# optimal criteria (largest R^2)
##

bestModelFormula <- getBestModelFormula(colnames(rTrainingData1[, 4:10]),
                                        rTrainingData1[,4:10],
                                        colnames(rTrainingData1)[1],
                                        rTrainingData1[,1])
print(bestModelFormula)

# Best model selected 
# test out it's prediction accuracy
#best.rmfSuicideModel <- lm(smf ~ fmlpr + gdp + la + sps + mh, data = rTrainingData1)
best.rmfSuicideModel <- lm(bestModelFormula, data = rTrainingData1)
summary(best.rmfSuicideModel) # Adjusted R-squared:  0.2897

rmfSuicidePredictions <- predict(best.rmfSuicideModel, testData)
results <- cbind(rmfSuicidePredictions, testData$smf)
results <- computeError(results, data$smf) # predicted negative values, R-squared: R-squared: -0.3086
if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message()
  message("Zeroing negative predicted values...")
  message()
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$smf)
} # R-squared: -0.011581177619467

bptest(best.rmfSuicideModel) # BP = 7.3867, df = 5, p-value = 0.1934

##
# RESULTS:
#   Adjusted R-squared:  0.2897, model is only explaining about 28.97% of the variability
#   Heteroscedasticity:  p-value = 0.1934, suggests Weighted LS may NOT be useful
#   Predicts negative suicide rates
#   R-squared on predictions: -0.3086
#   Correcting for negative predictions improved R-squared from -0.3086 to -0.0116
##
# CONCLUSION: 
#   The model is a poor fit for the data. 
#   Transform some variables.
#   Try modeling with interation terms.
#   Try poisson regression.
##

##
# VARIABLE TRANSFORMATION
# Use a power transformation on smf
##

# From the histogram on Y = smf, we see a tail to the right, not normal distribution 
ggplot(data, aes(x = smf)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')

# Apply a power transformation on Y
# bcTransformation <- boxcox(best.rmfSuicideModel)
bcTransformation <- boxcox(best.rmfSuicideModel, lambda = seq(0, 1, 0.025)) # ~ 0.5
bcTransformation$x[which.max(bcTransformation$y)] # 0.4545455, so sqrt(Y) or Y^0.5 might be best

# transform smf and add it as a new column in the data frame
rTrainingData1.smf.transformed <- sqrt(rTrainingData1$smf)
rTrainingData1 <- transform(rTrainingData1, smfTransformed = rTrainingData1.smf.transformed)

# plot another histogram of the transformed data
ggplot(rTrainingData1, aes(x = smfTransformed)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# histogram looks "more" normally distributed

# In the best model formula, replace the Y with the transformed Y 
transformedBestFormula <- update.formula(bestModelFormula, smfTransformed ~ .)

# train the new model
best.rmfSuicideModel2 <- lm(transformedBestFormula, data = rTrainingData1)
summary(best.rmfSuicideModel2) # Adjusted R-squared:  0.306

# apply the same transformation on the test data, then test out the prediction accuracy
testData <- transform(testData, smfTransformed = sqrt(testData$smf))
rmfSuicidePredictions <- predict(best.rmfSuicideModel2, testData)
results <- cbind(rmfSuicidePredictions, testData$smfTransformed)
results <- computeError(results, data$smf) # predicted negative values, R-squared: 0.9853
min(results) # small negative value
if(min(results) < 0) {
  # zero any negative predictions
  # improve accuracy without altering the model
  message()
  message("Zeroing negative predicted values...")
  message()
  results$predicted <- sapply(results$predicted, setZero)
  results <- computeError(results, data$smf)
} # R-squared: R-squared: 0.9853

bptest(best.rmfSuicideModel2) # BP = 5.1452, df = 5, p-value = 0.3984
##
# RESULTS:
#   Adjusted R-squared:  ~ 0.30 for the model after power transformation on Y
#   Heteroscedasticity:  p-value = 0.3984, suggests Weighted LS may NOT be useful
#   R-squared on predictions: ~ 0.98 on predictions (HOW?!?!?!)
#   No negative predictions when changing the seed from 1 to 2
##
##
# CONCLUSION:
#   The model predicts well on test data, but has a low R^2. I wouldn't trust this model yet.
#   Try modeling with interation terms.
#   Try poisson regression.
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
#   These models are not a good fit for the data
#   Try poisson regression.
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

bptest(mSuicideModel) # BP = 16.112, df = 7, p-value = 0.02411

##
# RESULTS:
#   Adjusted R-squared = 0.2846, model is only explaining about 28.46% of the variability
#   Heteroscedasticity: p-value = 0.02411, Weighted LS might be useful
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

bptest(fSuicideModel) # BP = 4.3884, df = 7, p-value = 0.7341

##
# RESULTS:
#   Adjusted R-squared = 0.115, model is only explaining about 11.5% of the variability
#   Heteroscedasticity: p-value = 0.7341, Weighted LS might not be useful
#   Predicts negative suicide rates
#   R-squared on predictions: 0.4720
#   Correcting for negative predictions improved R-squared from 0.4720 to 0.6033
##
# CONCLUSION: 
#   The model fit can be improved. Will compare results to poisson regression.
##
