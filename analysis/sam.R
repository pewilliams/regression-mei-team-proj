library(MASS) # Box-Cox transformation
library(lattice) # splom
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(data.table)



setwd("C:\\Dev\\projects\\isye-6414")
data <- read.table(file = "data_manipulation/alldata_nolabels.csv", sep=",")
# colnames(data) <- c("Y1", "Y2", "Y3","X1","X2","X3", "X4","X5","X6", "X7") 
colnames(data) <- c("smf", 
                    "sm", 
                    "sf",
                    "hepg",
                    "fmlpr",
                    "gdp", 
                    "la",
                    "sps",
                    "pimh", 
                    "mh") 

c_labels <- fread('data_manipulation/country_labels.csv',
                  header=F,sep = ',')
data <- transform(data,
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

# Round partial suicide counts up in order to user Poisson regression
# TODO: put these back into the data frame
# TODO: perform POISSON REGRESSION
ceiling(data$smf)
ceiling(data$sm)
ceiling(data$sf)

# TODO: feature selection
# TODO: model selection

######################################################################################### 
# Exploratory DATA ANALYSIS:
# check that the data was read in correctly
head(data)

# check for null or na values
any(is.na(data))

## NOT WORKING
# check that categorical features have levels assigned to them
# strat feature is numeric, but I want to change it to factor

# convert suicide prevention strat feature into a factor with 2 levels
# sp_factor <- factor(data$strat, levels = c(0, 1), labels = c("No", "Yes"))

# replace the old binary column with new factor column
# mfData <- c(data[, 4], data[, c(6:10, 1)])
# str(mfData)
#######

# get the numeric columns 
numeric_cols <- sapply(data, is.numeric)

# filter numeric columns for correlation
corr_data <- round(cor(data[, numeric_cols]), 4)

# The correlation table of R^2 (the last column is Y)
#round(cor(mfData)^2,4)

## Visualize the data:
# plots of all variables

## HISTOGRAM
ggplot(data, aes(x = smf)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: slight tail to the right

## BOX
ggplot(data, aes(x=smf)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
outlierValues <- boxplot(data$smf, plot=FALSE)$out
length(outlierValues)
min(outlierValues)
max(outlierValues)
# COMMENT: 
#  4 outliers: Guyana, 
#              Lesotho, 
#              Lithuania, 
#              Russian Federation
#


## HISTOGRAM
ggplot(data, aes(x = sm)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: slight tail to the right

## BOX
ggplot(data, aes(x=sm)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
outlierValues <- boxplot(data$sm, plot=FALSE)$out
length(outlierValues)
min(outlierValues)
max(outlierValues)
# COMMENT: 
#  6 outliers: Belarus, 
#              Guyana, 
#              Kazakhstan, 
#              Lithuania, 
#              Russian Federation
#              Suriname
# Guyana is an outlier for both women and men

ggplot(data, aes(x = sf)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')

# COMMENT: tail to the right

## BOX
ggplot(data, aes(x=sf)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
outlierValues <- boxplot(data$sf, plot=FALSE)$out
length(outlierValues)
min(outlierValues)
max(outlierValues)
# COMMENT: 
#  10 outliers: Cameroon
#              Chad
#              Guyana
#              India
#              Ivory Coast
#              Lesotho
#              Liberia
#              Nigeria
#              Sierra Leone
#              Uganda

ggplot(data, aes(x = hepg)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: appears normal

## BOX
ggplot(data, aes(x=hepg)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
outlierValues <- boxplot(data$hepg, plot=FALSE)$out
length(outlierValues)
min(outlierValues)
max(outlierValues)
# COMMENT: 
#  2 outliers: Sierra Leone
#              USA

ggplot(data, aes(x = fmlpr)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: tail to the left

## BOX
ggplot(data, aes(x=fmlpr)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
outlierValues <- boxplot(data$fmlpr, plot=FALSE)$out
length(outlierValues)
min(outlierValues)
max(outlierValues)
# COMMENT: 
#  5 outliers: Afghanistan
#              Algeria
#              Iran 
#              Iraq
#              Jordan


ggplot(data, aes(x = gdp)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: tail to the right
# look like poisson distribution 

ggplot(data, aes(x=gdp)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
outlierValues <- boxplot(data$gdp, plot=FALSE)$out
length(outlierValues)
min(outlierValues)
# COMMENT: 
#  7 outliers: Brunei Darussalam
#              Ireland
#              Kuwait
#              Luxembourg
#              Qatar
#              Singapore
#              United Arab Emirates


ggplot(data, aes(x = la)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: bi-modal?, is this a normal?

ggplot(data, aes(x=la)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
# COMMENT: No outliers


ggplot(data, aes(x = sps)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: categorical, high count of 'NO' compared to 'Yes'

ggplot(data, aes(x=sps)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
outlierValues <- boxplot(data$sps, plot=FALSE)$out
min(outlierValues)
# COMMENT: much fewer nations have a strategy in place versus do not (38/167)



ggplot(data, aes(x = pimh)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: tail to the right

ggplot(data, aes(x=pimh)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

outlierValues <- boxplot(data$pimh, plot=FALSE)$out
min(outlierValues)

# COMMENT: 
#  16 outliers: Argentina
#              Belgium
#              Denmark
#              Finland
#              France
#              Iceland
#              Ireland
#              Lithuania
#              Luxembourg
#              Netherlands
#              New Zealand
#              Norway
#              Poland
#              Sweden
#              Switzerland
#              UK


ggplot(data, aes(x = mh)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
# COMMENT: tail to the right, very many countries with very few zero or few hospitals

ggplot(data, aes(x=mh)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

outlierValues <- boxplot(data$mh, plot=FALSE)$out
min(outlierValues)
# COMMENT: 
#  16 outliers: Argentina
#              Armenia
#              Barbados
#              Belgium
#              Colombia
#              France
#              Germany
#              Ireland
#              Japan
#              Malta
#              Norway
#              Republic of Korea
#              Saint Lucia
#              Saint Vincent and the Grenadines
#              Samoa
#              Switzerland




## plots to understand possible collinearity
splom(data[, numeric_cols], pscales = 0, varname.cex = .8)
print(corrplot(corr_data, method = 'color')) # can only use numeric columns
corrgram(data, 
         lower.panel = panel.shade, 
         upper.panel = panel.pie, 
         text.panel = panel.txt) # correlogram


######################################################################################### 
# I. 
#  A. PLOT Y1 AGAINST INDIVIDUAL PREDICTORS
#       is there a linear relationship?
#    1.




# plot(data$exp, data$suicide_mf)
# fittedModel <- lm(data$suicide_mf ~ data$exp)
# summary(fittedModel) # Adjusted R-squared:  0.0004953
# abline(fittedModel)
# # COMMENT:
# # does not appear to be a correlation, Adjusted R-squared:  0.0004953
# 
# plot(data$lpr, data$suicide_mf)
# fittedModel <- lm(data$suicide_mf ~ data$lpr)
# summary(fittedModel)
# abline(fittedModel)
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.1026
# 
# plot(data$gdp, data$suicide_mf)
# fittedModel <- lm(data$suicide_mf ~ data$gdp)
# summary(fittedModel)
# abline(fittedModel)
# # COMMENT: no correlation, Adjusted R-squared:  -0.002265
# 
# plot(data$la, data$suicide_mf)
# fittedModel <- lm(data$suicide_mf ~ data$la)
# summary(fittedModel)
# abline(fittedModel)
# # COMMENT: moderate postive correlation, Adjusted R-squared:  0.1659
# 
# plot(data$strat, data$suicide_mf)
# fittedModel <- lm(data$suicide_mf ~ data$strat)
# summary(fittedModel)
# abline(fittedModel)
# # TODO: how to measure correlation on categorical data?
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.02594
# 
# plot(data$psyc, data$suicide_mf)
# fittedModel <- lm(data$suicide_mf ~ data$psyc)
# summary(fittedModel)
# abline(fittedModel)
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.01425
# 
# plot(data$hosp, data$suicide_mf)
# fittedModel <- lm(data$suicide_mf ~ data$hosp)
# summary(fittedModel)
# abline(fittedModel)
# # COMMENT: no correlation, Adjusted R-squared:  -0.002868

######################################################################################### 
#  B. PLOT Y2 AGAINST INDIVIDUAL PREDICTORS
#       is there a linear relationship?
#    1.
# plot(data$exp, data$suicide_m)
# fittedModel <- lm(data$suicide_m ~ data$exp)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: does not appear to be a correlation, Adjusted R-squared:  0.0002175
# 
# plot(data$lpr, data$suicide_m)
# fittedModel <- lm(data$suicide_m ~ data$lpr)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.08769
# 
# plot(data$gdp, data$suicide_m)
# fittedModel <- lm(data$suicide_m ~ data$gdp)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: no correlation, Adjusted R-squared:  -0.005379
# 
# plot(data$la, data$suicide_m)
# fittedModel <- lm(data$suicide_m ~ data$la)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: moderate postive correlation, Adjusted R-squared:  0.2169
# 
# plot(data$strat, data$suicide_m)
# fittedModel <- lm(data$suicide_m ~ data$strat)
# abline(fittedModel)
# summary(fittedModel)
# # TODO: how to measure correlation on categorical data?
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.03915
# 
# plot(data$psyc, data$suicide_m)
# fittedModel <- lm(data$suicide_m ~ data$psyc)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.03205
# 
# plot(data$hosp, data$suicide_m)
# fittedModel <- lm(data$suicide_m ~ data$hosp)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: no correlation, Adjusted R-squared:  -0.002494

##############################################################################
#  C. PLOT Y3 AGAINST INDIVIDUAL PREDICTORS
#       is there a linear relationship?
#    1.
# plot(data$exp, data$suicide_f)
# fittedModel <- lm(data$suicide_f ~ data$exp)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: does not appear to be a correlation, Adjusted R-squared:  -0.002016
# 
# plot(data$lpr, data$suicide_f)
# fittedModel <- lm(data$suicide_f ~ data$lpr)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.07664
# 
# plot(data$gdp, data$suicide_f)
# fittedModel <- lm(data$suicide_f ~ data$gdp)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: weak correlation, Adjusted R-squared:  0.0229
# 
# plot(data$la, data$suicide_f)
# fittedModel <- lm(data$suicide_f ~ data$la)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: weak postive correlation, Adjusted R-squared:  0.01969
# # dramatically less than for males
# 
# plot(data$strat, data$suicide_f)
# fittedModel <- lm(data$suicide_f ~ data$strat)
# abline(fittedModel)
# summary(fittedModel)
# # TODO: how to measure correlation on categorical data?
# # COMMENT: no correlation, Adjusted R-squared:  -0.004776
# 
# plot(data$psyc, data$suicide_f)
# fittedModel <- lm(data$suicide_f ~ data$psyc)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: no correlation, Adjusted R-squared:  -0.005364
# 
# plot(data$hosp, data$suicide_f)
# fittedModel <- lm(data$suicide_f ~ data$hosp)
# abline(fittedModel)
# summary(fittedModel)
# # COMMENT: no correlation, Adjusted R-squared:  -0.005688

##############################################################################
##############################################################################
# D. VARIABLE TRANSFORMATIONS
#   1. 
# TODO: for non-linear relationships, can box-cox transformation make a linear relationship?
# TODO: plot residuals against predictors?
plot(data$exp, data$suicide_mf)
fittedModel <- lm(data$suicide_mf ~ data$exp)
summary(fittedModel) # Adjusted R-squared:  0.0004953
abline(fittedModel)
# COMMENT: 
# does not appear to be a correlation, Adjusted R-squared:  0.0004953
# Can the Y be transformed?
bcTransformation <- boxcox(fittedModel, lambda = seq(0.1, 0.6, 0.05))
# The 95% confidence interval for lambda is about [.15, 0.55]
bcTransformation$x[which.max(bcTransformation$y)] # 0.3474747
yTransformed <- data$suicide_mf^0.35
fittedModel2 <- lm(yTransformed ~ data$exp)
plot(data$exp, yTransformed)
abline(fittedModel2)
summary(fittedModel2) # Adjusted R-squared:  0.007479
plot(data$exp, resid(fittedModel2))
plot(fittedModel2)
# COMMENT: 
#  Transforming Y didn't seem to help
#  Plot residuals against fitted values
#  Plot the normal probability plot (qqnorm) for the residuals
par(mfcol = c(1,2))
plot(fitted(fittedModel), resid(fittedModel))
abline(0, 0)
plot(fitted(fittedModel2), resid(fittedModel2)) # Appears to be 3 outliers, 2 x and 1 y
abline(0, 0)
qqnorm(residuals(fittedModel))
qqline(residuals(fittedModel))
qqnorm(residuals(fittedModel2))
qqline(residuals(fittedModel2))

summary(update(fittedModel, .~ . +I (data$exp^2)))
# COMMENT: Transforming X into X^2 didn't improve adusted R-squared



par(mfcol = c(1,1))


#    1. 1st order linear regression models: 
#      Y1 = B0 + B1(X1) + B2(X2) + B3(X3) + B4(X4) + B5(X5) + B6(X6) + B7(X7)
mfSuicideModel <- lm(suicide_mf ~ exp + lpr + 
                        gdp + la + strat + 
                        psyc + hosp, data = data)
summary(mfSuicideModel)
plot(fitted(mfSuicideModel), resid(mfSuicideModel))
abline(0, 0)
# COMMENT: 
#    according to mfSuicideMOdel, these appear least significant:
#    exp, psyc, hosp

mSuicideModel <- lm(suicide_m ~ exp + lpr + 
                       gdp + la + strat + 
                       psyc + hosp, data = data)
summary(mSuicideModel)
plot(fitted(mSuicideModel), resid(mSuicideModel))
abline(0, 0)
# COMMENT: 
#    according to mSuicideMOdel, these appear least significant:
#    lpr, psyc, hosp

fSuicideModel <- lm(suicide_f ~ exp + lpr + 
                      gdp + la + strat + 
                      psyc + hosp, data = data)
summary(fSuicideModel)
plot(fitted(fSuicideModel), resid(fSuicideModel))
abline(0, 0)
# COMMENT: 
#    according to fSuicideMOdel, these appear least significant:
#    exp, la, strat, psyc, hosp



######################################################################################
# 2. 
#   Modeling with interaction terms
#   Add another predictor using the healthcare-related predictors in an interaction term, X1 and X4
#interactionTermX1X4 <- data$X1 * data$X4
#mfSuicideModel2 <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X6 + interactionTermX1X4, data = data)
#summary(mfSuicideModel2)


# TODO: Plots of residuals vs X's, and Y's vs X's

# COMMENT: 
#   seems to suggest dropping X1, X2, X3, X4 due to high P value
#   and low adjusted R-squared

#mfSuicideModel3 <- lm(Y1 ~ X5 + X6 + interactionTermX1X4, data = data)
#summary(mfSuicideModel3)
###########################################################################################

#  3. Modeling with 2nd order terms
# Form a matrix containing all predictor columns and no Y columns
allX <- as.matrix(data[, 4:10])
# Use poly() to form all 2-way interactions and 2nd order terms
secondOrderX <- poly(allX, degree = 2, raw = TRUE)
# save as data frame including Y1
secondOrderDataMF <- as.data.frame(cbind(secondOrderX, y = data$suicide_mf))
# fit the 2nd order model
secondOrderSuicideModelMF <- lm(y ~ ., data = secondOrderDataMF)
summary(secondOrderSuicideModelMF)
plot(fitted(secondOrderSuicideModelMF), resid(secondOrderSuicideModelMF))
abline(0, 0)
# COMMENT: 2nd order model yields Ajusted R-squared of 0.2695
#   Look for other ways to improve the model

secondOrderDataM <- as.data.frame(cbind(secondOrderX, y = data$suicide_m))
# fit the 2nd order model
secondOrderSuicideModelM <- lm(y ~ ., data = secondOrderDataM)
summary(secondOrderSuicideModelM)
plot(fitted(secondOrderSuicideModelM), resid(secondOrderSuicideModelM))
abline(0, 0)
# COMMENT: 2nd order model yields Ajusted R-squared of 0.3196
#   Look for other ways to improve the model

secondOrderDataF <- as.data.frame(cbind(secondOrderX, y = data$suicide_f))
# fit the 2nd order model
secondOrderSuicideModelF <- lm(y ~ ., data = secondOrderDataF)
summary(secondOrderSuicideModelF)
plot(fitted(secondOrderSuicideModelF), resid(secondOrderSuicideModelF))
abline(0, 0)
# COMMENT: 2nd order model yields Ajusted R-squared of 0.1287
#   Look for other ways to improve the model

# Plot alcohol consumption against the suicide rate
lam <- lm(smf ~ la, data = data)
plot(data$smf ~ data$la, 
     pch = 15, 
     col = rgb(0, 0, 1, 0.5), 
     xlab = "Liters of pure alcohol consumed per capita", 
     ylab = "Suicide rate per capita")

# highlight Russia
with(data, 
     text(data[127, 1] ~ data[127, 7], 
          labels = paste(strwrap(data[127, 11],
                                 width = 8), collapse = "\n"), 
          pos = 3) )

