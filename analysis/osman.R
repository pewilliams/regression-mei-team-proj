library(MASS)
library(faraway)

data1 <- read.table(file = "C:/Users/oagha/Desktop/isye6414/regression-mei-team-proj/data_manipulation/alldata_nolabels.csv", sep=",")
colnames(data1) <- c("Y1", "Y2", "Y3","X1","X2","X3", "X4","X5","X6", "X7") 

#Y1 = suicide_male_and_female
#Y2 = suicide_male
#Y3 = suicide_female
#X1 = Current_health_expenditure_percentage_of_GDP
#X2 = female_male_labor_participation
#X3 = gdp/capita ppp
#X4 = litres_alcohol_per_capita
#X5 = Suicide_prevention_strategy, binary
#X6 = Psychiatrists working in mental health sector (per 100 000 population)
#X7 = Mental hospitals (per 100 000 population)

#original data, multiple linear regression
#fm1 <-lm(Y1 ~ X1 + X3 + X4 + X5 + X6 + X7, data = data1)

#however, more appropriate starting point based on intuitions of what is 'good' for 
#society and how it aligns with increased suicide:
fm1 <-lm(Y1 ~ (X1/-1) + (X3/-1) + X4 + (X5/-1) + (X6/-1) + (X7/-1), data = data1)

print(summary(fm1))
#Observations: 
#Adjusted R-squared:  0.7582
#X7 and X3 have very high P value, X5 has a moderately high p value

plot(fm1)
#plot 1: As fitted values increase, there is a slight decrease in the residuals. 
#Our model slightly underestimates suicide rates when they are low, but slightly overestimates suicide rates when they are high.
#good indication that no obvious non linear relationships exist between the x variables and the y variable

#plot 2: residuals do not deviate severely, another good indication

#plot 3: residuals are spread evenly across predictor range until the right tail, 
# where the residuals are relatively higher

#plot 4: Row 79 (Japan) is very influential in our model.
#this is very interesting, as Japan is notorious for having a generally 
#depressed and overworked population, along with high socioeconomic indicators

#might consider removing japan from data set? 
#data_no_japan <-data1[-c(79),]




#due to high p values, let us consider removing X7,X5, and X3
#use anova to compare models with and without X7, X5, and X3 

fm2 <-lm(Y1 ~ (X1/-1) + X4 + (X6/-1), data = data1)

print(summary(fm2))

print(anova(fm2, fm1))
#anova results show a non-significant result (p = 0.3974).
#Thus, we should reject the model that includes both X7, X5, and X3


#Consider that our model has an adjusted R^2 of .7583, despite all of the p values
#being within .01 level of significance

#consider a transformation of y:
#use the square root of y instead of y

fm3 <-lm(Y1^.5 ~ (X1/-1) + X4 + (X6/-1), data = data1)

print(summary(fm3))

#After transformation, X variables are within .001 level of significance, and 
#adjusted R-squared is 0.864. 
#we need a way to justify coming up with this transformation???











#scratch work below, can be ignored
#fm2 <-lm((Y1)^.5 ~  log(X1) + ((X3^.5)/-1) + X4 + X5 + (X6/-1) + X7, data = data1)


#boxcox(fm3, lambda = seq(0, 0.75, by = 0.05), plotit = TRUE)

#plot(fm3)
#fm3 <-lm(((Y1^.4-1)/.4) ~  X1a + X3a + X4 + X5 + X6 + X7, data = data1)
#print(summary(fm3))

#fm1 <-glm(Y1 ~ X1 + X3 + X4 + X5 + X6 + X7, data = data1,family = poisson())
