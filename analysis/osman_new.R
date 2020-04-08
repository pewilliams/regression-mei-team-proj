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





fm1 <-lm(Y1 ~ X1 + X3 + X4 + X5 + X6 + X7, data = data1)
#Normal Q Q plot -- residuals do not seem to be off in the higher quantiles especially
#deal with this using a box cox transformation

#box cox transformation  use lambda of .4
fm2<-lm(((Y1^.4-1)/.4)~ X1 + X3 + X4 + X5 + X6 + X7, data = data1)
#plot(fm2)

#Remove outliers: 79 due to high leverage, and 12 due to normal Q Q plot, 88 and 12 due to scale location and residuals vs fitted
new_data <- data1[-c(12, 79, 88), ] 

#12 is Barbados
#79 is Japan
#89 is Lesotho

fm3<-lm(((Y1^.4-1)/.4)~ X1 + X3 + X4 + X5 + X6 + X7, data = new_data)
#print(summary(fm3))

#X1, X6, and X7 have P values > .05 level of significance, consider dropping, use anova test
fm4<-lm(((Y1^.4-1)/.4)~  X3 + X4 + X5, data = new_data)
#print(anova(fm4, fm3))

#p value from anova test is 0.1601.
#Thus, we should reject the model that includes X1, X6, and X7

#final model is fm4


#weighted least squares
residuals_data<-resid(fm4)
fm_resid<-lm(abs(residuals_data) ~ X3 + X4 +X5, data = new_data)

s_hat<-predict(fm_resid, new_data[,c("X3","X4","X5")])

w<-1/(s_hat^2)

weighted_fit<-lm(((Y1^.4-1)/.4) ~ X3 + X4 + X5, data = new_data, weights=w)

