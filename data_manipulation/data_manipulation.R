data <- read.table(file = "C:/Users/oagha/Desktop/isye6414/regression-mei-team-proj/data_manipulation/alldata_nolabels.csv", sep=",")
colnames(data) <- c("Y1", "Y2", "Y3","X1","X2","X3", "X4","X5","X6", "X7") 

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
