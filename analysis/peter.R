#!/usr/local/bin/R
# Rscript analysis/peter.R
# cd ~/Projects/regression-mei-team-proj
# head -n 5 data_manipulation/alldata_nolabels.csv
#########################
# author: @pewilliams

#depends
library(data.table)
library(ggplot2)
library(MASS)
library(GGally)

#load data
dat <- fread(file = "data_manipulation/alldata_nolabels.csv", 
	sep=",", header=F)

###	read-able labels

#srate 		#Y1 = suicide_male_and_female 						
#sm 		#Y2 = suicide_male 									
#sf 		#Y3 = suicide_female 								
#h_pct_gdp 	#X1 = Current_health_expenditure_percentage_of_GDP 	
#fm_labor 	#X2 = female_male_labor_participation 				
#gdp 		#X3 = gdp/capita ppp 								
#lalc 		#X4 = litres_alcohol_per_capita 					
#sstrat		#X5 = Suicide_prevention_strategy, binary 			
#psych 		#X6 = Psychs work in m.health sect (per/100k/pop) 	
#hosp 		#X7 = Mental hospitals (per 100 000 population) 	

setnames(dat, c("srate", "sm", "sf",
	"h_pct_gdp","fm_labor","gdp",
		"lalc","sstrat","psych", "hosp"))

#add country labels for reference
c_labels <- fread('data_manipulation/country_labels.csv',
		header=F,sep = ',')
dat <- transform(dat,
	clab = c_labels$V1)

#pictures

#outcome - positive skew - transformation needed
# ggplot(dat, aes(x = srate)) + geom_density()

#scatter plot all - total rate
#be patient while plot loads

# ggpairs(dat[,c("srate",
# 	"h_pct_gdp","fm_labor","gdp",
# 		"lalc","sstrat","psych", "hosp")])

# baseline model
lm1 <- lm(log2(srate)~h_pct_gdp+
	fm_labor+gdp+
	log1p(lalc)+sstrat+
	psych+hosp,
	data = dat)
summary(lm1)

######
# sensitivity to liters of alcohol consumed
# with suicide prevention strategy in place
newdat <- data.frame(

	h_pct_gdp = mean(dat$h_pct_gdp),
	fm_labor = mean(dat$fm_labor),
	gdp = mean(dat$gdp),
	lalc = seq(0,20,by=0.5),
	sstrat = 1,
	psych = mean(dat$psych),
	hosp = mean(dat$hosp))

newdat$pred_srate <- predict(lm1, 
	newdata = newdat)

ggplot(newdat, aes(x = lalc, y = pred_srate)) + 
	geom_line() + 
		xlab('Liters of Alcohol Consumer PP') +
		ylab('Predicted Suicide Rate')





