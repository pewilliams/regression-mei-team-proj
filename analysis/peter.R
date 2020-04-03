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

#add violent against women - optional
#https://data.oecd.org/inequality/violence-against-women.htm
# vw <- fread('data_manipulation/violence_against_women_index.csv',
# 		header=F,sep = ',')
# dat$vw_index <- match(dat$clab,vw$V1)
# dat <- dat[!is.na(vw_index)]

#EDA

#outcome - positive skew - transformation needed
ggplot(dat, aes(x = srate)) + geom_density()
ggplot(dat, aes(x = srate, group = 1)) + geom_jitter()


#outliers
boxplot(dat$srate)
dat <- dat[!(dat$srate > 25)] #removed the following: 
#######Guyana
#NPR Story on Guyana Here: https://www.npr.org/sections/goatsandsoda/2018/06/29/622615518/trying-to-stop-suicide-guyana-aims-to-bring-down-its-high-rate
#Factors: Lack of mental health facilities
#Agricultural Work, not advanced tech to support
#Women Stay at Home - labor participation
#Alcohol a big contributor (rum shops)
#Domestic Violence
#Lesotho, Economics, domestic violence, substance abuse
#Lithuania, economic crisis: https://en.wikipedia.org/wiki/Suicide_in_Lithuania
#Russia, Heavy Drinking: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1642767/
#suicide rate over 25 - classic boxplot outliers



#scatter plot all - total rate
#be patient while plot loads

ggpairs(dat[,c("srate",
	"h_pct_gdp","fm_labor","gdp",
		"lalc","sstrat","psych", "hosp","vw_index")])

# baseline model
lm1 <- lm(srate~h_pct_gdp+
	fm_labor+gdp+
	lalc+sstrat+
	psych+hosp,
	data = dat)
summary(lm1)

####################### BOXCOX
#boxcox transformation - full model
bc <- boxcox(lm1, 
	lambda = seq(-2, 2, 1/10), plotit = TRUE,
	xlab = expression(lambda),
       ylab = "log-Likelihood")

best_lambda <- bc$x[which.max(bc$y)]

bc_transform <- function(lambda,y){
	if(lambda == 0){
		log(y)	
	}else{
		(y^lambda - 1)/lambda
	}	
}

bc_inv <- function(lambda,x){
	if(lambda == 0){
		exp(x)		
	}else{
		(lambda*x + 1)^(1/lambda)
	}
}

dat$bc_srate <- bc_transform(lambda = best_lambda, 
	y = dat$srate)

############
# Model 2

dat <- dat[hosp < 1.5] #remove outliers
#interaction plot: 
ggplot(dat, aes(y = bc_srate, x = log10(hosp+1))) + 
	geom_point() + geom_smooth(method = 'lm') + 
	facet_wrap(~sstrat)

lm2 <- lm(bc_srate~
	fm_labor+gdp+
	lalc +sstrat + log1p(hosp) + sstrat * log1p(hosp), #outlier driven
	data = dat)
summary(lm2)

#check it out


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

newdat$pred_srate <- predict(lm2, 
	newdata = newdat)

ggplot(newdat, aes(x = lalc, 
	y = bc_inv(lambda = best_lambda,pred_srate))) + 
	geom_line() + 
		xlab('Liters of Alcohol Consumer PP') +
		ylab('Predicted Suicide Rate')





