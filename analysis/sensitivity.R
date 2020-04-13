#!/usr/local/bin/R
# Run the following to create images: 
# Rscript analysis/sensitivity.R

# cd ~/Projects/regression-mei-team-proj
# setwd('~/Projects/regression-mei-team-proj')
#########################
# author: @pewilliams

#################
#helpers
require(data.table)
require(MASS)
require(ggplot2)
options(scipen = 100)

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

build_seq <- function(x,nlen){
	return(seq(min(x), max(x), length.out = nlen))
}

lambda <- 0.4 #chosen already

#fm_labor 	#X2 = female_male_labor_participation 				
#gdp 		#X3 = gdp/capita ppp 								
#lalc 		#X4 = litres_alcohol_per_capita 					
#sstrat		#X5 = Suicide_prevention_strategy, binary 			


#### Import model and grab original data 
#### from model object
#final model object
fmod <- readRDS('~/Projects/regression-mei-team-proj/analysis/final_mod.rds')

#data.frame associated with model
dat <- fmod$model
dat <- data.table(dat)
setnames(dat, names(dat), c('srate','fm_labor',
	'gdp','lalc','sstrat', 'wtg'))
fmod <- lm(srate ~ fm_labor + gdp + lalc + sstrat, 
	weights = wtg, data = dat)

#prediction data.frame and sensitivity by variable

#X2: Labor Participation (fm_labor)
labor_dat <- data.frame(
	fm_labor = build_seq(dat$fm_labor, 50),
	gdp = mean(dat$gdp),
	lalc = mean(dat$lalc),
	sstrat = 1)

pred <- data.frame(bc_inv(0.4, predict(fmod, 
	newdata = labor_dat, 
	interval = 'confidence')))

labor_dat <- cbind(labor_dat, pred)

lplot <- ggplot(labor_dat, 
	aes(x = fm_labor, 
	y = fit)) + 
	geom_line() + 
		xlab('Labor Participation Rate') +
		ylab('Predicted Suicide Rate') +
	geom_line(aes(x = fm_labor, y = upr, 
		colour = 'red')) + 
	geom_line(aes(x = fm_labor, y = lwr, 
		colour = 'red')) + 
	theme_bw() + theme(legend.position = "none")

ggsave('images/labor_plot.pdf', plot = lplot)

#X3: GDP 
gdp_dat <- data.frame(
	fm_labor = mean(dat$fm_labor),
	gdp = build_seq(dat$gdp, 50),
	lalc = mean(dat$lalc),
	sstrat = 1)

pred <- data.frame(bc_inv(0.4, predict(fmod, 
	newdata = gdp_dat, 
	interval = 'confidence')))

gdp_dat <- cbind(gdp_dat, pred)

gplot <- ggplot(gdp_dat, 
	aes(x = exp(gdp), 
	y = fit)) + 
	geom_line() + 
		xlab('Income: Per Person GDP') +
		ylab('Expected Suicide Rate: per 100k Population (Annual)') +
	geom_line(aes(x = exp(gdp), y = upr, 
		colour = 'red')) + 
	geom_line(aes(x = exp(gdp), y = lwr, 
		colour = 'red')) + 
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
	theme_bw() + theme(legend.position = "none")

#elasticity
gdp_dat$gdp_exp <- exp(gdp_dat$gdp)
write.csv(gdp_dat, '~/Desktop/gdp.csv', row.names = F)



ggsave('images/gdp_plot.pdf', plot = gplot)

#X4: Litres Alcohol 
alc_dat <- data.frame(
	fm_labor = mean(dat$fm_labor),
	gdp = mean(dat$gdp, 50),
	lalc = build_seq(dat$lalc, 50),
	sstrat = 1)

pred <- data.frame(bc_inv(0.4, predict(fmod, 
	newdata = alc_dat, 
	interval = 'confidence')))

alc_dat <- cbind(alc_dat, pred)

aplot <- ggplot(alc_dat, 
	aes(x = lalc, 
	y = fit)) + 
	geom_line() + 
		xlab('Liters of Alcohol Consumed Per Year') +
		ylab('Expected Suicide Rate: per 100k Population (Annual)') +
	geom_line(aes(x = lalc, y = upr, 
		colour = 'red')) + 
	geom_line(aes(x = lalc, y = lwr, 
		colour = 'red')) + 
	theme_bw() + theme(legend.position = "none")

write.csv(alc_dat, '~/Desktop/alc.csv', row.names = F)

ggsave('images/alc_plot.pdf', plot = aplot)

#X5: Suicide Strategy 
ss_dat <- data.frame(
	fm_labor = mean(dat$fm_labor),
	gdp = mean(dat$gdp, 50),
	lalc = mean(dat$lalc),
	sstrat = c(1,0))

pred <- data.frame(bc_inv(0.4, predict(fmod, 
	newdata = ss_dat, 
	interval = 'confidence')))

ss_dat <- cbind(ss_dat, pred)
ss_dat$ind <- ifelse(ss_dat$sstrat == 1, 'Yes', 'No')

splot <- ggplot(ss_dat, 
	aes(y = fit, ymin = lwr, 
	ymax = upr, x = factor(ind))) +
	geom_crossbar(width = 0.2, fill = 'gray') +
		xlab('National Suicide Prevention Strategy (Presence)') +
		ylab('Expected Suicide Rate: per 100k Population (Annual)')  + 
	theme_bw() + theme(legend.position = "none")

ggsave('images/ss_plot.pdf', plot = splot)
