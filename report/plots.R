#!/usr/local/bin/R
# Rscript analysis/peter.R
# Generate plots for report
#setwd('~/Projects/regression-mei-team-proj')

# Plot Objects
# 'world_map' - suicide rate by country
# 'africa_map' - suicide rate in africa
# 'russia_map' - suicide rate in russia
# 'gdp_plot' - gdp sens plot
# 'labor_plot' - female male labor participation
# 'ss_plot' - suicide rate in africa
# 'russia_map' - suicide rate in russia
#########################
# author: @pewilliams

#depends
library(data.table)
library(ggplot2)
library(MASS)
library(GGally)
options(scipen = 100)
#load data
dat <- fread(file = "../data_manipulation/alldata_nolabels.csv", 
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
c_labels <- fread('../data_manipulation/country_labels.csv',
                  header=F,sep = ',')
dat <- transform(dat,
                 clab = c_labels$V1)
############### World Heatmap
# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "rworldmap",
#                    "ggspatial", "libwgeom", "sf", 
#                    "rnaturalearth", "rnaturalearthdata","rgeos"))

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

world <- ne_countries( returnclass = "sf")
class(world)

#joint df of country labels and missing names
jdf <- data.frame(clab = dat$clab, 
                  name = world$name[match(dat$clab,world$name)], 
                  stringsAsFactors = F)

#fix names to ISO standard - need to add more
jdf$name[161] <- 'United States'
jdf$name[157] <- 'United Kingdom'
jdf$name[164] <- 'Vietnam'
jdf$name[138] <- 'Solomon Is.'
# jdf$name[135] <- '' #singapore
# jdf$name[131] <- '' #samoa
# jdf$name[130] <- '' #st vincent
# jdf$name[129] <- '' #Saint Lucia
jdf$name[127] <- 'Russia' #russia
#jdf$name[151] <- 'Vietnam' #Tonga - can't find

##map https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

dat$name <- jdf$name[match(dat$clab, jdf$clab)]
world$srate <- dat$srate[match(world$name, dat$name)]

mp1 <- ggplot(data = world) +
  geom_sf(aes(fill = srate)) +
  theme_bw() + labs(fill = "Rate") 

####WORLD MAP
world_map <- mp1 + coord_sf(crs = "+proj=merc", 
               ylim = c(-8000000, 18000000)) 




#africa
zmap <- ggplot(data = world) +
  geom_sf(aes(fill = srate)) +
  theme_bw() + labs(fill = "Rate") 

africa_map <- zmap + coord_sf(crs = "+proj=merc", 
                        ylim = c(-3700000, 6000000), xlim = c(-3000000, 6000000)) 



#russia
zmap <- ggplot(data = world) +
  geom_sf(aes(fill = srate)) +
  theme_bw() + labs(fill = "Rate") 

russia_map <- zmap + coord_sf(crs = "+proj=merc", 
                        ylim = c(-2000000, 14000000), xlim = c(-2000000, 22000000)) 



#################
#helpers


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
fmod <- readRDS('../analysis/final_mod.rds')

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

labor_plot <- ggplot(labor_dat, 
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

gdp_plot <- ggplot(gdp_dat, 
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

alc_plot <- ggplot(alc_dat, 
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

ss_plot <- ggplot(ss_dat, 
                aes(y = fit, ymin = lwr, 
                    ymax = upr, x = factor(ind))) +
  geom_crossbar(width = 0.2, fill = 'gray') +
  xlab('National Suicide Prevention Strategy (Presence)') +
  ylab('Expected Suicide Rate: per 100k Population (Annual)')  + 
  theme_bw() + theme(legend.position = "none")


