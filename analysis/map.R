#!/usr/local/bin/R
# Rscript analysis/peter.R
setwd('~/Projects/regression-mei-team-proj')

# World Heatmap
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

mp1 + coord_sf(crs = "+proj=merc", 
               ylim = c(-8000000, 18000000)) 

ggsave('map.pdf')


