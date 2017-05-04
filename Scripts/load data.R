library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

setwd("~/GitHub/SoSE")

#### Enrolments ####
enrol=read_csv("./Data/04770019-eng.csv", na='..')
# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770019 
enrol=mutate(enrol, Ref_Date = as.numeric(str_extract( enrol[['Ref_Date']], "^[:digit:]+")) )
glimpse(enrol)

enrol %>% filter( Coordinate=="1.2.1.13.9.1.1") %>% select(Ref_Date, Value) %>% ggplot( aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1) 
 
#### Graduates ####
grad=read_csv("./Data/04770020-eng.csv", na='..')
# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770020
glimpse(grad)

grad %>% filter( Coordinate=="1.2.13.9.1.1") %>% select(Ref_Date, Value) %>% ggplot( aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1)


#### UofT ####
#Data from http://cudo.utoronto.ca/
#Tri-campus data collected by data 
cudo_UT=read_csv("./Data/CUDO UofT.csv", na='..')

cudo_UT %>% filter( FoS=="CS") %>% select(Year, Enroll) %>% ggplot( aes(Year,Enroll)) + geom_point(size=3) + geom_line(lwd=1)



