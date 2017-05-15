library(readr)
library(stringr)
library(dplyr)

setwd("~/GitHub/SoSE")
rm(list=ls())

#### Enrolments ####
enrol=read_csv("./Data/04770019-eng.csv", na='..')
# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770019 
enrol=mutate(enrol, Ref_Date = as.numeric(str_extract( enrol[['Ref_Date']], "^[:digit:]+")) )
# glimpse(enrol)

#### Graduates ####
grad=read_csv("./Data/04770020-eng.csv", na='..')
# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770020
# glimpse(grad)

#### UofT ####
#Data from http://cudo.utoronto.ca/
#Tri-campus data collected by data 
cudo_UT=read_csv("./Data/CUDO UofT.csv", na='..')

#### Programs ####
lop = read_delim("./Data/Stats Program Data - List of Programs.csv", col_names = TRUE, delim=',')
progs=list()
#for(i in 1:length(lop$ShortName)){
for(i in 1:3){
  fname=paste("./Data/Stats Program Data - ", lop$ShortName[i], ".csv", sep="")
  progs[[i]]= read_delim(fname, col_names = TRUE, delim=',')
  progs[[i]]= mutate(progs[[i]], Univ=lop$ShortName[i])
}
aprogs=do.call(bind_rows,progs[1:3]) # all programs

