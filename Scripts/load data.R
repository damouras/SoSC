library(tidyverse)
library(stringr)
library(ggplot2)

setwd("~/GitHub/SoSE")
rm(list=ls())

#### Enrolments ####

# Key for province codes  http://www.statcan.gc.ca/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm
(PROVOFSTUDY_levels=c("10", "12", "13", "24", "35", "46", "47", "48", "59" ))
(PROVOFSTUDY_labels=c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ))
(REGION_levels=c("Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))

enrol = read_delim("./Data/PSIS/RTRA_PSIS_enrollments.csv",delim = ",", col_names=TRUE)
enrol = enrol[apply( !apply(enrol[,1:5],2,is.na), 1, all), ]
names(enrol)=str_replace_all(names(enrol),"PSIS_","")
names(enrol)=str_replace_all(names(enrol),"_CODE","")
names(enrol)=str_replace_all(names(enrol),"_COUNT_","COUNT")
names(enrol)=str_replace_all(names(enrol),"CIP6_D","")
enrol= enrol %>% mutate(YEAR = as.Date(apply( cbind(1, 1, enrol$REFYEAR_D), 1, paste, collapse="/"),"%d/%m/%Y")) %>% select(-REFYEAR_D)
enrol= enrol %>% mutate(GENDER = factor(GENDER, levels=c("1","2"), labels=c("M","F") ))
enrol= enrol %>% mutate(ENROLMENTS = COUNT) %>% select(-COUNT)
enrol= enrol %>% mutate(LEVEL = factor(PCSCE_D, levels=c("66","76","86"), labels=c("BSc","MSc","PhD") )) %>% select(-PCSCE_D)
enrol= enrol %>% mutate(PROVINCE = factor(PROVOFSTUDY, levels=PROVOFSTUDY_levels, labels=PROVOFSTUDY_labels )) %>% select(-PROVOFSTUDY)
enrol= enrol %>% mutate(REGION = PROVINCE); levels(enrol$REGION)=REGION_levels

#### Graduates ####
grad = read_delim("./Data/PSIS/RTRA_PSIS_graduates.csv",delim = ",", col_names=TRUE)
grad = grad[apply( !apply(grad[,1:5],2,is.na), 1, all), ]
names(grad)=str_replace_all(names(grad),"PSIS_","")
names(grad)=str_replace_all(names(grad),"_CODE","")
names(grad)=str_replace_all(names(grad),"_COUNT_","COUNT")
names(grad)=str_replace_all(names(grad),"CIP6_D","")
grad= grad %>% mutate(YEAR = as.Date(apply( cbind(1, 1, grad$REFYEAR_D), 1, paste, collapse="/"),"%d/%m/%Y")) %>% select(-REFYEAR_D)
grad= grad %>% mutate(GENDER = factor(GENDER, levels=c("1","2"), labels=c("M","F") ))
grad= grad %>% mutate(GRADUATES = COUNT) %>% select(-COUNT)
grad= grad %>% mutate(LEVEL = factor(PCSCE_D, levels=c("66","76","86"), labels=c("BSc","MSc","PhD") )) %>% select(-PCSCE_D)
grad= grad %>% mutate(PROVINCE = factor(PROVOFSTUDY, levels=PROVOFSTUDY_levels, labels=PROVOFSTUDY_labels )) %>% select(-PROVOFSTUDY)
grad= grad %>% mutate(REGION = PROVINCE); levels(grad$REGION)=REGION_levels

# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770019
# enrol=read_csv("./Data/04770019-eng.csv", na='..')
# enrol=mutate(enrol, Ref_Date = as.numeric(str_extract( enrol[['Ref_Date']], "^[:digit:]+")) )
# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770020
# grad=read_csv("./Data/04770020-eng.csv", na='..')

#### UofT ####
#Data from http://cudo.utoronto.ca/
#Tri-campus data collected by data 
cudo_UT=read_csv("./Data/CUDO UofT.csv", na='..')

#### Programs ####
lop = read_delim("./Data/Stats Program Data - List of Programs.csv", col_names = TRUE, delim=',')
lop = lop %>% filter(`Honours/Specialist`=="Y")
progs=list()
#for(i in 1:length(lop$ShortName)){
for(i in 1:nrow(lop)){
  fname=paste("./Data/Stats Program Data - ", lop$ShortName[i], ".csv", sep="")
  progs[[i]]= read_delim(fname, col_names = TRUE, delim=',')
  progs[[i]]= mutate(progs[[i]], Univ=lop$ShortName[i])
}
aprogs=do.call(bind_rows,progs) # all programs
aprogs=mutate(aprogs, Category= str_replace_all(Category," ",""))
aprogs=mutate(aprogs, Category = str_split(Category, ",") )
aprogs=mutate(aprogs, Discipline= str_replace_all(Discipline," ",""))
aprogs=mutate(aprogs, Discipline = str_split(Discipline, ",") )
aprogs=mutate(aprogs, Level = str_replace_all(Level," ",""))
aprogs=mutate(aprogs, Level = str_split(Level,""))

