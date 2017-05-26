library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

rm(list=ls())
enrol = read_delim("./Data/PSIS/RTRA_PSIS_enrollments.csv",delim = ",", col_names=TRUE)
names(enrol)=str_replace_all(names(enrol),"PSIS_","")
names(enrol)=str_replace_all(names(enrol),"_CODE","")
names(enrol)=str_replace_all(names(enrol),"_COUNT_","COUNT")
enrol= enrol %>% mutate(YEAR = as.Date(apply( cbind(1, 1, enrol$REFYEAR_D), 1, paste, collapse="/"),"%d/%m/%Y"))
enrol= enrol %>% mutate(GENDER = factor(GENDER, levels=c("1","2"), labels=c("M","F") ))
enrol= enrol %>% mutate(ENROLMENTS = COUNT) %>% select(-COUNT)


enrol %>% filter( !is.na(REFYEAR_D), is.na(PROVOFSTUDY), is.na(GENDER), is.na(PCSCE_D), PRGCODECIP6_D==27.0501 ) %>%
  ggplot(aes(x=YEAR,y=ENROLMENTS)) + geom_line(size=1.5) + geom_point(size=3) + scale_y_continuous(limits=c(0,2500)) 

enrol %>% filter( !is.na(REFYEAR_D), is.na(PROVOFSTUDY), !is.na(GENDER), is.na(PCSCE_D), PRGCODECIP6_D==27.0501 ) %>%
  ggplot(aes(x=YEAR,y=ENROLMENTS, colour=GENDER)) + geom_line(size=1.5) + geom_point(size=3) + 
  scale_y_continuous(limits=c(0,1250)) 

  
grad = read_delim("./Data/PSIS/RTRA_PSIS_graduates.csv",delim = ",", col_names=TRUE)
names(grad)=str_replace_all(names(grad),"PSIS_","")
names(grad)=str_replace_all(names(grad),"_CODE","")
names(grad)=str_replace_all(names(grad),"_COUNT_","COUNT")
grad= grad %>% mutate(YEAR = as.Date(apply( cbind(1, 1, grad$REFYEAR_D), 1, paste, collapse="/"),"%d/%m/%Y"))
grad= grad %>% mutate(GENDER = factor(GENDER, levels=c("1","2"), labels=c("M","F") ))
grad= grad %>% mutate(GRADUATES = COUNT) %>% select(-COUNT)


grad %>% filter( !is.na(REFYEAR_D), is.na(PROVOFSTUDY), is.na(GENDER), is.na(PCSCE_D), PRGCODECIP6_D==27.0501 ) %>%
  ggplot(aes(x=YEAR,y=GRADUATES)) + geom_line(size=1.5) + geom_point(size=3) + scale_y_continuous(limits=c(0,600)) 

grad %>% filter( !is.na(REFYEAR_D), is.na(PROVOFSTUDY), !is.na(GENDER), is.na(PCSCE_D), PRGCODECIP6_D==27.0501 ) %>%
  ggplot(aes(x=YEAR,y=GRADUATES, colour=GENDER)) + geom_line(size=1.5) + geom_point(size=3) + 
  scale_y_continuous(limits=c(0,300)) 

