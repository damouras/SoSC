library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)

rm(list=ls())
# Key for province codes  http://www.statcan.gc.ca/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm
(PROVOFSTUDY_levels=c("10", "12", "13", "24", "35", "46", "47", "48", "59" ))
(PROVOFSTUDY_labels=c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ))
(REGION_levels=c("Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))

enrol = read_delim("./Data/PSIS/psisfreq_enrolment_2.csv",delim = ",", col_names=TRUE)
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
enrol= enrol %>% mutate(PROGRAM = factor(PRGCODE, levels=c("26.1102","27.0501","27.0502","27.0599","27.9999","52.1302"), 
                                         labels=c("Biostatistics",
                                                  "Statistics, General",
                                                  "Mathematical Statistics and Probability",
                                                  "Statistics, Other",
                                                  "Mathematics and Statistics, Other",
                                                  "Business Statistics") ))

# 26.1102	Biostatistics
# 27.0501	Statistics, General
# 27.0502	Mathematical Statistics and Probability
# 27.0599	Statistics, Other
# 27.9999	Mathematics and Statistics, Other
# 52.1302	Business Statistics

enrol %>% group_by(YEAR,PROGRAM,REGION) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=YEAR,y=ENROLMENTS,colour=PROGRAM)) + geom_line(size=1.2) + geom_point(size=2) + 
  facet_grid(REGION~.)

enrol %>% filter(PROGRAM == "Mathematical Statistics and Probability") %>% 
  group_by(YEAR,LEVEL,REGION) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>% complete(REGION) %>%
  ggplot(aes(x=YEAR,y=ENROLMENTS,colour=LEVEL)) + geom_line(size=1.2) + geom_point(size=2) + 
  facet_grid(REGION~.)

enrol %>% filter(PROGRAM=='Biostatistics') %>% group_by(YEAR,GENDER) %>% summarise(ENROLMENTS=sum(ENROLMENTS)) %>% 
  ggplot(aes(x=YEAR,y=ENROLMENTS,colour=GENDER)) + geom_line(size=1.5) + geom_point(size=3) 
