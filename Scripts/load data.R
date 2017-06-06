library(tidyverse)
library(stringr)
library(ggplot2)

rm(list=ls())

#### Enrolments ####

# Key for province codes  http://www.statcan.gc.ca/pub/92-195-x/2011001/geo/prov/tbl/tbl8-eng.htm
(PROVOFSTUDY_levels=c("10", "12", "13", "24", "35", "46", "47", "48", "59" ))
(PROVOFSTUDY_labels=c("NL", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC" ))
(REGION_levels=c("Atlantic", "Atlantic", "Atlantic", "QC", "ON", "Prairies", "Prairies", "Prairies", "BC" ))
# Key for Stats Programs
# 26.1102	Biostatistics
# 27.0501	Statistics, General
# 27.0502	Mathematical Statistics and Probability
# 27.0599	Statistics, Other
# 27.9999	Mathematics and Statistics, Other
# 52.1302	Business Statistics

enrol = read_delim("./Data/PSIS/psis_enrolments.csv",delim = ",", col_names=TRUE)
enrol = enrol[apply( !apply(enrol[,1:5],2,is.na), 1, all), ]
names(enrol)=str_replace_all(names(enrol),"PSIS_","")
names(enrol)=str_replace_all(names(enrol),"_CODE","")
names(enrol)=str_replace_all(names(enrol),"_COUNT_","COUNT")
names(enrol)=str_replace_all(names(enrol),"CIP6_D","")
enrol= enrol %>% mutate(YEAR = as.Date(apply( cbind(1, 1, enrol$REFYEAR_D), 1, paste, collapse="/"),"%d/%m/%Y")) %>% select(-REFYEAR_D)
enrol= enrol %>% mutate(SEX = factor(GENDER, levels=c("2","1"), labels=c("F","M") ))
enrol= enrol %>% mutate(ENROLMENTS = COUNT) %>% select(-COUNT)
enrol= enrol %>% mutate(LEVEL = factor(PCSCE_D, levels=c("66","76","86"), labels=c("BSc","MSc","PhD") )) %>% select(-PCSCE_D)
enrol= enrol %>% mutate(PROVINCE = factor(PROVOFSTUDY, levels=PROVOFSTUDY_levels, labels=PROVOFSTUDY_labels )) %>% select(-PROVOFSTUDY)
enrol= enrol %>% mutate(REGION = PROVINCE); levels(enrol$REGION)=REGION_levels
enrol= enrol %>% mutate(REGION = factor(REGION,levels(REGION)[c(1,5,3,4,2)] ) ); levels(enrol$REGION)
enrol= enrol %>% mutate(PROGRAM = factor(PRGCODE, levels=c("26.1102","27.0501","27.0502","27.0599","27.9999","52.1302"), 
                                         labels=c("Biostats",
                                                  "Stats, Gen",
                                                  "Math-Stats & Prob",
                                                  "Stats, Othr",
                                                  "Mathematics and Statistics, Other",
                                                  "Business Statistics") ))



#### Graduates ####
grad = read_delim("./Data/PSIS/RTRA_PSIS_graduates.csv",delim = ",", col_names=TRUE)
grad = grad[apply( !apply(grad[,1:5],2,is.na), 1, all), ]
names(grad)=str_replace_all(names(grad),"PSIS_","")
names(grad)=str_replace_all(names(grad),"_CODE","")
names(grad)=str_replace_all(names(grad),"_COUNT_","COUNT")
names(grad)=str_replace_all(names(grad),"CIP6_D","")
grad= grad %>% mutate(YEAR = as.Date(apply( cbind(1, 1, grad$REFYEAR_D), 1, paste, collapse="/"),"%d/%m/%Y")) %>% select(-REFYEAR_D)
grad= grad %>% mutate(SEX = factor(GENDER, levels=c("2","1"), labels=c("F","M") ))
grad= grad %>% mutate(GRADUATES = COUNT) %>% select(-COUNT)
grad= grad %>% mutate(LEVEL = factor(PCSCE_D, levels=c("66","76","86"), labels=c("BSc","MSc","PhD") )) %>% select(-PCSCE_D)
grad= grad %>% mutate(PROVINCE = factor(PROVOFSTUDY, levels=PROVOFSTUDY_levels, labels=PROVOFSTUDY_labels )) %>% select(-PROVOFSTUDY)
grad= grad %>% mutate(REGION = PROVINCE); levels(grad$REGION)=REGION_levels
grad= grad %>% mutate(REGION = factor(REGION,levels(REGION)[c(1,5,3,4,2)] ) )
grad= grad %>% mutate(PROGRAM = factor(PRGCODE, levels=c("26.1102","27.0501","27.0502","27.0599","27.9999","52.1302"), 
                                         labels=c("Biostatistics",
                                                  "Statistics, General",
                                                  "Mathematical Statistics and Probability",
                                                  "Statistics, Other",
                                                  "Mathematics and Statistics, Other",
                                                  "Business Statistics") ))


# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770019
enrol_all=read_csv("./Data/04770019-eng.csv", na='..')
enrol_all=enrol_all %>% filter(Ref_Date=="2014/2015",GEO!="Canada",TYPE=="University",
                          STATUS=="Total, registration status",
                          PCSCE=="Postsecondary 1st cycle education or equivalent",
                          CIPPG=="Total, instructional programs",
                          IMMIGRA=="Total, student status",
                          SEX %in% c("Females","Males")) %>%
  mutate(SEX=as.factor(SEX)) %>% mutate(REGION=as.factor(GEO))
levels(enrol_all$REGION)=c("Prairies","BC","Prairies","Atlantic","Atlantic","Atlantic","ON","Atlantic","QC","Prairies")
levels(enrol_all$SEX)=c("F","M")

# enrol_all=mutate(enrol_all, Ref_Date = as.numeric(str_extract( enrol[['Ref_Date']], "^[:digit:]+")) )
# StatsCan public data from http://www5.statcan.gc.ca/cansim/a26?lang=eng&retrLang=eng&id=4770020
# grad_all=read_csv("./Data/04770020-eng.csv", na='..')

#### UofT ####
#Data from http://cudo.utoronto.ca/
#Tri-campus data collected by data 
cudo_UT=read_csv("./Data/CUDO UofT.csv", na='..')

#### Programs ####
# list of programs
lop=read_delim("./Data/Stats Program Data - List of Programs.csv", col_names = TRUE, delim=',')
lop=lop %>% mutate(PROVINCE = as.factor(PROVINCE))
lop=lop %>% mutate(REGION = PROVINCE)
levels(lop$REGION) = c("Prairies","BC","Prairies","Atlantic","Atlantic","Atlantic","ON","Atlantic","QC","Prairies")
#lop%>% select(PROVINCE,REGION)%>%unique()

# Honours/Specialist programs only
lop.hs=lop %>% filter(Hons_Spec=="Y") %>% select(c(2:6),REGION) # keep only relevant columns
lop.hs=lop.hs %>% mutate( REGION = factor(REGION,levels(REGION)[c(3,2,4,1,5)] ) )

progs=list()
for(i in 1:nrow(lop.hs)){
  print( paste( i, " --- ", lop.hs$UNIVERSITY[i] ) )
  fname=paste("./Data/Stats Program Data - ", lop.hs$UNIVERSITY[i], ".csv", sep="")
  progs[[i]]=read_csv(fname, col_names = TRUE, col_types = cols( 
    Code = col_character(),
    Title = col_character(),
    Description = col_character(),
    Prerequisites = col_character(),
    Exclusions = col_character(),
    Discipline = col_character(),
    Level = col_character(),
    Type = col_character(),
    Category = col_character(),
    Comments = col_character() ) )
}

lop.hs=lop.hs %>% mutate( PROGRAMS = progs )

# all program info tibble 
aprogs=lop.hs %>% unnest(PROGRAMS)
aprogs=aprogs %>% mutate(Category= str_replace_all(Category," ","")) %>% 
  mutate(Category = str_split(Category, ",") ) %>% 
  mutate(Discipline= str_replace_all(Discipline," ","")) %>%
  mutate(Discipline = str_split(Discipline, ",") ) %>%
  mutate(Level = str_replace_all(Level," ","")) %>%
  mutate(Level = str_replace_all(Level,".0","")) %>%
  mutate(Level = str_replace_all(Level,",","")) %>%
  mutate(Level = str_split(Level,"")) %>%
  mutate(Type = factor(Type, levels=c("R","E","FE"), labels=c("Core","Elect","Free")))

