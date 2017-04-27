library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)

#### Enrolments ####
setwd("~/GitHub/SoSE")
enrol=read_csv("./Data/04770019-eng.csv", na='..')
enrol=mutate(enrol, Ref_Date = as.numeric(str_extract( enrol[['Ref_Date']], "^[:digit:]+")) )
glimpse(enrol)

enrol %>% 
  filter( grepl("Mathematics",CIPPG) ) %>% 
  filter( grepl("Canada",GEO) ) %>% 
  filter( TYPE=="University" ) %>% 
  filter( grepl("Both",SEX) ) %>% 
  filter( grepl("Total",STATUS ) ) %>% 
  filter( grepl("Total",IMMIGRA ) ) %>% 
  filter( grepl("Total",PCSCE ) ) %>% 
  select(Ref_Date, Value) %>% plot( type='b')

enrol %>% filter( Coordinate=="1.2.1.13.9.1.1") %>% select(Ref_Date, Value) %>% ggplot( aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1) 
 

#### Graduates ####
grad=read_csv("./Data/04770020-eng.csv", na='..')
glimpse(grad)

grad %>% filter( Coordinate=="1.2.13.9.1.1") %>% select(Ref_Date, Value) %>% ggplot( aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1)


#### UofT ####
cudo_UT=read_csv("./Data/CUDO UofT.csv", na='..')



