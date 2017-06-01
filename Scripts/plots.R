source("./Scripts/load data.R") 

# Key for Stats Programs
# 26.1102	Biostatistics
# 27.0501	Statistics, General
# 27.0502	Mathematical Statistics and Probability
# 27.0599	Statistics, Other
# 27.9999	Mathematics and Statistics, Other
# 52.1302	Business Statistics

##### Enrollment & Graduates ####
enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), YEAR >= as.Date('2009-01-01') ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=YEAR,y=ENROLMENTS, col=LEVEL, shape=LEVEL)) + geom_line(size=1.5) + geom_point(size=3) + scale_y_continuous(limits=c(0,2000)) 



grad %>% filter(PRGCODE %in% c(27.0501,27.0502,27.0599), LEVEL=="BSc", YEAR >= as.Date('2009-01-01') ) %>% 
  group_by(YEAR) %>% summarise(GRADUATES=sum(GRADUATES))%>%
  ggplot(aes(x=YEAR,y=GRADUATES)) + geom_line(size=1.5) + geom_point(size=3)
+ scale_y_continuous(limits=c(0,2000)) 

enrol %>% filter(PRGCODE %in% c(27.0501,27.0502,27.0599), LEVEL=="BSc", YEAR == as.Date('2014-01-01') ) %>% 
  group_by(GENDER, REGION) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=REGION, y=ENROLMENTS, fill=GENDER)) + geom_bar(stat = "identity")


enrol %>% filter(PRGCODE %in% c(27.0501,27.0502,27.0599), YEAR >= as.Date('2009-01-01') ) %>% 
  group_by(YEAR, LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=YEAR,y=ENROLMENTS, col=LEVEL, shape=LEVEL)) + geom_line(size=1.2) + geom_point(size=3) + scale_y_continuous(limits=c(0,2000)) 
grad %>% filter(PRGCODE %in% c(27.0501,27.0502,27.0599), YEAR >= as.Date('2009-01-01') ) %>% 
  group_by(YEAR, LEVEL) %>% summarise(GRADUATES=sum(GRADUATES))%>%
  ggplot(aes(x=YEAR,y=GRADUATES, col=LEVEL, shape=LEVEL)) + geom_line(size=1.2) + geom_point(size=3) + scale_y_continuous(limits=c(0,2000)) 


##### Programs ####


## by Discipline

