source("./Scripts/load data.R") 

theme_set(theme_bw()); theme_update( text = element_text(size=14) )

# Key for Stats Programs
# 26.1102	Biostatistics
# 27.0501	Statistics, General
# 27.0502	Mathematical Statistics and Probability
# 27.0599	Statistics, Other

##### Enrollment & Graduates ####


#### Vital Stats ####
enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), YEAR >= as.Date('2010-01-01') ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=YEAR,y=ENROLMENTS, col=LEVEL, shape=LEVEL)) + geom_line(size=1.5) + geom_point(size=5) + 
  scale_y_continuous(limits=c(0,2000)) 

grad %>% filter(PRGCODE %in% c(27.0501,27.0502,27.0599), YEAR >= as.Date('2010-01-01') ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(GRADUATES=sum(GRADUATES))%>%
  ggplot(aes(x=YEAR,y=GRADUATES, col=LEVEL, shape=LEVEL)) + geom_line(size=1.5) + geom_point(size=5) + 
  scale_y_continuous(limits=c(0,400)) 

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), 
                 YEAR %in% c( as.Date('2010-01-01'),  as.Date('2014-01-01') ) ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS))

#### BSc Enrolment Breakdown ####

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), LEVEL=="BSc", YEAR == as.Date('2014-01-01') ) %>% 
  group_by(REGION, GENDER) %>% summarise(STAT_BSc_ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=REGION,y=STAT_BSc_ENROLMENTS, fill=GENDER)) + geom_bar(stat="identity")
  
lop %>% group_by(REGION) %>% summarise(UNIVERSITY_ENROLMENTS=sum(Enrolment))%>%
  ggplot(aes(x=REGION,y=UNIVERSITY_ENROLMENTS)) + geom_bar(stat="identity")













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

