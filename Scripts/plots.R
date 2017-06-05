source("./Scripts/load data.R") 

theme_set(theme_bw()); 
theme_update( text = element_text(size=12) )
theme_update(plot.title = element_text(size = 15, face = "bold"))

# Key for Stats Programs
# 26.1102	Biostatistics
# 27.0501	Statistics, General
# 27.0502	Mathematical Statistics and Probability
# 27.0599	Statistics, Other

##### Enrollment & Graduates ####

#### Vital Stats
enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), YEAR >= as.Date('2010-01-01') ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=YEAR,y=ENROLMENTS, col=LEVEL, shape=LEVEL)) + geom_line(size=1.5) + geom_point(size=5) + 
  scale_y_continuous(limits=c(0,2000)) + ggtitle("Stats Enrolments Time Series by Level of Study")

grad %>% filter(PRGCODE %in% c(27.0501,27.0502,27.0599), YEAR >= as.Date('2010-01-01') ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(GRADUATES=sum(GRADUATES))%>%
  ggplot(aes(x=YEAR,y=GRADUATES, col=LEVEL, shape=LEVEL)) + geom_line(size=1.5) + geom_point(size=5) + 
  scale_y_continuous(limits=c(0,400)) + ggtitle("Stats Graduates Time Series by Level of Study")

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), 
                 YEAR %in% c( as.Date('2010-01-01'),  as.Date('2014-01-01') ) ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS))

#### BSc Enrolment Breakdown

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), LEVEL=="BSc", YEAR == as.Date('2014-01-01') ) %>% 
  group_by(SEX, REGION) %>% summarise(ENROLMENTS=sum(ENROLMENTS))%>%
  ggplot(aes(x=REGION, y=ENROLMENTS, fill=SEX)) + 
  geom_bar(stat = "identity")+
  scale_x_discrete(limits = c("Atlantic","BC","ON","QC","Prairies")) + 
  ggtitle("Stats BSc Enrolments (2014) by Region/Sex")

enrol_all %>% group_by(REGION, SEX) %>% summarise(ENROLMENTS =sum(Value)) %>% 
  ggplot(aes(x=REGION,y=ENROLMENTS, fill=SEX)) + geom_bar(stat="identity") +
  scale_x_discrete(limits = c("Atlantic","BC","ON","QC","Prairies")) + 
  ggtitle("Total BSc Enrolments (2014) by Region/Sex")

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), LEVEL=="BSc", YEAR == as.Date('2014-01-01') ) %>% 
  group_by(SEX) %>% summarise(ENROLMENTS=sum(ENROLMENTS))
  

##### Programs ####

#### List of Universities
aprogs %>%  group_by(UNIVERSITY) %>% slice(1) %>%
  ggplot(aes(x=UNIVERSITY, y=Enrolment, fill=REGION)) + geom_bar(stat="identity") + coord_flip() +
  scale_x_discrete(limits = as.character(lop.hs%>%arrange(desc(as.character(REGION)))%>%.[['UNIVERSITY']]) ) +
  ggtitle("List of Universities with Stats Honours/Specialist program") + labs(y="TOTAL UNIVERSITY ENROLMENT") 


#### Number of Credits

aprogs %>% filter(Type!="Free") %>% group_by(UNIVERSITY,Type) %>% summarise(N_COURSES=2*sum(Credits)) %>%
  ggplot(aes(x=UNIVERSITY, y=N_COURSES, fill=Type)) + geom_bar(stat="identity") + coord_flip() + 
  scale_x_discrete(limits = as.character(lop.hs%>%arrange(desc(as.character(REGION)))%>%.[['UNIVERSITY']]) )  +
  ggtitle("Program Requirements by University") + labs(y="NUMBER of SEMESTER COURSES") +
  geom_col(position = position_stack(reverse = TRUE))

aprogs %>% filter(Type!="Free") %>% group_by(UNIVERSITY) %>% summarise(N_COURSES=2*sum(Credits)) %>%
  ggplot(aes(x="", y=N_COURSES)) + geom_boxplot() 

aprogs %>% filter(Type!="Free") %>% group_by(UNIVERSITY, Type) %>% 
  summarise(n=2*sum(Credits)) %>% mutate( Ratio = n/sum(n)) %>%
  filter(Type=="Core") %>% select(Ratio) %>% summary()
  

#### by Discipline

aprogs %>% mutate( test = sapply( Discipline, paste, collapse=" ") ) %>%
  filter( !(test %in% c("MATH","STAT","OTHR","COMP") ) ) %>% select(UNIVERSITY,Discipline)
                   
aprogs_dis = aprogs %>% filter(Type!="FE") %>% select(-Category,-Level) %>% 
  mutate(Credits = Credits / sapply(Discipline, length) ) %>% unnest( )
aprogs_dis=aprogs_dis %>% mutate(Discipline = factor(Discipline, levels=c("COMP","MATH","STAT","OTHR"))) 
levels(aprogs_dis$Discipline)

aprogs_dis %>% group_by(UNIVERSITY,Discipline) %>% summarize(COURSES = sum(Credits)*2) %>% 
  complete(UNIVERSITY,Discipline, fill = list(COURSES=0)) %>%
  ggplot(aes(x=Discipline,y=COURSES)) + stat_summary(fun.y=mean, geom="bar") 


aprogs_dis %>% group_by(UNIVERSITY,Discipline) %>% summarize(COURSES = sum(Credits)*2) %>% 
  complete(UNIVERSITY,Discipline, fill = list(COURSES=0)) %>%
  ggplot(aes(x=Discipline,y=COURSES)) + stat_boxplot() 

aprogs_dis %>% group_by(UNIVERSITY,Discipline) %>% summarize(n_Courses = sum(Credits)*2) %>% complete(UNIVERSITY,Discipline, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Discipline,y=n_Courses)) + geom_boxplot()

## by Category
aprogs_cat = aprogs %>% filter(Type!="FE") %>% select(-Discipline,-Level) %>% 
  mutate(Credits = Credits / sapply(Category, length) ) %>% unnest( )
aprogs_cat = mutate( aprogs_cat, Category = factor(Category))
aprogs_cat %>% group_by(UNIVERSITY,Category) %>% summarize(n_Courses = sum(Credits)*2) %>% 
  complete(UNIVERSITY,Category, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Category,y=n_Courses)) + geom_boxplot()

## by Level
aprogs_lev = aprogs %>% filter(Type!="FE") %>% select(-Category,-Discipline) %>% 
  mutate(Credits = Credits / sapply(Level, length) ) %>% unnest( )
aprogs_lev=aprogs_lev %>% mutate(Level = factor(Level))

aprogs_lev %>% group_by(UNIVERSITY,Level) %>% summarize(n_Courses = sum(Credits)*2) %>% 
  complete(UNIVERSITY,Level, fill = list(n_Courses=0)) %>% mutate(Level=as.numeric(Level)) %>%
  ggplot(aes(x=Level,y=n_Courses,col=UNIVERSITY)) + geom_line()  





