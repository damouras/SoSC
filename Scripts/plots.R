source("./Scripts/load data.R") 

theme_set(theme_bw()); 
theme_update(axis.title = element_text(size=14),
             axis.text = element_text(size=14),
             plot.title = element_text(size = 18, face = "bold"),
             legend.title = element_text(size = 14),
             legend.text = element_text(size = 14)
)

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
  scale_y_continuous(limits=c(0,2000)) + ggtitle("Stats Enrolments by Year & Level of Study")

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), YEAR==as.Date('2014-01-01') ) %>% 
  group_by(PROGRAM,LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS)) %>%
  ggplot(aes(x=LEVEL,y=ENROLMENTS, fill=PROGRAM)) + 
  geom_bar(stat="identity")  + ggtitle("2014 Stats Enrolments by Level of Study & Program")
  

grad %>% filter(PRGCODE %in% c(27.0501,27.0502,27.0599), YEAR >= as.Date('2010-01-01') ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(GRADUATES=sum(GRADUATES))%>%
  ggplot(aes(x=YEAR,y=GRADUATES, col=LEVEL, shape=LEVEL)) + geom_line(size=1.5) + geom_point(size=5) + 
  scale_y_continuous(limits=c(0,400)) + ggtitle("Stats Degrees by Year & Level of Study") + labs(y="DEGREES")

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), 
                 YEAR %in% c( as.Date('2010-01-01'),  as.Date('2014-01-01') ) ) %>% 
  group_by(YEAR,LEVEL) %>% summarise(ENROLMENTS=sum(ENROLMENTS))

#### BSc Enrolment Breakdown

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), 
                 LEVEL=="BSc", YEAR == as.Date('2014-01-01') ) %>% 
  mutate(ENROLMENTS=ENROLMENTS/sum(ENROLMENTS)) %>%
  group_by(SEX, REGION) %>% summarize(ENROLMENTS=sum(ENROLMENTS)) %>%
  ggplot(aes(x=REGION, y=ENROLMENTS, fill=SEX)) + geom_bar(stat = "identity")+
  scale_x_discrete(limits = c("Atlantic","BC","ON","QC","Prairies")) + 
  scale_y_continuous(limits=c(0,.7)) + 
  ggtitle("Stats BSc Enrolments (2014) by Region & Sex") + labs(y="ENROLMENTS (%)")
  
enrol_all %>% mutate(ENROLMENTS=Value/sum(Value)) %>%
  group_by(REGION, SEX) %>% summarise(ENROLMENTS =sum(ENROLMENTS)) %>% 
  ggplot(aes(x=REGION,y=ENROLMENTS, fill=SEX)) + geom_bar(stat="identity") +
  scale_x_discrete(limits = c("Atlantic","BC","ON","QC","Prairies")) + 
  scale_y_continuous(limits=c(0,.7)) + 
  ggtitle("Total BSc Enrolments (2014) by Region & Sex") + labs(y="ENROLMENTS (%)")

enrol %>% filter(PRGCODE %in% c(26.1102, 27.0501,27.0502,27.0599), LEVEL=="BSc", YEAR == as.Date('2014-01-01') ) %>% 
  group_by(SEX) %>% summarise(ENROLMENTS=sum(ENROLMENTS))
  

##### Programs ####

#### List of Universities
aprogs %>%  group_by(UNIVERSITY) %>% slice(1) %>%
  ggplot(aes(x=UNIVERSITY, y=Enrolment, fill=REGION)) + geom_bar(stat="identity") + coord_flip() +
  scale_x_discrete(limits = as.character(lop.hs%>%arrange(desc(as.character(REGION)))%>%.[['UNIVERSITY']]) ) +
  ggtitle("Universities with Stats Hons/Spec by Size & Region") + labs(y="TOTAL UNIVERSITY ENROLMENT (2016)") 


#### Number of Credits

aprogs %>% filter(Type!="Free") %>% group_by(UNIVERSITY,Type) %>% summarise(N_COURSES=2*sum(Credits)) %>%
  ggplot(aes(x=UNIVERSITY, y=N_COURSES, fill=Type)) + geom_bar(stat="identity") + coord_flip() + 
  scale_x_discrete(limits = as.character(lop.hs%>%arrange(desc(as.character(REGION)))%>%.[['UNIVERSITY']]) )  +
  ggtitle("Program Requirements by University") + labs(y="# of COURSES") +
  geom_col(position = position_stack(reverse = TRUE))

aprogs %>% filter(Type!="Free") %>% group_by(UNIVERSITY) %>% summarise(N_COURSES=2*sum(Credits)) %>%
  ggplot(aes(x="", y=N_COURSES)) + geom_boxplot() 

aprogs %>% filter(Type!="Free") %>% group_by(UNIVERSITY, Type) %>% 
  summarise(n=2*sum(Credits)) %>% mutate( Ratio = n/sum(n)) %>%
  filter(Type=="Core") %>% select(Ratio) %>% summary()
  

#### by Discipline

aprogs_dis = aprogs %>% filter(Type!="Free") %>% 
  mutate(Credits = Credits / sapply(Discipline, length) ) %>% unnest(Discipline)
aprogs_dis=aprogs_dis %>% mutate(Discipline = factor(Discipline, levels=c("COMP","MATH","STAT","OTHR"))) 

aprogs_dis %>% group_by(UNIVERSITY,Discipline) %>% summarize(n_Courses = sum(Credits)*2) %>% 
  complete(UNIVERSITY,Discipline, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Discipline,y=n_Courses, fill=Discipline)) + geom_boxplot() + 
  ggtitle("Number of Courses by Discipline ") + labs(y="# of COURSES", x="DISCIPLINE") 

aprogs_dis %>% group_by(Discipline) %>% summarize(n_Courses = sum(Credits)*2) %>% 
  mutate(Prop=n_Courses/sum(n_Courses))
  
aprogs_dis_lev = aprogs %>% filter(Type!="Free") %>% 
  mutate(Credits = Credits / sapply(Discipline, length) ) %>% unnest(Discipline, .drop=FALSE) %>%
  mutate(Credits = Credits / sapply(Level, length) ) %>% unnest(Level) %>% 
  mutate(Discipline = factor(Discipline, levels=c("COMP","MATH","STAT","OTHR"))) %>% 
  mutate(Level = factor(Level, levels=c("1","2","3","4"))) 

aprogs_dis_lev %>% group_by(Discipline, Level) %>% summarise(Credits=sum(Credits)*2/24) %>% 
  mutate(Level = as.integer(Level)) %>%  
  ggplot(aes(x=Level,y=Credits, col=Discipline, shape=Discipline)) + 
  geom_line(size=1.5) + geom_point(size=5) +
  scale_shape_manual(values=c(15,16,17,18)) + 
  ggtitle("Average Number of Courses by Discipline and Level") + labs(y="# of COURSES") 


## by Topic Category
aprogs_cat = aprogs %>% filter(Type!="Free") %>% 
  mutate(Credits = Credits / sapply(Category, length) ) %>% unnest(Category)
aprogs_cat = aprogs_cat %>% 
  mutate( Category = factor(Category,levels = c("CS","MT","PT","SM","SP","ST","OT"))) 

aprogs_cat %>% group_by(UNIVERSITY,Category) %>% summarize(n_Courses = sum(Credits)*2) %>% 
  complete(UNIVERSITY, Category, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Category, y=n_Courses, fill=Category)) + geom_boxplot() + 
  scale_fill_discrete(name="TOPIC \n CATEGORY",
                      breaks=c("CS","MT","PT","SM","SP","ST","OT"),
                      labels=c("Computation","Mathematics","Probability","Stat Methodology","Stat Practice","Stat Theory","Other")) +
  #scale_x_discrete(limits=c("CS","MT","PT","SM","SP","ST","OT"))+
  ggtitle("Number of Courses by Topic Category") + labs(y="# of COURSES", x="TOPIC CATEGORY")
  
aprogs_dis_cat = aprogs %>% filter(Type!="Free") %>% 
  mutate(Credits = Credits / sapply(Discipline, length) ) %>% unnest(Discipline, .drop=FALSE) %>%
  mutate(Credits = Credits / sapply(Category, length) ) %>% unnest(Category) %>% 
  mutate(Discipline = factor(Discipline, levels=c("COMP","MATH","STAT","OTHR"))) %>%
  mutate( Category = factor(Category,levels = c("CS","MT","PT","SM","SP","ST","OT"))) 

aprogs_dis_cat %>% group_by(Discipline, Category) %>% summarise(Credits=sum(Credits)*2/24) %>% 
  ggplot(aes(x=Discipline, y=Credits, fill=Category)) + geom_bar(stat = "identity") +
  ggtitle("Average # of Courses by Discipline and Topic Category") + labs(y="# of COURSES", x="DISCIPLINE") +
  scale_fill_discrete(name="TOPIC \n CATEGORY",
                      breaks=c("CS","MT","PT","SM","SP","ST","OT"),
                      labels=c("Computation","Mathematics","Probability","Stat Methodology","Stat Practice","Stat Theory","Other"))
  


