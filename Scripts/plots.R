source("./Scripts/load data.R") 

##### Enrollment & Graduates ####
enrol %>% filter( Coordinate=="1.2.1.13.9.1.1") %>% select(Ref_Date, Value) %>% ggplot( aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1) 
grad %>% filter( Coordinate=="1.2.13.9.1.1") %>% select(Ref_Date, Value) %>% ggplot( aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1)
cudo_UT %>% filter( FoS=="CS") %>% select(Year, Enroll) %>% ggplot( aes(Year,Enroll)) + geom_point(size=3) + geom_line(lwd=1)

ggplot( filter(enrol, Coordinate=="1.2.1.13.9.1.1"), aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1) 
ggplot(filter(grad, Coordinate=="1.2.13.9.1.1"),aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1)

temp=grad %>% filter( Coordinate=="1.2.1.9.1.1") %>% select(Ref_Date, Value)

ggplot( filter(enrol, Coordinate=="1.2.1.13.9.1.1"), aes(Ref_Date,Value)) + geom_point(size=3) + geom_line(lwd=1) + 
   geom_line(data=filter(grad, Coordinate=="1.2.1.9.1.1"),  aes(x=Ref_Date,y=Value), lwd=1, col=2)

cudo_UT %>% ggplot() + geom_line(aes(x=Year, y=Enroll, group = FoS, colour=FoS)) + 
  geom_line(data=cudo_UT, aes(x=Year, y=Grad, group = FoS, colour=FoS))



##### Programs ####

## by Discipline

aprogs_dis = aprogs %>% filter(Type!="FE") %>% select(-Category,-Level) %>% 
  mutate(Credits = Credits / sapply(Discipline, length) ) %>% unnest( )
aprogs_dis=mutate(aprogs_dis, Discipline = factor(Discipline))

aprogs_dis %>% group_by(Univ,Discipline) %>% summarize(n_Courses = sum(Credits)*2) %>% 
  complete(Univ,Discipline, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Discipline,y=n_Courses)) + stat_summary(fun.y=mean, geom="bar")

aprogs_dis %>% group_by(Univ,Discipline) %>% summarize(n_Courses = sum(Credits)*2) %>% complete(Univ,Discipline, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Discipline,y=n_Courses)) + geom_boxplot()

## by Category
aprogs_cat = unnest( aprogs %>% select(-Discipline,-Level) %>% mutate(Credits = Credits / sapply(aprogs$Category, length) ))

## by Level
aprogs_lev = aprogs %>% filter(Type!="FE") %>% select(-Category,-Discipline) %>% 
  mutate(Credits = Credits / sapply(Level, length) ) %>% unnest( )
aprogs_lev=aprogs_lev %>% mutate(Level = factor(Level))

aprogs_lev %>% group_by(Univ,Level) %>% summarize(n_Courses = sum(Credits)*2) %>% 
  complete(Univ,Level, fill = list(n_Courses=0)) %>% mutate(Level=as.numeric(Level)) %>%
  ggplot(aes(x=Level,y=n_Courses,col=Univ)) + geom_line()  



