source("./Scripts/load data.R") 

aprogs = aprogs %>% filter(Type!="FE")
aprogs_dis = unnest( aprogs %>% select(-Category,-Level) %>% mutate(Credits = Credits / sapply(aprogs$Discipline, length) ))
aprogs_dis=mutate(aprogs_dis, Discipline = factor(Discipline))

aprogs_dis %>% group_by(Univ,Discipline) %>% summarize(n_Courses = sum(Credits)*2) %>% complete(Univ,Discipline, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Discipline,y=n_Courses)) + stat_summary(fun.y=mean, geom="bar")

aprogs_dis %>% group_by(Univ,Discipline) %>% summarize(n_Courses = sum(Credits)*2) %>% complete(Univ,Discipline, fill = list(n_Courses=0)) %>%
  ggplot(aes(x=Discipline,y=n_Courses)) + geom_boxplot()



aprogs_cat = unnest( aprogs %>% select(-Discipline,-Level) %>% mutate(Credits = Credits / sapply(aprogs$Category, length) ))
aprogs_lev = unnest( aprogs %>% select(-Category,-Discipline) %>% mutate(Credits = Credits / sapply(aprogs$Level, length) ))
aprogs_dis=mutate(aprogs_dis, Discipline = factor(Discipline))
aprogs_lev %>% ggplot(aes(x=Level,y=Credits)) + geom_boxplot()  
stat_summary(fun.y=sum, geom="bar")


aprogs_lev %>% group_by(Univ,Level) %>% summarise(cred=sum(Credits))


