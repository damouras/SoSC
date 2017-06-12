source("./Scripts/load data.R") 

theme_set(theme_bw()); 
theme_update(axis.title = element_text(size=14),
             axis.text = element_text(size=14),
             plot.title = element_text(size = 18, face = "bold"),
             legend.title = element_text(size = 14),
             legend.text = element_text(size = 14) )


temp = aprogs %>% filter(sapply(Category,length)==1) %>% mutate(Category = unlist(Category)) %>%
  filter(Category %in% c("CS","MT","PT","SM","ST","SP"))
# %>% group_by(Category) %>% summarise( Desc_all = paste(Description, collapse=" "))
par(mfrow=c(0,0), mar=c(0,0,0,0))
for(ctg in c("CS","MT","PT","SM","ST","SP") ){
  my_word_cloud(temp %>% filter(Category==ctg) %>%.[['Description']] )  
  #title(main=ctg)
}







