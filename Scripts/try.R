source("./Scripts/load data.R") 

theme_set(theme_bw()); 
theme_update(axis.title = element_text(size=14),
             axis.text = element_text(size=14),
             plot.title = element_text(size = 18, face = "bold"),
             legend.title = element_text(size = 14),
             legend.text = element_text(size = 14) )


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

temp = aprogs %>% filter(sapply(Category,length)==1) %>% mutate(Category = unlist(Category)) %>%
  filter(Category %in% c("CS","MT","PT","SM","ST","SP"))
# %>% group_by(Category) %>% summarise( Desc_all = paste(Description, collapse=" "))
par(mfrow=c(0,0), mar=c(0,0,0,0))
for(ctg in c("CS","MT","PT","SM","ST","SP") ){
  my_word_cloud(temp %>% filter(Category==ctg) %>%.[['Description']] )  
  #title(main=ctg)
}


my_word_cloud=function(x, MAX.WRDS=15){
  docs = Corpus(VectorSource(x))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("statistics", "statistical", "course", "introduction",'students',"will")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=MAX.WRDS, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))

}






