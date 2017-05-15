library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)


temp %>% filter(Elective=="N") %>% group_by(Level) %>% summarize(sum(Credits))
