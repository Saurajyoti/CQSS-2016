library(tm)
library(lda)
library(topicmodels)
library(LDAvis)

source("functions.R")

stop_words <- stopwords("SMART")

open_ended <- dataset %>% select(contains("Open.Ended.Response")) %>% colnames

perform.lda(open_ended[1]) #family relocation
perform.lda(open_ended[4]) #harassment
perform.lda(open_ended[5]) #academic satisfaction
perform.lda(open_ended[6]) #overall satisfaction
perform.lda(open_ended[7]) #internship
perform.lda(open_ended[16]) #course representatives


#changing to dataset with feedback on each university
dataset <- as.data.frame(read_csv("../../Media/2016/Master_tables/open_ended_per_university.csv"))
per_university <- names(dataset)
perform.lda(per_university[5]) #orientation and integration
perform.lda(per_university[6]) #feedback improvement
