setwd("Desktop")

library(tm)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(qdap)
library(wordcloud)

#create a corpus
bucks.docs <- read.csv(file="foodandbev tmbo.csv", head=TRUE, sep=",")
bucks.corpus <- Corpus(DataframeSource(data.frame(bucks.docs$text)))

#clean the text
bucks.corpus <- tm_map(bucks.corpus, removePunctuation)
bucks.corpus <- tm_map(bucks.corpus, removeNumbers)
bucks.corpus <- tm_map(bucks.corpus, tolower)
bucks.corpus <- tm_map(bucks.corpus, stripWhitespace)
 
#remove stop words
custom.stopwords <- c(stopwords("english"), "bucks", "nba", "food","beverage","game","eat","like")
bucks.corpus <- tm_map(bucks.corpus, removeWords, custom.stopwords)

#make a TDM then change to a simple matrix 
bucks.corpus <- tm_map(bucks.corpus, PlainTextDocument)
bucks.tdm <- TermDocumentMatrix(bucks.corpus)
bucks.tdm.m <- as.matrix(bucks.tdm)

#sum each row, or term 
bucks.tdm.v <- sort(rowSums(bucks.tdm.m),decreasing=TRUE)

#create a data frame for the wordcloud function
list_of_price_words <- c('price','prices')

bucks.tdm.d <- data.frame(word = names(bucks.tdm.v),freq=bucks.tdm.v) 

#group similar words to avoid redundancy
bucks.tdm.d$group[bucks.tdm.d$word %in% bucks.tdm.d$word] <- as.character(bucks.tdm.d$word)
bucks.tdm.d$group[bucks.tdm.d$word %in% list_of_price_words] <- "price"

#calculate frequency of groups
total_group_numbers <- bucks.tdm.d %>% group_by(group) %>% summarise(total_word_count = sum(freq))
bucks.tdm.d <- total_group_numbers

#look at all available color palettes
display.brewer.all()

#Choose a palette
pal <- brewer.pal(8, "Set2")

#create the word cloud
wordcloud(bucks.tdm.d$group,bucks.tdm.d$total_word_count,max.words=500, random.order=FALSE, colors=pal)