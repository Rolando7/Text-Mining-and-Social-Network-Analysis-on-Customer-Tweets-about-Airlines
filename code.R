install.packages(stringr)
install.packages(graph) 
install.packages(grid)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)

library(igraph)
library(grid)
library(stringr)
library(rpart)

delta = read.csv("E:/MIS/BIA658/Final Project/Delta Airline.csv", stringsAsFactors = F, row.names = 1)
delta1 <- subset(delta, lang=='en')
#delta_tweets <- delta$description
#write.csv(delta_tweets, file = "E:/MIS/BIA658/Final Project/tweets.txt")

# Clean tweets
delta_clean = gsub("&amp", "", delta1$title)
delta_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", delta_clean)
delta_clean = gsub("@\\w+", "", delta_clean)
delta_clean = gsub("[[:punct:]]", "", delta_clean)
delta_clean = gsub("[[:digit:]]", "", delta_clean)
delta_clean = gsub("http\\w+", "", delta_clean)
delta_clean = gsub("[ \t]{2,}", "", delta_clean)
delta_clean = gsub("^\\s+|\\s+$", "", delta_clean) 
delta_clean = gsub("[^\x20-\x7E]", "", delta_clean)

#build a corpus and specify the source to be character vectors
delta_Corpus = Corpus(VectorSource(delta_clean))
delta_Corpus[[1]]$content


# Change to lower case
delta_Corpus = tm_map(delta_Corpus, content_transformer(tolower))
#Remove numbers
delta_Corpus = tm_map(delta_Corpus, removeNumbers)
#Remove punctuation marks and stopwords
delta_Corpus = tm_map(delta_Corpus, removePunctuation)
delta_Corpus = tm_map(delta_Corpus, removeWords, c("can", "come", "someone", "dont", "isnt", "like", "need", "now", "one", "thank", "thatleaves", "theteam", 
                                                   "just","im", "youfor","will","that", "still", "got", "get", "flight", "rt","whatever","â",stopwords("english")))

 

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
delta_Corpus = tm_map(delta_Corpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
delta_Corpus = tm_map(delta_Corpus, content_transformer(removeNumPunct))

toSpace<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
delta_Corpus<-tm_map(delta_Corpus,toSpace,"Ã¢â¬Å“~/|@|\\|")

# Remove extra whitespaces
delta_Corpus = tm_map(delta_Corpus, stripWhitespace)
delta_Corpus[[1]]$content


# Document-Term Matrix: documents as the rows, terms/words as the columns, frequency of the term in the document as the entries. Notice the dimension of the matrix
delta_Corpus_tdm = TermDocumentMatrix(delta_Corpus, control = list(wordLengths = c(1, Inf)))
delta_Corpus_dtm = DocumentTermMatrix(delta_Corpus, control = list(wordLengths = c(1, Inf)))
delta_Corpus_tdm

inspect(delta_Corpus[1:10])

# inspect frequent words
freq.terms <- findFreqTerms(delta_Corpus_tdm, lowfreq = 100)
term.freq <- rowSums(as.matrix(delta_Corpus_tdm))
term.freq <- subset(term.freq, term.freq >= 150)
df <- data.frame(term = names(term.freq), freq = term.freq)

# Term count
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=12))


m <- as.matrix(delta_Corpus_tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]



# Simple word cloud 
findFreqTerms(delta_Corpus_dtm, 100)
freq = data.frame(sort(colSums(as.matrix(delta_Corpus_dtm)),decreasing = TRUE))
wordcloud(rownames(freq), freq[,1], max.words = 200, colors = brewer.pal(9, "Dark2"))


#plot network of terms
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz", suppressUpdates=TRUE)

library(Rgraphviz)
delta_Corpus_tdm <- Corpus(VectorSource(delta_Corpus_tdm))
plot(delta_Corpus_tdm, term = findFreqTerms(delta_Corpus_tdm, lowfreq = 200)[1:15], weighting = T)

#Topic Modelling
dtm <- as.DocumentTermMatrix(delta_Corpus_dtm)
install.packages("topicmodels")
library(topicmodels)
lda<- LDA(dtm, k=8) #find 8 topics
term <-  terms(lda, 7) #first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ","))

topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.IDate(delta_Corpus$date), topic=topics)
        ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")

#############################################
# install package sentiment140
install.packages("devtools")
require(devtools)
install_github("sentiment140", "okugami79")


# sentiment analysis
library(sentiment)
sentiments <- sentiment(delta_Corpus$content)
table(sentiments$polarity)

# sentiment plot
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.df$created)

result <- aggregate(score ~ date, data = sentiments, sum)

plot(result, type = "l")


####################################################

#####################
# Sentiment Analysis 
install.packages  ('plyr')
install.packages ('stirngr')
install.packages ('ggplot2')
library(plyr)
library(stringr)
library (ggplot2)


score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}


pos <- scan('E:/MIS/BIA658/Material/Prof/data/positive-words.txt', what='character', comment.char=';')
neg <- scan('E:/MIS/BIA658/Material/Prof/data/negative-words.txt', what='character', comment.char=';') 
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail')

analysis <- score.sentiment(delta_clean,pos.words,neg.words)
table(analysis$score)


neutral <- length(which(analysis$score == 0))
positive <- length(which(analysis$score > 0))
negative <- length(which(analysis$score < 0))
Sentiment <- c("Negative","Neutral","Positive")
Count <- c(negative,neutral,positive)
output <- as.data.frame(Sentiment,Count)


#bar plot of sentiment analysis

qplot(Sentiment,Count,data=output,geom = "histogram", fill=Sentiment, 
      xlab = "sentiment", ylab = "count",main="Sentiment Analysis")
ggplot(output, aes(x=Sentiment,y = Count, fill=Sentiment)) +geom_bar(stat = "identity")


