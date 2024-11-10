twitter=read.csv("twitterdataset.csv",stringsAsFactors = FALSE)
twitter=head(twitter,n=500)
View(twitter)
str(twitter)
twitter$text = factor(twitter$text) #convert it into a factor, repeated values
str(twitter$text)

#table(twitter$text)

library(tm)
corpus<-iconv(twitter$text)
sms_corpus <- Corpus(VectorSource(twitter$text))
print(sms_corpus)
inspect(sms_corpus[1:2]) #summary of 1st and 2nd SMS in corpus 
as.character(sms_corpus[[1]]) #read 1st message in character form 

lapply(sms_corpus[1:2], as.character) #read full 1st and 2nd SMS in character

sms_corpus_clean = tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus[[1]]) #inspect the first SMS
as.character(sms_corpus_clean[[1]]) #compare it to 1st stored in corpus





sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
sms_corpus_clean <- tm_map(sms_corpus_clean, toSpace, "/")
sms_corpus_clean <- tm_map(sms_corpus_clean, toSpace, "@")
sms_corpus_clean <- tm_map(sms_corpus_clean, toSpace, "\\|")


#removePunctuation("hello...world")


sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[1])

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)


sms_dtm

sms_dtm_train <- sms_dtm[1:350, ]
sms_dtm_test <- sms_dtm[350:500, ]
sms_train_labels <- twitter[1:350, ]$sentiment #labels $text
sms_test_labels <- twitter[350:500, ]$sentiment

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))




library(wordcloud)
wordcloud(twitter$text, min.freq = 100, random.order = FALSE)


positive <- subset(twitter, sentiment == "Positive")
wordcloud(positive$text, min.freq = 50, random.order = FALSE)
negative <- subset(twitter, sentiment == "Negative")
wordcloud(negative$text, min.freq = 50, random.order = FALSE)

neutral <- subset(twitter, sentiment == "Neutral")
wordcloud(neutral$text, min.freq = 50, random.order = FALSE)
