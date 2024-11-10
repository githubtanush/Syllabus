getwd()
spam=read.csv("sms_spam.csv",stringsAsFactors = FALSE)
View(spam)
str(spam)
spam$type = factor(spam$type) #convert it into a factor, repeated values
str(spam$type)

table(spam$type)

library(tm)
#corpus<-iconv(spam$text)
sms_corpus <- Corpus(VectorSource(spam$text))
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


removePunctuation("hello...world")

library(SnowballC)

wordStem(c("learn", "learned", "learning", "learns"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[1])

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,
                                                          removeNumbers = TRUE,
                                                          stopwords = TRUE,
                                                          removePunctuation = TRUE))


sms_dtm
sms_dtm2
View(sms_dtm_train)
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- spam[1:4169, ]$type #labels $type
sms_test_labels <- spam[4170:5559, ]$type

prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))




library(wordcloud)
wordcloud(spam$text, min.freq = 100, random.order = FALSE)

ham <- subset(spam, type == "ham")
wordcloud(ham$text, min.freq = 50, random.order = FALSE)
spam <- subset(spam, type == "spam")

wordcloud(spam$text, min.freq = 50, random.order = FALSE)






findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_counts)

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
             prop.chisq = FALSE, prop.t = FALSE,
             dnn = c('predicted', 'actual'))

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

