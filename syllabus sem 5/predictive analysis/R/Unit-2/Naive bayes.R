#Naive Bayes Algorithm
#It can take both text and numerical value as input
#Preprocessing is necessary
#It is called Bayes because it depends on the principle of Bayes Theorem
#It can be used for Binary as well as Multi-class Classifications.
#It is the most popular choice for text classification problems.

library(e1071)
library(caTools)
library(caret)
split<-sample.split(iris,SplitRatio = 0.7)
train_cl<-subset(iris,split == "TRUE")
test_cl<-subset(iris,split == "FALSE")
#FEATURE_SCALING
train_scale<-scale(train_cl[,1:4])
test_scale<-scale(test_cl[,1:4])
#fitting naive bayes algorithm
#training dataset
set.seed(120) #setting seed
classifier_cl<-naiveBayes(Species ~ .,data = train_cl)
classifier_cl
#predicting on test data
y_pred <- predict(classifier_cl,newdata = test_cl)
#confusion matrix
cm<-table(test_cl$Species,y_pred)
cm
a=confusionMatrix(cm)
acc=sum(diag(cm))/sum(cm)
acc
accuracy <- mean(y_pred == test_cl$Species)
accuracy

#------------------------------------------------------------------------------------------------------------------------------------
#Preprocessing of data

#gsub
a=read.csv(file.choose())
View(a)
str(a)
#google_tweets<-twListToDF(a)
google_tweets=a
View(google_tweets)
google_text<-a$text
google_text<-tolower(google_text)#Convert all text to lower case
google_text[1:5]
google_text<-gsub("rt","",google_text)#Replace blank space ("rt")
google_text<-gsub("@\\w+","",google_text)#Replace @UserName
google_text<-gsub("[[:punct:]]","",google_text)#Remove Punctuation
google_text<-gsub("http\\w+","",google_text)#Remove links
google_text<-gsub("[ |\t]{2,}","",google_text)#remove tabs
google_text<-gsub("^ ","",google_text)#remove blank spaces  at the beginning
google_text<-gsub(" $","",google_text)#remove blank spaces at the end
?gsub

#Text Mining

#lib(tm)
#a=read.csv(file.choose())
#b=Corpus(VectorSource(a$text))
#c=tm_map(b,tolower)
#inspect(c)

a=read.csv(file.choose())
str(a)
summary(a)
library(tm)
corpus<-iconv(a$text)
corpus<-Corpus(VectorSource(a))
inspect(corpus[1:2])
corpus=tm_map(b,tolower)
inspect(corpus[1:5])
corpus=tm_map(c,removePunctuation)
inspect(corpus[1:5])
corpus=tm_map(c,removeNumbers)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords,stopwords('english'))
inspect(cleanset[1:5])
removeURL<-function(x)gsub('http.*','',x)
cleanset<-tm_map(cleanset,content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords,c('anyone','walker16'))
cleanset<-tm_map(cleanset,gsub,pattern='stocks',replacement='stock')
cleanset<-tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])


#------------------------------------------------------------------------------------------------------------------------------------
#twitter dataset
twitter=read.csv(file.choose(),stringsAsFactors = FALSE)
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



install.packages("wordcloud")
library(wordcloud)
wordcloud(twitter$text, min.freq = 100, random.order = FALSE)


positive <- subset(twitter, sentiment == "Positive")
wordcloud(positive$text, min.freq = 50, random.order = FALSE)
negative <- subset(twitter, sentiment == "Negative")
wordcloud(negative$text, min.freq = 50, random.order = FALSE)

neutral <- subset(twitter, sentiment == "Neutral")
wordcloud(neutral$text, min.freq = 50, random.order = FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------

getwd()
spam=read.csv(file.choose(),stringsAsFactors = FALSE)
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
install.packages("gmodels")
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
cm<-table(sms_test_pred2, sms_test_labels)
acc=sum(diag(cm))/sum(cm)
acc

#------------------------------------------------------------------------------------------------------------------------
#case study
#prostate cancer
library(e1071)
library(caTools)
data <- read.csv(file.choose())  
data$diagnosis_result <- as.factor(data$diagnosis_result)
set.seed(10)
split <- sample.split(data$diagnosis_result, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)
nb_model <- naiveBayes(diagnosis_result ~ ., data = train_data)
predictions <- predict(nb_model, test_data)
conf_matrix <- table(test_data$diagnosis_result, predictions)
print(conf_matrix)
acc=sum(diag(conf_matrix))/sum(conf_matrix)
acc



