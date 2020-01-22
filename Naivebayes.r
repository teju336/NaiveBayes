#importing the CSV data and saving it in a data frame:
sms_data <- read.csv("~/Downloads/sms_spam.csv",stringsAsFactors = FALSE)

str(sms_data)
#Since this is a categorical variable, it would be better to convert it into a factor
sms_data$type <- factor(sms_data$type)
#Examining this with the str() and table() functions
str(sms_data$type)
table(sms_data$type)
#Install Package Tm to simplify the data 
install.packages("tm")
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_data$text))
print(sms_corpus)
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)
sms_corpus_clean <- tm_map(sms_corpus,
                           content_transformer(tolower))
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,
                           removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
install.packages("SnowballC")
library(SnowballC)
wordStem(c("learn", "learned", "learning", "learns"))
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))
sms_dtm
sms_dtm2
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]
sms_train_labels <- sms_data[1:4169, ]$type
sms_test_labels  <- sms_data[4170:5559, ]$type
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(sms_data, type == "spam")
ham <- subset(sms_data, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
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
#training a model on the data
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
#evaluating model performance
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
#improving model performance
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
confusionMatrix(sms_test_pred2,sms_test_labels)
