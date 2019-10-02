# Span Detector using Natural Language Processing

# importing libraries

# loading libraries
library(ggplot2)
library(tm)
library(SnowballC)
library(caret)
library(randomForest)
library(e1071)
library(wordcloud)
library(RColorBrewer)

# ----
#   GATHERING THE DATA
# ----

# importing datasets
our.data = read.csv("input/spam.csv", encoding = "latin1", stringsAsFactors = F, header = T)
head(our.data)
str(our.data)


# ----
#  PREPROCESSING THE DATA
# ---- 
# Convert blank values to null 
our.data$X = ifelse(our.data$X == '', NA, our.data$X)
our.data$X.1 = ifelse(our.data$X.1 == '', NA, our.data$X.1)
our.data$X.2 = ifelse(our.data$X.2 == '', NA, our.data$X.2)

# Check total numbers of values present in column X, X.1 and X.2
sum(!is.na(our.data$X))
sum(!is.na(our.data$X.1))

# Removing column X, X.1 and X.2 from our data set as mostly it contains null values
our.data = our.data[, 1:2]

# Renaming the columns to make it more readable
names(our.data) = c("label", "message")

# Change 'ham' as 0 and 'spam' as 1 (1 is our positive class)
our.data$label = ifelse(our.data$label == 'ham', 0, 1)

# ----
# DATA EXPLORATION
# ----

# Cleaning the text
corpus = VCorpus(VectorSource(x = our.data$message))

# converting all uppercase letter to lower case
corpus = tm_map(x = corpus, content_transformer(tolower))

# removing all numbers
corpus = tm_map(x = corpus, removeNumbers)

# removing all puntucations
corpus = tm_map(x = corpus, removePunctuation)

# removing all not usefull words
corpus = tm_map(x = corpus, removeWords, stopwords('english'))

# change the word to its root word - STEMING
corpus = tm_map(x = corpus, stemDocument)

# remove extra space
corpus = tm_map(x = corpus, stripWhitespace)

# creating a Bag Of Word model
dtm = DocumentTermMatrix(corpus)

dtm = removeSparseTerms(dtm, 0.999)
dim(dtm)

inspect(dtm[40:50, 10:15])

# Converting the word frequencies to YES NO labels
convert_count = function(x){
  y = ifelse(x>0, 1, 0)
  y = factor(y, levels = c(0, 1), labels = c('No', 'Yes'))
  y
}

our.data.mod = apply(dtm, 2, convert_count)
our.data.mod = as.data.frame(as.matrix(our.data.mod))

# Exploratory analysis of data
freq = sort(colSums(as.matrix(dtm)), decreasing = T)
tail(freq, 10)

findFreqTerms(dtm, lowfreq = 60)
wf = data.frame(words = names(freq), freq = freq)

# add Spam variable in our.data.mod
our.data.mod$spam = as.factor(our.data$label)

# Splitting the data into training and testing data set
indexes = createDataPartition(y = our.data.mod$spam, p = 0.8, list = FALSE)
trd = our.data.mod[indexes, ]
tsd = our.data.mod[-indexes, ]

#
# Save workspace
#

# Creating a random forest classifier
rf_classifier = randomForest(x = trd[-1206],
                             y = trd$spam,
                             ntree = 300)

rf_classifier

# Prediction using random forest classifier
rf_pred = predict(rf_classifier, type = 'class', newdata = tsd[-1206])
confusionMatrix(rf_pred,tsd$spam, positive = '1')

# creating a classifier
control <- trainControl(method="repeatedcv", number=10, repeats=3)
system.time( classifier_nb <- naiveBayes(trd[-1206], trd$spam, laplace = 1,
                                         trControl = control,tuneLength = 7) )

# Prediction using naive bayes classifier
nb_pred = predict(classifier_nb, type = 'class', newdata = tsd[-1206])

confusionMatrix(nb_pred,tsd$spam, positive = '1')


