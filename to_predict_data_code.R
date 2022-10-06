### Natural language processing for the WB data
## Developed by: SEI LA
## Date: 28/09/2022

#### Set the working directory
setwd('C:/Users/MarioCárdenas/OneDrive - SEI/Dokument/WB_NLP')

## Import first dataframe

dataset_original <- read.csv('Data_WB_2.csv', header = TRUE, sep = ';')

## Import the data set for the prediction--- the new one

dataset_to_predict <- read.csv('to_predict.csv', header = TRUE, sep = ';')
str(dataset_to_predict)

#View(dataset_to_predict)

# Combine the two datasets

dataset_original_F <- rbind(dataset_original, dataset_to_predict)

View(dataset_original_F)

### Second step

# Create a Corpus in order to work with text data
# the VectorSource is the column of the DataFrame from which we want to work with
corpus <- VCorpus(VectorSource(dataset_original_F$Text))

# lowercase all the textdata of out corpus
corpus <- tm_map(corpus, content_transformer(tolower))

# If needed: remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove stopWords
corpus <- tm_map(corpus, removeWords, stopwords(kind = 'en'))

# Coduct the stemming process: to reduce a word to its root. Reading -> read, playing -> play
corpus <- tm_map(corpus, stemDocument)

# Eliminate multiple whitespaces
corpus <- tm_map(corpus, stripWhitespace)

## Create the 'Bag of Words' model
dtm <- DocumentTermMatrix(corpus)

### Third step

# tranform the data into a dataframe
dataset <- as.data.frame(as.matrix(dtm))
dataset$SDG <- dataset_original_F$SDG
str(dataset_original_F)

# Codify the variable to use as a factor
dataset$SDG <- factor(dataset$SDG)
levels(dataset$SDG)

### Fourth step
library(caTools)

set.seed(123)

# Divide the data between the training set and the validation set
x_train <- dataset[1:1387, ]
x_train

x_train$SDG <- factor(x_train$SDG)


x_test <- dataset[1388:1403, ]
x_test$SDG <- factor(x_test$SDG)

y_train <- dataset[1:1387, ]
y_train

#### The Model



## Adjust the random forest model to the training set
classifier <- randomForest(x = x_train[, -772],
                           y = x_train$SDG,
                           ntree = 35)


## Predict the results with the testing set
y_pred <- predict(classifier, newdata <- x_test[, -772])


View(y_pred)

# Create the confussion matrix
cm <- table(x_test[, 772], y_pred)
View(cm)



