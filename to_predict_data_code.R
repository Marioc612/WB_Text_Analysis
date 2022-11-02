### Natural language processing for the WB data
## Developed by: SEI LA
## Date: 28/09/2022

#### Set the working directory
setwd('C:/Users/MarioCárdenas/OneDrive - SEI/Dokument/WB_NLP')

## Import first dataframe

dataset_original <- read.csv('Data_WB_2.csv', header = TRUE, sep = ';')
tail(dataset_original)

## Import the data set for the prediction--- the new one

dataset_to_predict <- read.csv('to_predict.csv', header = TRUE, sep = ';')
str(dataset_to_predict)
head(dataset_to_predict)

#View(dataset_to_predict)

# Combine the two datasets

dataset_original_F <- rbind(dataset_original, dataset_to_predict)

View(dataset_original_F)

####################################################################################################
### Second step

library(tm)
library(SnowballC)

# Create a Corpus in order to work with text data
###### Create a function to create the corpus and the DTM

corpus_dtm <- function(doc_term_mx) {
     corpus <- VCorpus(VectorSource(dataset_original_F$Text)) # The VectorSource is the column of the DataFrame from which we want to work with
     corpus <- tm_map(corpus, content_transformer(tolower)) # Lowercase all the textdata of out corpus
     corpus <- tm_map(corpus, removeNumbers) # If needed: remove numbers
     corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
     corpus <- tm_map(corpus, removeWords, stopwords(kind = 'en')) # Remove stop Words
     corpus <- tm_map(corpus, stemDocument) # Conduct the stemming process: to reduce a word to its root. Reading -> read, playing -> play
     corpus <- tm_map(corpus, stripWhitespace) # Eliminate multiple whitespaces
     dtm <- DocumentTermMatrix(corpus) # Create the 'Bag of Words' model
     return(dtm)
}

corpus_dtm()

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

############################################################################################
### Third step

# tranform the data into a dataframe
dataset <- as.data.frame(as.matrix(corpus_dtm()))
dataset$SDG <- dataset_original_F$SDG
str(dataset_original_F)

# Codify the variable to use as a factor
dataset$SDG <- factor(dataset$SDG)
levels(dataset$SDG)

### Fourth step
library(caTools)

set.seed(123)

# Divide the data between the training set and the validation set
x_train <- dataset[1:1357, ]
x_train

x_train$SDG <- factor(x_train$SDG)


x_test <- dataset[1358:1373, ]
x_test$SDG <- factor(x_test$SDG)

y_train <- dataset[1:1357, ]
y_train

#### The Model
library(randomForest)
## Adjust the random forest model to the training set
classifier <- randomForest(x = x_train[, -775],
                           y = x_train$SDG,
                           ntree = 101)

#Predict the results with the testing set
y_pred <- predict(classifier, newdata <- x_test[, -775])

y_pred
## Save the resutls
# create an empty character vector
my_predicted_results <- character()
my_predicted_results

# fill in my vector by creating a for-loop
results_to_map <- c()
for (i in y_pred) {results_to_map=c(results_to_map, i)}

# print the results
results_to_map

### subset of the data frame

datos_a_mapear <- dataset_original_F[c(1358: length(dataset_original_F$Project)), c(1:3)] 
View(datos_a_mapear)

datos_a_mapear["SDG"][datos_a_mapear["SDG"] == "A"] <- results_to_map
View(datos_a_mapear)
