### Natural language processing for the WB data
## Developed by: SEI LA
## Date: 28/09/2022

#### Set the working directory
setwd('C:/Users/MarioCárdenas/OneDrive - SEI/Dokument/WB_NLP')

## Import the data set
dataset_original <- read.csv('Data_WB_2.csv', header = TRUE, sep = ';')

# Display the first 6 rows of the dataset
head(dataset_original)
dim(dataset_original)
#View(dataset_original)

# Shuffle/permute the dataset row-wise
#dataset_original <- dataset_original[sample(1:nrow(dataset_original)),] # Randomly reorder the rows
#dataset_original # Print updated data

# Review the structure and type of the data
str(dataset_original)

## Clean the text data
#install.packages('tm')
#install.packages('SnowballC')
library(tm)
library(SnowballC)

# Create a Corpus in order to work with text data
# the VectorSource is the column of the DataFrame from which we want to work with
corpus <- VCorpus(VectorSource(dataset_original$Text))

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

### First ML Algrorithm
##### Random-Forest
#install.packages('randomForest')
library(randomForest)

# tranform the data into a dataframe
dataset <- as.data.frame(as.matrix(dtm))
dataset$SDG <- dataset_original$SDG

str(dataset_original)
# Codify the variable to use as a factor
dataset$SDG <- factor(dataset$SDG)
levels(dataset$SDG)

# Divide the data between the training set and the validation set
#install.packages('caTools')
library(caTools)

set.seed(123)
split <- sample.split(dataset$SDG, SplitRatio = 0.80)

training_set <- subset(dataset, split == TRUE)
testing_set <- subset(dataset, split == FALSE)


## Adjust the random forest model to the training set
classifier <- randomForest(x = training_set[, -772],
                           y = training_set$SDG,
                           ntree = 35)

## Predict the results with the testing set
y_pred <- predict(classifier, newdata <- testing_set[, -772])

## Aquí en este predict toca meterle en nuevo dataset


# Create the confussion matrix
cm <- table(testing_set[, 772], y_pred)
#View(cm)


#### A ver

results_summary <- function(resultados_modelo) {
     suma <- sum(cm)
     dimensiones <- dim(cm)
     precision <- sum(diag(cm)/suma)
     return(list(suma, dimensiones, precision))
}

results_summary()



## Confusion matrix results
multi_class_rates <- function(cm_results) {
     precision <- sum(diag(cm)) / sum(cm) 
     true_positives  <- diag(cm)
     false_positives <- colSums(cm) - true_positives
     false_negatives <- rowSums(cm) - true_positives
     true_negatives  <- sum(cm) - true_positives -
          false_positives - false_negatives
     return(data.frame(precision ,true_positives, false_positives, true_negatives,
                       false_negatives, row.names = names(true_positives)))
}

multi_class_rates()
