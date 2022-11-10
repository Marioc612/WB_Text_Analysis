library(tm)
library(SnowballC)
library(caTools)
library(randomForest)

# Create a Document Term Matrix (DFM)
corpus_dtm <- function(complete_dataset) {
    # The VectorSource is the column of the dataset from which we want to
    # work with
    corpus <- VCorpus(VectorSource(complete_dataset$Text))

    # Remove stop words
    stopwords <- as.character(
        read.csv(here('Settings/stop_words.csv'), head = FALSE)$V1)
    stopwords <- c(stopwords, stopwords())
    corpus <- tm_map(corpus, removeWords, stopwords)

    # Lowercase all the textdata of out corpus
    corpus <- tm_map(corpus, content_transformer(tolower))
    # If needed: remove numbers
    corpus <- tm_map(corpus, removeNumbers)
    # Remove punctuation
    corpus <- tm_map(corpus, removePunctuation)
    # Remove stop Words
    corpus <- tm_map(corpus, removeWords, stopwords(kind = 'en'))
    # Conduct the stemming process: to reduce a word to its root.
    # Reading -> read, playing -> play
    corpus <- tm_map(corpus, stemDocument)
    # Eliminate multiple white spaces
    corpus <- tm_map(corpus, stripWhitespace)
    # Create the 'Bag of Words' model

    dtm <- DocumentTermMatrix(corpus)
    return(dtm)
}

# Transform the data into a dataframe and codify the SDGs as factors
dataset_DF <- function(dtm_data, complete_dataset) {
    dataset <- as.data.frame(as.matrix(dtm_data))
    dataset$Target <- complete_dataset$Target
    #str(complete_dataset)
    return(dataset)
}

# Codify SDG as factors
codify <- function(dtm_data, complete_dataset){
    start <- Sys.time()
    # Create working data set
    data_set_to_work <- dataset_DF(dtm_data, complete_dataset)

    # Codify the variable to use as a factor
    data_set_to_work$Target <- factor(data_set_to_work$Target)

    # Review the levels/factors: SDG targets
    levels(data_set_to_work$SDG)

    end <- Sys.time()
    print(end-start)
    return(data_set_to_work)
}

