library(tm)
library(SnowballC)
library(caTools)
library(randomForest)

# Create a Document Term Matrix (DFM)
corpus_dtm <- function(complete_dataset) {
    cli_h2("The corpus is being created")
    # The VectorSource is the column of the dataset from which we want to
    # work with
    cli_progress_bar("Creating corpus")
    corpus <- VCorpus(VectorSource(complete_dataset$Text))

    # Remove stop words
    cli_progress_update()
    stopwords <- as.character(
        read.csv(here('Settings/stop_words.csv'), head = FALSE)$V1)
    stopwords <- unique(c(stopwords, stopwords()))

    # Lowercase all the textdata of out corpus
    cli_progress_update()
    corpus <- tm_map(corpus, content_transformer(tolower))
    # If needed: remove numbers
    cli_progress_update()
    corpus <- tm_map(corpus, removeNumbers)
    # Remove punctuation
    cli_progress_update()
    corpus <- tm_map(corpus, removePunctuation)
    # Remove stop Words
    cli_progress_update()
    corpus <- tm_map(corpus, removeWords, stopwords(kind = 'en'))
    corpus <- tm_map(corpus, removeWords, stopwords)
    # Conduct the stemming process: to reduce a word to its root.
    # Reading -> read, playing -> play
    cli_progress_update()
    corpus <- tm_map(corpus, stemDocument)
    # Eliminate multiple white spaces
    cli_progress_update()
    corpus <- tm_map(corpus, stripWhitespace)
    # Create the 'Bag of Words' model
    cli_progress_update()
    dtm <- DocumentTermMatrix(corpus)
    cli_progress_done()
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
    cli_text("")
    return(data_set_to_work)
}

