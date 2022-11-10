library(here)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(tm)
library(SnowballC)
library(caTools)
library(randomForest)
library(showtext)
source(here('src', 'DataReader.R'))
source(here('src', 'Mapper.R'))
source(here('src', 'ViewResults.R'))
source(here('src', 'AnalysisModule.R'))

preset_analysis(analysis_mode = 'test',
                save_tidy_texts = FALSE,
                tidy_texts_filename = 'Test',
                export_summaries = TRUE)


# ====== Setting up text fonts =================================================
initialise_fonts()
# ------------------------------------------------------------------------------


# ====== Loading the saved data instead of processing it again =================
texts <- extract('Test')

# You can tidy the extracted texts
tidy <- tidify(texts,
               token='sentences',
               low_lim = 0.65,
               up_lim = 0.7,
               export_json = TRUE,
               version_name = 'Test')
# --------------------------- or Using previously pre-processed results
tidy <- from_saves('Test')
results <- tidy %>%
    mutate(Target = 'A')

# Using test data
results <- generate_testData(tidy)
results <- identify_SDGs(results)

# ===== Machine learning model =================================================

# Import the data used for training the model
data_training <- read.csv(here('Settings/Training_data.csv'),
                          header = TRUE,
                          sep = ',')

# Combine the training data set with the extracted texts
data_complete <- rbind(data_training,
                       results)

data_complete <- data_complete %>%
    mutate(Text = str_replace_all(data_complete$Text, '[^A-Za-z ]', ''))

# Capturing the data sets' length
nrows_data_t <- nrow(data_training)
nrows_data_c <- nrow(data_complete)

rm(data_training)

# Create a Document Term Matrix (DTM)
dtm <- corpus_dtm(data_complete)

# I think this is unnecessary, as *dataset_DF()* is executed in *codify()*
# # Transform the data set
# data_set_to_work <- dataset_DF(dtm, data_complete)

t0 <- Sys.time()
working_dataset <- codify(dtm, data_complete)
tf <- Sys.time()
print(tf - t0)

# We don't need *dtm* anymore, so we remove it from the environment
rm(dtm)

# Create the model and split the data in train and test
set.seed(612)

# Select training data
x_train <- working_dataset[1:nrows_data_t,]
x_train$Target <- factor(x_train$Target)

# Select test data
x_test <- working_dataset[nrows_data_t:nrows_data_c,]
x_test <- x_test %>% slice(-1)
x_test$Target <- factor(x_test$Target)

rm(working_dataset)

ncols_dtm <- dim(x_train)[2]

# Train the random forest model using the training data set
t0 <- Sys.time()
classifier <- randomForest(x = x_train[, -ncols_dtm],
                           y = x_train$Target,
                           ntree = 101)
rm(x_train)
tf <- Sys.time()
print(tf - t0)


# Classify the input data -- Map the projects to the SDGs
t0 <- Sys.time()
y_pred <- predict(classifier, newdata = x_test[, -ncols_dtm])
rm(x_test)
tf <- Sys.time()
print(tf - t0)

# Save the results
classified <- character()
for (goal in y_pred) {classified = c(classified, goal)}

# Subset the data frame and paste the classified data
results <- data_complete %>%
    slice(nrows_data_t+1: nrows_data_c)

rm(data_complete)

results$Target <- classified

results <- identify_SDGs(results)

results <- results %>% dplyr::filter(SDG %in% glue("SDG {1:17}"))

results <- as_tibble(results)

# ====== Summaries =============================================================

# Total matches by project -------------------------------> can feed a histogram
matches_T <- count_matches(results,
                           by = 'total_matches',
                           sorted = 'Frequency')

# Total matches by Project and SDG ---------------------> can feed a column plot
matches_SDG <- count_matches(results,
                             by = 'SDG',
                             sorted = 'Frequency',
                             collapse_projects = TRUE)

# Total matches by Project and Target ------------------> can be exported as csv
matches_tgt <- count_matches(results,
                             by = 'Target',
                             sorted = 'Frequency',
                             collapse_projects = TRUE)

# Binary occurrence results by SDG ---------------------> can be exported as csv
occurrence_SDG <- count_occurrence(results,
                                   by = 'SDG',
                                   collapse_projects = TRUE)

count_occurrence(results, by = 'SDG', collapse_projects = FALSE)


# Identify the main SDG in each project. From binary == TRUE identifies the main
# SDG with the sum of the binary frequencies (presence or absence of a target)
# of all targets in a SDG.
# -------------------------------> can feed a column plot and be exported to csv
main_SDGs <- get_main_SDG(results,
                          from_binary = FALSE,
                          collapse_SDG = FALSE)

SDGs_proj <- get_SDGs_proj(results)


# Results as matrix ------------------------------------------------------------

matrix_relative <- results_matrix(results,
                                  relative_freqs = TRUE,
                                  with_main_SDG = TRUE)

matrix_absolute <- results_matrix(results,
                                  relative_freqs = FALSE,
                                  with_main_SDG = TRUE)


# Plotting =====================================================================

initialise_fonts()

occ <- occurrence_SDG %>% plot_results(
    title = 'Testing',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    fontsize_barlabs = 7,
  #  fontsize_title = 50,
   # fontsize_axis = 30,
    scale = 1,
    dpi = 96)

export_plot(occ, 'a', dpi = 300)

matches <- matches_SDG %>% plot_results(
    title = 'Testing 2',
    xlabel ='SDG',
    ylabel ='Number of matches',fontsize_barlabs = 5,
    scale = 2)

main_SDG <- main_SDGs %>% plot_results(
    title = 'Main SDG',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    scale = 1)

histo <- results %>% plot_SDG_distribution(
    binwidth = 2,
    title = "Testing 3",
    test = TRUE)


# Network ======================================================================

# Generate network from results --------------------
net <- generate_network(results)

graph <- plot_network(results)
