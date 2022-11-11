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



# ====== Loading the saved data instead of processing it again =================
texts <- extract('PADs')

# You can tidy the extracted texts
tidy <- tidify(texts,
               token='sentences',
               low_lim = 0.65,
               up_lim = 0.7,
               export_json = TRUE,
               version_name = 'PADs')
# --------------------------- or Using previously pre-processed results
tidy <- from_saves('PADs')


# ===== Machine learning model =================================================

# ----- Data set-up for the classification model ----------

# Import the data used for training the model
data_training <- read.csv(here('Settings/Training_data.csv'),
                          header = TRUE,
                          sep = ',')

# Combine the training data set with the extracted texts
data_complete <- rbind(data_training,
                       tidy)

data_complete <- data_complete %>%
    mutate(Text = str_replace_all(data_complete$Text, '[^A-Za-z ]', ''))

# Capturing the data sets' length
nrows_data_t <- nrow(data_training)
nrows_data_c <- nrow(data_complete)

rm(data_training)

# Create a Document Term Matrix (DTM)
dtm <- corpus_dtm(data_complete)

# Create the working data set
working_dataset <- codify(dtm, data_complete)
rm(dtm)


# ----- Create the random forest model -----------

# Set random seed
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
cli_alert_info(paste0("The model is being trained. Please wait, this",
                      "process can take several minutes."))
classifier <- randomForest(x = x_train[, -ncols_dtm],
                           y = x_train$Target,
                           ntree = 51)
rm(x_train)
cli_alert_success(paste0(
    "The model was trained successfully after",
    difftime(time1 = t0, time2 = Sys.time(), units = "min"), " minutes"))

# Classify the input data -- Map the projects to the SDGs
y_pred <- predict(classifier, newdata = x_test[, -ncols_dtm])
rm(x_test)

# Save the results in a character vector
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
                          collapse_SDG = TRUE)

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

occurrence_SDG %>% plot_results(
    title = 'Testing',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    fontsize_barlabs = 7,
  #  fontsize_title = 50,
   # fontsize_axis = 30,
    scale = 1,
    dpi = 96)

matches_SDG %>% plot_results(
    title = 'Matches of the SDGs',
    subtitle = '(Number of times each SDG was mapped)',
    xlabel ='SDG',
    ylabel ='Number of matches',
    fontsize_barlabs = 5,
    scale = 2)

main_SDGs %>% plot_results(
    title = 'Predominant SDGs',
    subtitle = 'Across the portfolio',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    scale = 1)

results %>% plot_SDG_distribution(
    binwidth = 2,
    title = "Distribution of the SDGs",
    subtitle = "By the number of projects they map to",
    test = FALSE)


# Network ======================================================================

# Generate network from results --------------------
net <- generate_network(results)

graph <- plot_network(results,
                      fontsize_base = 23,
                      fontsize_title = 40,
                      fontsize_subt = 30,
                      dpi = 96,
                      scale = 1)

graph
