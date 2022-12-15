import_dependencies <- function() {
    library(caTools)
    library(cli)
    library(DBI)
    library(ggplot2)
    library(ggraph)
    library(glue)
    library(here)
    library(igraph)
    library(jsonlite)
    library(pdftools)
    library(randomForest)
    library(readr)
    library(RSQLite)
    library(showtext)
    library(SnowballC)
    library(sysfonts)
    library(tidygraph)
    library(tidytext)
    library(tidyverse)
    library(tm)
    source(here('src', 'DataReader.R'))
    source(here('src', 'Mapper.R'))
    source(here('src', 'ViewResults.R'))
    source(here('src', 'AnalysisModule.R'))
}

mode_selector <- function() {
    cli({
        cli_h2("Analysis mode")
        cli_text("Please, select the agenda that you want to map ",
                 "your projects to: \n\n")
        cli_text("")

        cli_text(glue(col_green("1) "), "Sustainable Development Goals."))
        cli_text(glue(
            col_green("2) "),
            "European Taxonomy for Sustainable Activities."
        ))
    })

    analysis_mode <- invisible(readline())

    if (analysis_mode == 1) {
        analysis_mode <- 'SDGs'
    } else if (analysis_mode == 2) {
        analysis_mode <- 'EUT'
    } else {mode_selector()}
    return(analysis_mode)
}

run_mapper <- function() {
    import_dependencies()

    cli_h1("Sustainability Mapper Tool")
    cli_text("")

    cli_text(glue(
        "Welcome to the ", col_red("Sustainability Mapper Tool"), ", ",
        "a tool developed by the Stockholm Environment Institute for ",
        " mapping the World Bank's portfolio to the ",
        col_red("Sustainable Development Goals "),
        "and the ", col_red("European Taxonomy for Sustainability"), "."
    ))

    cli_text("")
    cli_text("Please, press {col_green('ENTER')} to continue")
    invisible(readline())

    cli_h2("Folder verification")
    cli_alert_info("\n\nVerifying folders")
    dir_checker()
    cli_alert_success("Folders checked\n\n")
    cli_text("")

    analysis_mode <- mode_selector()

    cli({
        cli_h2("Data source")
        cli_text("Please, select the source of the projects you want to ",
                 "analyze:")
        cli_text("")

        cli_text(glue(col_green("1) "), "Start analysis from scratch."))
        cli_text(glue(col_green("2) "), "Read saved data."
        ))
    })

    option <- as.character(invisible(readline()))


    # ====== Setting up text fonts =============================================
    initialise_fonts()


    # ===== Text pre-processing ================================================
    if (option == '1') {
        cli_text("")
        cli_text(glue(
            "Write the name of the folder that contains the PDF files and ",
            "press {col_green('ENTER')} to continue:\n\n"))

        name <- as.character(invisible(readline()))
        # Text extraction
        texts <- extract(name)

        # Text tidying
        tidy <- tidify(
            texts,
            token = 'sentences',
            low_lim = 0,
            up_lim = 1)

    } else if (option == '2') {
        # Retrieving previously saved data
        cli_text("")
        cli_text(glue(
        "Write the name of the saved file and press {col_green('ENTER')}",
        "to continue:"))

        name <- as.character(invisible(readline()))

        tidy <- from_saves(name)

    }

    # ===== Classification model ===============================================

    # ----- Data set-up for the classification model ----------
    # Connect to the training database
    conn <- dbConnect(RSQLite::SQLite(),
                      here("Settings", "training_data.db"))

    # Import the data used for training the model
    data_training <- dbGetQuery(conn, glue("SELECT * FROM {analysis_mode}"))

    dbDisconnect(conn)

    # Combine the training data set with the extracted texts
    data_complete <- rbind(data_training, tidy)

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

    # ----- Training the model -------------------------------------------------

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
    cli_alert_info(paste0("The model is being trained. Please wait, this ",
                          "process can take several minutes."))
    classifier <- randomForest(x = x_train[, -ncols_dtm],
                               y = x_train$Target,
                               ntree = 51)
    rm(x_train)
    cli_alert_success(paste0(
        "The model was trained successfully after ",
        difftime(time1 = Sys.time(), time2 = t0, units = "min"),
        "minutes."))

    # ----- Document classification --------------------------------------------

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

    # ====== Summaries =========================================================

    # Total matches by project
    matches_T <- count_matches(results,
                               by = 'total_matches',
                               sorted = 'Frequency',)

    # Total matches by SDG
    matches_SDG <- count_matches(results,
                                 by = 'SDG',
                                 sorted = 'Frequency',
                                 collapse_projects = TRUE)

    # Total matches by Target
    matches_tgt <- count_matches(results,
                                 by = 'Target',
                                 sorted = 'Frequency',
                                 collapse_projects = TRUE)

    # Binary occurrence results by SDG
    occurrence_SDG <- count_occurrence(results,
                                       by = 'SDG',
                                       collapse_projects = TRUE)

    # Total matches by Project and SDG
    matches_SDG_proj <- count_matches(results,
                                      by = 'SDG',
                                      sorted = 'Frequency',
                                      collapse_projects = FALSE)

    # Total matches by Project and Target
    matches_tgt_proj <- count_matches(results,
                                      by = 'Target',
                                      sorted = 'Frequency',
                                      collapse_projects = FALSE)

    # Binary occurrence results by Project and SDG
    occurrence_SDG_proj <- count_occurrence(results,
                                            by = 'SDG',
                                            collapse_projects = FALSE)

    # Aggregate of the main SDGs -- Number of times an SDG was identified as the
    # main SDG across the projects
    main_SDGs <- get_main_SDG(results,
                              from_binary = FALSE,
                              collapse_SDG = TRUE)

    # Aggregate of the main SDGs -- Number of times an SDG was identified as the
    # main SDG in each project
    main_SDGs_proj <- get_main_SDG(results,
                                   from_binary = FALSE,
                                   collapse_SDG = TRUE)

    # Number of SDGs identified in each project
    SDGs_proj <- get_SDGs_proj(results)

    # ----- Results as a matrix ------------------------------------------------

    matrix_relative <- results_matrix(results,
                                      relative_freqs = TRUE,
                                      with_main_SDG = TRUE)

    matrix_absolute <- results_matrix(results,
                                      relative_freqs = FALSE,
                                      with_main_SDG = TRUE)

    # ----- Export results -----------------------------------------------------

    export_data_files <- prompt_export_summary()

    if ((export_data_files == 'y') || (export_data_files == 'Y')) {
        export_summary(matches_T, 'matches_project_total', analysis_mode)
        export_summary(matches_SDG, 'matches_SDG_total', analysis_mode)
        export_summary(matches_tgt, 'matches_tgt_total', analysis_mode)
        export_summary(occurrence_SDG, 'occurrence_SDG_total', analysis_mode)
        export_summary(matches_SDG_proj, 'matches_SDG_proj', analysis_mode)
        export_summary(matches_tgt_proj, 'matches_tgt_proj', analysis_mode)
        export_summary(occurrence_SDG_proj,
                       'occurrence_SDG_proj',
                       analysis_mode)
        export_summary(main_SDGs, 'main_SDGs_total', analysis_mode)
        export_summary(main_SDGs_proj, 'main_SDGs_proj', analysis_mode)
        export_summary(SDGs_proj, 'SDGs_by_project', analysis_mode)
        export_summary(matrix_absolute, 'results_matrix_abs', analysis_mode)
        export_summary(matrix_relative, 'results_matrix_rel', analysis_mode)
    }

    # ===== Prompting results to user ==========================================

    cli_h2("Results plotting")

    cli_text(glue(
        "Press ", col_green("ENTER"), " to start viewing the plots"
    ))

    invisible(readline())

    prompt_export_plot(occurrence_SDG,
                       analysis_mode,
                       title = 'Occurrence of the SDGs',
                       subtitle = '',
                       xlabel = 'SDG',
                       ylabel = 'Number of projects',
                       figname = 'occurrence')

    prompt_export_plot(matches_SDG,
                       analysis_mode,
                       title = "Matches of the SDGs",
                       subtitle = '',
                       xlabel = "SDG",
                       ylabel = "Number of matches",
                       figname = 'matches')

    prompt_export_plot(main_SDGs,
                       analysis_mode,
                       title = 'Testing main',
                       subtitle = '',
                       xlabel = 'SDG',
                       ylabel = 'Number of projects',
                       figname = 'main_SDG')

    prompt_export_histogram(results,
                            analysis_mode,
                            binwidth = 2,
                            title = "Distribution of the number of SDGs",
                            subtitle = "mapped by project",
                            figname = "SDGs_by_project",
                            test = testing_mode)

    prompt_export_graph(results,
                        analysis_mode,
                        figname = 'SDG_interactions')
}