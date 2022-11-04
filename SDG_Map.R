library(here)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(showtext)
source(here('DataReader.R'))
source(here('ViewResults.R'))
# source(here::here('DataManipulation.R'))


preset_analysis <- function(analysis_mode = 'test',
                            data_from_saves = FALSE,
                            saved_data_name = NULL,
                            save_tidy_texts = TRUE,
                            tidy_texts_filename = 'tidy_texts',
                            export_summaries = FALSE,
                            export_plots = FALSE) {
    # ====== Setting up text fonts =============================================
    initialise_fonts()


    # ===== Text pre-processing ================================================
    if (data_from_saves == FALSE) {
        # Text extraction
        texts <- extract('Test')

        # Text tidying
        tidy <- tidify(
            texts,
            token = 'sentences',
            low_lim = 0,
            up_lim = 1,
            export_json = save_tidy_texts,
            json_name = tidy_texts_filename
        )

    } else if (data_from_saves == TRUE) {
        # Retrieving previously saved data
        if (isSingleString(saved_data_name)) {
            tidy <- from_saves(saved_data_name)
        } else {
            cli_abort(paste0("If the argument 'data_from_saves' is TRUE, ",
                             "the argument 'saved_data_name' should be a ",
                             "single string"))
        }
    }

    if (analysis_mode == 'analysis') {
        # ====== Machine learning model ========================================
        # Pending
        cli_abort("This function has not been implemented yet!")
        # Pending
        # ======================================================================
    } else if (analysis_mode == 'test') {
        results <- generate_testData(tidy)
    }

    results <- identify_SDGs(results)


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

    # Results as a matrix ------------------------------------------------------

    matrix_relative <- results_matrix(results,
                                      relative_freqs = TRUE,
                                      with_main_SDG = TRUE)

    matrix_absolute <- results_matrix(results,
                                      relative_freqs = FALSE,
                                      with_main_SDG = TRUE)

    if (export_summaries == TRUE) {
        export_summary(matches_T, 'matches_project_total')
        export_summary(matches_SDG, 'matches_SDG_total')
        export_summary(matches_tgt, 'matches_tgt_total')
        export_summary(occurrence_SDG, 'occurrence_SDG_total')
        export_summary(matches_SDG_proj, 'matches_SDG_proj')
        export_summary(matches_tgt_proj, 'matches_tgt_proj')
        export_summary(occurrence_SDG_proj, 'occurrence_SDG_proj')
        export_summary(main_SDGs, 'main_SDGs_total')
        export_summary(main_SDGs_proj, 'main_SDGs_proj')
        export_summary(SDGs_proj, 'SDGs_by_project')
        export_summary(matrix_absolute, 'results_matrix_abs')
        export_summary(matrix_relative, 'results_matrix_rel')
    }


    # Plotting =================================================================

    occ <- occurrence_SDG %>% plot_results(
        title = 'Occurrence of the SDGs',
        xlabel = 'SDG',
        ylabel = 'Number of projects',
        savefig = export_plots,
        figname = 'occurrence',
        scale = 1
        )

    mat <- matches_SDG %>% plot_results(
        title = "Matches of the SDGs",
        subtitle = '',
        xlabel = "SDG",
        ylabel = "Number of matches",
        savefig = export_plots,
        figname = 'matches',
        scale = 1
        )

    mains <- main_SDGs %>% plot_results(
        title = 'Testing main',
        xlabel = 'SDG',
        ylabel = 'Number of projects',
        savefig = export_plots,
        figname = 'main_SDG',
        scale = 1
        )

    dis <- results %>% plot_SDG_distribution(
        binwidth = 2,
        title = "Distribution of the number of SDGs",
        subtitle = "mapped by project",
        savefig = export_plots,
        figname = "SDGs_by_project",
        test = TRUE
        )
}



# ====== Setting up text fonts =================================================
initialise_fonts()
# ------------------------------------------------------------------------------


# ====== Loading the saved data instead of processing it again =================
# texts <- extract('Test')
#
# tidy <- tidify(texts, token='sentences', low_lim = 0, up_lim=1,
#                export_json=TRUE, json_name='Test')

tidy <- from_saves('Test') # --------------------------- Using the saved results

results <- generate_testData(tidy)

results <- identify_SDGs(results)


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


# Identify the main SDG in each project. From binary == TRUE identifies the main
# SDG with the sum of the binary frequencies (presence or absence of a target)
# of all targets in a SDG.
# -------------------------------> can feed a column plot and be exported to csv
main_SDGs <- get_main_SDG(results,
                          from_binary = FALSE,
                          collapse_SDG = TRUE)

SDGs_proj <- get_SDGs_proj(results)


# Plotting =====================================================================

occurrence_SDG %>% plot_results(title = 'Testing',
                        xlabel = 'SDG',
                        ylabel = 'Number of projects',
                        # savefig = TRUE,
                        # figname = 'Test 1',
                        scale = 1)

matches_SDG %>% plot_results(title='Testing 2',
                     xlabel='SDG',
                     ylabel='Number of matches',
                     # savefig = TRUE,
                     # figname = 'Test 2',
                     scale = 1)

main_SDGs %>% plot_results(title = 'Testing main',
                           xlabel = 'SDG',
                           ylabel = 'Number of projects',
                           # savefig = TRUE,
                           # figname = 'Test 3',
                           scale = 1)


results %>% plot_SDG_distribution(
    binwidth = 2,
    title = "Testing 3",
    test = TRUE)


# Results as matrix ============================================================

matrix_relative <- results_matrix(results,
                                  relative_freqs = TRUE,
                                  with_main_SDG = TRUE)

matrix_absolute <- results_matrix(results,
                                  relative_freqs = FALSE,
                                  with_main_SDG = TRUE)

# Network ======================================================================

# Generate network from results --------------------
net <- generate_network(results)

plot_network(results, savefig = TRUE, figname = 'Network test')
