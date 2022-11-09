library(here)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(showtext)
source(here('DataReader.R'))
source(here('ViewResults.R'))
source(here('AnalysisModule.R'))

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
               low_lim = 0,
               up_lim=1,
               export_json=TRUE,
               version_name='Test')
# --------------------------- or Using previously pre-processed results
tidy <- from_saves('Test')

# Using test data
results <- generate_testData(tidy)

# Identifying the SDGs
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

initialise_fonts(dpi = 300)

occ <- occurrence_SDG %>% plot_results(
    title = 'Testing',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    fontsize_barlabs = 15,
    fontsize_title = 50,
    fontsize_axis = 30,
    scale = 1,
    dpi = 300)

export_plot(occ, 'a', dpi = 300)

matches <- matches_SDG %>% plot_results(
    title = 'Testing 2',
    xlabel ='SDG',
    ylabel ='Number of matches',
    # savefig = TRUE,
    # figname = 'Test 2',
    scale = 1)

main_SDG <- main_SDGs %>% plot_results(
    title = 'Testing main',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    # savefig = TRUE,
    # figname = 'Test 3',
    scale = 1)

histo <- results %>% plot_SDG_distribution(
    binwidth = 2,
    title = "Testing 3",
    test = TRUE)


# Network ======================================================================

# Generate network from results --------------------
net <- generate_network(results)

graph <- plot_network(results)
