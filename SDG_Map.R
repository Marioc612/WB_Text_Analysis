library(here)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(remotes)
library(showtext)
source(here('DataReader.R'))
source(here('ViewResults.R'))
# source(here::here('DataManipulation.R'))


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
                        ylabel = 'Number of projects')

matches_SDG %>% plot_results(title='Testing 2',
                     xlabel='SDG',
                     ylabel='Number of matches')

main_SDGs %>% plot_results(title = 'Testing main',
                           xlabel = 'SDG',
                           ylabel = 'Number of projects')


plot_SDG_distribution(results,
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
