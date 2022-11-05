preset_analysis <- function(analysis_mode = 'test',
                            data_from_saves = FALSE,
                            saved_data_name = NULL,
                            save_tidy_texts = TRUE,
                            tidy_texts_filename = 'tidy_texts',
                            export_summaries = FALSE) {
    cli_h1("SDG Mapper tool")

    cli_text(glue(
        "Welcome to the ", col_br_red("SDG Mapper Tool"), ", a tool developed ",
        "by the Stockholm Environment Institute for mapping the World Bank's ",
        "portfolio to the Sustainable Development Goals.\n\n",
        "Press ", col_br_green("ENTER"), " to continue"
    ))

    invisible(readline())

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
        testing_mode = FALSE
        cli_abort("This function has not been implemented yet!")

    } else if (analysis_mode == 'test') {
        cli_alert_warning("Running in test mode!")

        res <- generate_testData(tidy)
        testing_mode = TRUE
    } else {
        cli_abort(paste0("The argument 'analysis_mode' should be either ",
                         "'analysis' or 'test'"))
    }

    res <- identify_SDGs(res)


    # ====== Summaries =========================================================

    # Total matches by project
    matches_T <- count_matches(res,
                               by = 'total_matches',
                               sorted = 'Frequency',)

    # Total matches by SDG
    matches_SDG <- count_matches(res,
                                 by = 'SDG',
                                 sorted = 'Frequency',
                                 collapse_projects = TRUE)

    # Total matches by Target
    matches_tgt <- count_matches(res,
                                 by = 'Target',
                                 sorted = 'Frequency',
                                 collapse_projects = TRUE)

    # Binary occurrence results by SDG
    occurrence_SDG <- count_occurrence(res,
                                       by = 'SDG',
                                       collapse_projects = TRUE)

    # Total matches by Project and SDG
    matches_SDG_proj <- count_matches(res,
                                      by = 'SDG',
                                      sorted = 'Frequency',
                                      collapse_projects = FALSE)

    # Total matches by Project and Target
    matches_tgt_proj <- count_matches(res,
                                      by = 'Target',
                                      sorted = 'Frequency',
                                      collapse_projects = FALSE)

    # Binary occurrence results by Project and SDG
    occurrence_SDG_proj <- count_occurrence(res,
                                            by = 'SDG',
                                            collapse_projects = FALSE)

    # Aggregate of the main SDGs -- Number of times an SDG was identified as the
    # main SDG across the projects
    main_SDGs <- get_main_SDG(res,
                              from_binary = FALSE,
                              collapse_SDG = TRUE)

    # Aggregate of the main SDGs -- Number of times an SDG was identified as the
    # main SDG in each project
    main_SDGs_proj <- get_main_SDG(res,
                                   from_binary = FALSE,
                                   collapse_SDG = TRUE)

    # Number of SDGs identified in each project
    SDGs_proj <- get_SDGs_proj(res)

    # Results as a matrix ------------------------------------------------------

    matrix_relative <- results_matrix(res,
                                      relative_freqs = TRUE,
                                      with_main_SDG = TRUE)

    matrix_absolute <- results_matrix(res,
                                      relative_freqs = FALSE,
                                      with_main_SDG = TRUE)

    export_data_files <- prompt_export_summary()

    if ((export_data_files == 'y') | (export_data_files == 'Y')) {
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


    # ===== Prompting results to user ==========================================

    cli_h2("Results plotting")

    cli_text(glue(
        "Press ", col_br_green("ENTER"), " to start viewing the plots"
    ))

    invisible(readline())

    prompt_export_plot(occurrence_SDG,
                       title = 'Occurrence of the SDGs',
                       subtitle = '',
                       xlabel = 'SDG',
                       ylabel = 'Number of projects',
                       figname = 'occurrence')

    prompt_export_plot(matches_SDG,
                       title = "Matches of the SDGs",
                       subtitle = '',
                       xlabel = "SDG",
                       ylabel = "Number of matches",
                       figname = 'matches')

    prompt_export_plot(main_SDGs,
                       title = 'Testing main',
                       subtitle = '',
                       xlabel = 'SDG',
                       ylabel = 'Number of projects',
                       figname = 'main_SDG')

    prompt_export_histogram(res,
                            binwidth = 2,
                            title = "Distribution of the number of SDGs",
                            subtitle = "mapped by project",
                            figname = "SDGs_by_project",
                            test = testing_mode)

    prompt_export_graph(res,
                        figname = 'SDG_interactions')
}