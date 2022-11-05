library(tidyverse)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(igraph)
library(showtext)
library(sysfonts)


initialise_fonts <- function(font = "Roboto Condensed", dpi = 96) {
    sysfonts::font_add_google(font)
    showtext_opts(dpi)
    showtext_auto(enable = TRUE)
}


generate_color_palette <- function(all_SDGs = TRUE, SDG_vector = NULL) {
    palette <- as_tibble(read.csv(here('Settings/SDG_colors.csv')))
    if (all_SDGs == FALSE) {
        if (!is.null(SDG_vector)) {
            palette <- as_tibble(read.csv(here('Settings/SDG_colors.csv')))
            palette <- palette %>% column_to_rownames("SDG")
            palette <- palette[SDG_vector, ]
        } else {
            cli_abort(paste0("The argument 'SDG_vector' must be a vector of ",
                             "SDGs -- e.g., c('SDG 1', 'SDG 3', 'SDG n') -- ",
                             "if 'all_SDGs' is set to FALSE"))
        }
    }
    return(palette)
}


generate_testData <- function(tidy_texts) {
    SDGs <- as_tibble(read_csv(here('Settings/SDGs.csv'), col_types = 'cc'))
    test_results <- tidy_texts

    test_results <- test_results %>%
        mutate(
            Target = sample(
                SDGs$Target,
                size = nrow(test_results),
                replace = TRUE,
                prob = SDGs$Probability)
        )
    return(test_results)
}


identify_SDGs <- function(mapping_res,
                          drop_text = TRUE,
                          save = FALSE,
                          version_name = NULL) {
    mapping_res <- mapping_res %>%
        separate(Target, c('SDG', 'Target'), sep = '[.]') %>%
        mutate(Target = paste0(SDG, '.', Target))

    if (save == TRUE) {
        if (isSingleString(version_name)) {
            write.csv(mapping_res,
                      here(glue('Saves/clean-results_{version_name}.csv')),
                      row.names = FALSE)
        } else {
            warning('version_name should be a string')
        }
    }
    return(mapping_res)
}


color_by_SDG <- function(mapping_res) {
    SDG_colors <- as_tibble(read_csv(here('Settings/SDG_colors.csv'),
                                     col_types = 'cc'))

    mapping_res <- mapping_res %>%
        inner_join(SDG_colors, by = 'SDG')
}


count_matches <- function(mapping_res,
                          by,
                          sorted=NULL,
                          collapse_projects = FALSE) {
    if (isSingleString(by)) {
        if (any(by == 'total_matches')) {
            matches <- mapping_res %>%
                group_by(Project) %>%
                summarise(Frequency = n(), .groups = 'drop')
        } else {
            matches <- mapping_res %>%
                group_by(Project, mapping_res[, by]) %>%
                summarise(Frequency = n(), .groups = 'drop')

            if (by == 'Target') {
                matches <- identify_SDGs(matches)
            }
        }
    }

    matches <- as_tibble(matches)

    if (!is.null(sorted)) {
        sorting_ref <- as.list(matches[, sorted])[[1]]

        if (by != 'total_matches') {
            if (is.numeric(sorting_ref)) {
                matches <- matches %>%
                    arrange(Project, -sorting_ref)
            } else if (is.character(sorting_ref)) {
                sorting_ref <- str_sort(sorting_ref, numeric = TRUE)

                matches <- matches %>%
                    arrange(Project, sorting_ref)
            } else {
                warning(paste0("The argument 'sorted' must be either reference",
                               " a column of integers or strings"))
            }
        } else {
            matches <- matches %>%
                arrange(-sorting_ref)
        }
    }

    if (collapse_projects == TRUE &
        by != 'total_matches') {
        if (by == 'Target') {
            matches <- matches %>%
                group_by(SDG, Target) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        } else {
            matches <- matches %>%
                group_by(SDG) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        }

    } else if (collapse_projects == TRUE & by == 'total_matches') {
        cli_abort(paste0("It is not possible to collapse the results by ",
                         "Project when argument 'by' == 'total_matches', ",
                         "only when 'by' == 'SDG' or 'Target'"))
    }

    matches <- as_tibble(matches)

    return(matches)
}


count_occurrence <- function(mapping_res,
                             by = 'Target',
                             collapse_projects = FALSE) {
    if (by %in% c('SDG', 'Target')) {
        occurrence <- mapping_res %>%
            count_matches(by = 'Target',
                          sorted = 'Frequency') %>%
            mutate(Frequency = 1) %>%
            color_by_SDG()
    } else {cli_abort("Argument 'by' must be either 'SDG' or 'Target'")}

    if (by == 'SDG') {
        occurrence <- occurrence %>%
            group_by(Project, SDG) %>%
            summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
            mutate(Frequency = 1) %>%
            arrange(Project, str_sort(SDG, numeric = TRUE)) %>%
            color_by_SDG()
    }

    if (collapse_projects == 'TRUE') {
        if (by == 'SDG') {
            occurrence <- occurrence %>%
                group_by(SDG) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        }
        if (by == 'Target') {
            occurrence <- occurrence %>%
                group_by(SDG, Target) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        }
    }
    return(as_tibble(occurrence))
}

get_main_SDG <- function(mapping_res,
                         from_binary = TRUE,
                         collapse_SDG = FALSE) {
    if (from_binary == TRUE) {
        mapping_res <- mapping_res %>%
            count_occurrence(by = 'SDG')

    } else if (from_binary == FALSE) {
        mapping_res <- mapping_res %>%
            count_matches(by = 'SDG', sorted = 'Frequency') %>%
            color_by_SDG()
    } else {cli_abort("Argument 'from_binary' must be either TRUE or FALSE")}

    main <- mapping_res %>%
        group_by(Project) %>%
        top_n(1, Frequency) %>%
        mutate(Frequency = 1)

    if (collapse_SDG == TRUE) {
        main <- main %>%
            group_by(SDG) %>%
            summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
            color_by_SDG()
    }

    return(main)
}


get_SDGs_proj <- function(mapping_res) {
    SDGs_project <- count_occurrence(mapping_res,
                                     by = 'SDG',
                                     collapse_projects = FALSE)

    SDGs_project <- SDGs_project %>%
        mutate(Frequency = 1) %>%
        group_by(Project) %>%
        summarise(Frequency = sum(Frequency), .groups = 'drop')
    return(SDGs_project)
}


results_matrix <- function(mapping_res,
                           relative_freqs = FALSE,
                           with_main_SDG = TRUE) {
    matrix_results <- mapping_res %>%
        count_matches(by = 'SDG',
                      sorted = 'Frequency',
                      collapse_projects = FALSE) %>%
        dplyr::mutate(Color = NULL)

    matrix_results <- pivot_wider(matrix_results,
                                  names_from = 'SDG',
                                  values_from = 'Frequency')

    matrix_results <- matrix_results %>%
        dplyr::select(str_sort(colnames(matrix_results), numeric = TRUE))

    matrix_results <- matrix_results %>%
        replace(is.na(.), 0)

    if (relative_freqs == TRUE) {
        projects <- as.list(matrix_results %>% select(Project))[[1]]

        matrix_results <- matrix_results %>%
            select(-Project)

        for (i in 1:nrow(matrix_results)) {
            matrix_results[i, ] <-
                round(100 * matrix_results[i, ] / sum(matrix_results[i, ]), 0)
        }

        matrix_results <- matrix_results %>%
            add_column(Project = projects, .before = 1, ) %>%
            add_column(Unit = rep('%', nrow(matrix_results)), .after = 1)
    } else if (relative_freqs == FALSE) {
        matrix_results <- matrix_results %>%
            add_column(Unit = rep('# matches', nrow(matrix_results)),
                       .after = 1)
    } else {
        cli_alert_warning(paste0("The parameter 'relative_freqs' should be ",
                                 "either TRUE or FALSE. By default, the ",
                                 "any other value will return an absolute ",
                                 "frequencies matrix"))
    }

    if (with_main_SDG == TRUE) {
        main_goals <- get_main_SDG(mapping_res,
                                   from_binary = FALSE,
                                   collapse_SDG = FALSE)

        matrix_results <- matrix_results %>%
            dplyr::mutate(main_SDG = main_goals$SDG)
    }
    return(matrix_results)
}


generate_network <- function(mapping_res) {
    # Count SDGs
    results <- mapping_res %>%
        count_occurrence('SDG') %>%
        mutate(Color = NULL)

    # Iterate through documents
    tibblist <- list()
    for (project in unique(results$Project)) {
        a <- results %>% filter(Project == project)
        a <- as.list(a$SDG)

        a <- combn(a, 2, simplify = FALSE)

        sources <- list()
        targets <- list()
        for (i in 1:length(a)) {
            sources <- c(sources, a[[i]][1])
            targets <- c(targets, a[[i]][2])
        }

        net <- tibble(source = sources,
                      target = targets)

        tibblist <- c(tibblist, list(net))
    }

    # Concatenates the list of tibble into a single tibble
    tibblist <- as_tibble(data.table::rbindlist(tibblist))

    net <- tibblist %>%
        mutate(source = as.numeric(str_remove_all(source, "[^0-9]")),
               target = as.numeric(str_remove_all(target, "[^0-9]")))

    min <- list()
    max <- list()
    for (i in 1:nrow(net)) {
        if (net[[i, 1]] > net[[i, 2]]) {
            max <- c(max, net[[i, 1]])
            min <- c(min, net[[i, 2]])
        } else if (net[[i, 1]] < net[[i, 2]]) {
            max <- c(max, net[[i, 2]])
            min <- c(min, net[[i, 1]])
        } else {cli_abort("Something is wrong")}
    }

    net <- tibble(source = min, target = max)

    net <- net %>%
        mutate(source = as.numeric(source),
               target = as.numeric(target)) %>%
        arrange(source, target) %>%
        group_by(source, target) %>%
        summarise(weight = n(), .groups = 'drop') %>%
        mutate(source = glue("SDG {source}"),
               target = glue("SDG {target}"))

    nodes <- c(as.list(net$source), as.list(net$target))
    nodes <- str_sort(unique(nodes), numeric = TRUE)

    net <- graph_from_data_frame(net, directed = FALSE, vertices = nodes)

    V(net)$degree <- strength(net, mode='total')
    V(net)$color <- generate_color_palette(all_SDGs = FALSE, SDG_vector = V(net))
    E(net)$color <- "gray"

    return(net)
}


# ===== Plotting functions =====================================================

plot_results <- function(data,
                         title = NULL,
                         subtitle = NULL,
                         xlabel,
                         ylabel,
                         font = "Roboto Condensed",
                         fontsize_barlabs = 14,
                         fontsize_title = 20,
                         fontsize_subt = 16,
                         fontsize_axis = 15,
                         savefig = FALSE,
                         figname = NULL,
                         dpi = 96,
                         scale = 1,
                         transparent_bg = FALSE) {
    fig <- ggplot(data,
                  aes(fct_rev(fct_reorder(SDG, Frequency)), Frequency)) +
        geom_col(aes(fill = Color)) +
        geom_text(
            aes(SDG, Frequency, label = Frequency, size = fontsize_barlabs),
            angle = 90,
            vjust = 0.35,
            hjust = 1.3,
            colour = 'white',
        ) +
        scale_fill_identity() +
        ggtitle(title, subtitle) +
        xlab(xlabel) +
        ylab(ylabel) +
        theme_minimal() +
        theme(
            aspect.ratio = 0.4,
            axis.title.x = element_text(size = fontsize_axis,
                                        margin = margin(15, 0, 0, 0)),
            axis.title.y = element_text(size = fontsize_axis,
                                        margin = margin(0, 15, 0, 0)),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5,
                                       hjust = 0,
                                       size = fontsize_axis),
            axis.text.y = element_text(size = fontsize_axis),
            legend.position = 'none',
            plot.title = element_text(size = fontsize_title,
                                      face = 'bold',
                                      hjust = 0,
                                      margin = margin(0, 0, 0, 0),
                                      family = 'Roboto Condensed'),
            plot.subtitle = element_text(size = fontsize_subt,
                                         hjust = 0,
                                         margin = margin(0, 0, 25, 0))
        )
    return(fig)
}


plot_network <- function(mapping_res,
                         concentric = FALSE,
                         savefig = FALSE,
                         figname = NULL,
                         title = "Interactions between the SDGs",
                         subtitle = "In the World Bank's portfolio",
                         font = "Roboto Condensed",
                         fontsize_base = 15,
                         fontsize_title = 20,
                         fontsize_subt = 16,
                         dpi = 96,
                         scale = 1) {
    folder <- outputs_folder('img')

    net <- generate_network(mapping_res)

    if (concentric == TRUE) {
        ggraph(net, 'focus', focus = node_is_center()) +
            ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r),
                                 data.frame(r = 1:3),
                                 colour = 'grey')
    } else {
        g <- ggraph(net, layout = 'igraph', algorithm = 'nicely')
    }

    g <- g +
        # Edges' settings
        geom_edge_link(aes(
            colour = color,
            alpha = weight,
            width = weight
        )) +
        scale_edge_colour_identity() +
        scale_edge_width(range = c(0.1, 0.8), name = 'Weight') +
        scale_edge_alpha_continuous(range = c(0.05, 1), name = 'Weight') +
        # Nodes' settings
        geom_node_point(aes(
            size = degree,
            colour = color),
        ) +
        geom_node_text(aes(
            label = names(as.list(V(net))),
            size = degree),
            show.legend = FALSE,
            colour = 'gray4',
            repel = TRUE,
            family = font
        ) +
        scale_colour_identity() +
        scale_size_continuous(name = 'Degree', range = c(1, 8)) +
        scale_label_size_continuous(range = c(0.8, 1.4)) +
        # General settings and aesthetics configurations
        theme_graph(
            background = 'white',
            title_margin = 0,
            subtitle_margin = 20,
            title_size = fontsize_title,
            subtitle_size = fontsize_subt,
            base_size = fontsize_base,
            base_family = font,
            plot_margin = margin(15, 15, 15, 15),
        ) +
        # Title
        ggtitle(title,
                subtitle)

    if (savefig == TRUE) {
        if (isSingleString(figname)) {
            ggsave(here(glue('{folder}/{figname}.png')),
                   plot = g,
                   dpi = dpi,
                   scale = scale)

            cli_alert_success(glue(
                "Graph successfully exported to the path ",
                style_underline(style_italic(
                    col_br_red("\'{folder}/{figname}.png\'")))
            ))
        }
    }
    return(g)
}


plot_SDG_distribution <- function(mapping_res,
                                  binwidth = 2,
                                  title = NULL,
                                  subtitle = NULL,
                                  xlabel = "Number of Goals",
                                  ylabel = "Number of projects",
                                  font = "Roboto Condensed",
                                  fontsize_title = 20,
                                  fontsize_subt = 16,
                                  fontsize_axis = 15,
                                  kde = FALSE,
                                  savefig = FALSE,
                                  figname = NULL,
                                  dpi = 96,
                                  scale = 1,
                                  transparent_bg = FALSE,
                                  test = FALSE) {
    folder <- outputs_folder('img')

    if (test == FALSE) {
        SDG_dist <- get_SDGs_proj(mapping_res)
    } else if (test == TRUE){
        SDG_dist <- tibble(Project = as.character(1:150),
                           Frequency = base::round(rnorm(150, 8, 2), 0))
    } else {
        cli_abort("Argument 'test' must be either TRUE or FALSE")
    }

    histo <- ggplot(SDG_dist, aes(Frequency)) +
        geom_histogram(binwidth = binwidth, boundary = 0)

    xticks <- as.list(round(ggplot_build(histo)$data[[1]][4], 4))[[1]]
    xticks <- c(xticks, tail(xticks, n = 1) + (xticks[2] - xticks[1]))

    max_val <- as.list(round(ggplot_build(histo)$data[[1]][1], 1))[[1]]
    max_val <- max(max_val)

    mean_goals <- mean(SDG_dist$Frequency)

    histo <- histo +
        scale_x_continuous(breaks = xticks,
                           labels = round(xticks, 1)
        ) +
        theme(legend.position = 'none') +
        ggtitle(title) +
        xlab(xlabel) +
        ylab(ylabel) +
        theme_minimal() +
        theme(
            aspect.ratio = 0.4,
            axis.title = element_text(size = fontsize_axis),
            axis.text.x = element_text(angle = 0,
                                       vjust = 0.5,
                                       hjust = 1,
                                       size = fontsize_axis),
            axis.text.y = element_text(size = fontsize_axis),
            plot.title = element_text(size = fontsize_title,
                                      face = 'bold',
                                      hjust = 0,
                                      margin = margin(0, 0, 0, 0),
                                      family = font),
            plot.subtitle = element_text(size = fontsize_subt,
                                         hjust = 0,
                                         margin = margin(0, 0, 25, 0)),
            legend.position = 'none'
        ) +
        geom_vline(xintercept = mean_goals,
                   linetype = 'dashed',
                   color = 'red',
                   lwd = 1) +
        geom_text(aes(x = mean_goals + 0.1,
                      y = max_val*1.03,
                      label = glue::glue("Mean = {round(mean_goals, 1)}"),
                      hjust = 0,
                      vjust = 0,
                      colour = 'red',
                      alpha = 1,
                      size = fontsize_axis
        ),
        ) +
        ggtitle(title, subtitle)

    if (kde == TRUE) {
        histo <- histo +
            geom_density(aes(y = after_stat(count * 2),
                             colour = 'darkturquoise'),
                         kernel = "gaussian",
                         lwd = 1,
                         linetype = 1
            )
    }

    if (savefig == TRUE) {
        if (isSingleString(figname)) {
            if (transparent_bg == FALSE) {
                ggsave(
                    here(glue('{folder}/{figname}.png')),
                    plot = histo,
                    device = 'png',
                    scale = scale,
                    units = 'cm',
                    dpi = dpi,
                    bg = 'white'
                )

                cli_alert_success(glue(
                    "Plot successfully exported to the path ",
                    style_underline(style_italic(col_br_red(glue(
                        "\'{folder}/{figname}.png\'"))))
                ))
            } else if (transparent_bg == TRUE) {
                ggsave(
                    here(glue('{folder}/{figname}.png')),
                    plot = histogram,
                    device = 'png',
                    scale = scale,
                    units = 'cm',
                    dpi = dpi
                )

                cli_alert_success(glue(
                    "Plot successfully exported to the path ",
                    style_underline(style_italic(col_br_red(glue(
                        "\'{folder}/{figname}.png\'"))))
                ))
            } else {
                cli_abort(paste0("The argument 'transparent_bg' must be ",
                                 "either TRUE or FALSE"))
            }
        } else {
            cli_abort("The argument 'figname' must be a single string")
        }
    }
    return(histo)
}


# ===== Exporter functions =====================================================

export_plot <- function(plot,
                        figname,
                        transparent_bg = FALSE,
                        dpi = 96,
                        scale = 1) {
    folder <- here(outputs_folder('img'))

    if (isSingleString(figname)) {
        if (transparent_bg == FALSE){
            ggsave(
                here(glue('{folder}/{figname}.png')),
                plot = plot,
                device = 'png',
                scale = scale,
                dpi = dpi,
                bg = 'white')

            cli_alert_success(glue(
                "Plot successfully exported to the path ",
                style_underline(style_italic(col_br_red(glue(
                    "\'{folder}/{figname}.png\'"))))
            ))
        } else if (transparent_bg == TRUE) {
            ggsave(
                here(glue('{folder}/{figname}.png')),
                plot = plot,
                device = 'png',
                scale = scale,
                dpi = dpi)

            cli_alert_success(glue(
                "Plot successfully exported to the path ",
                style_underline(style_italic(
                    col_br_red("\'{folder}/{figname}.png\'\n\n"))
                )
            ))
        } else {
            cli_abort(paste0("The argument 'transparent_bg' must be ",
                             "either TRUE or FALSE"))
        }
    } else {
        cli_abort("The argument 'figname' must be a single string")
    }
}


prompt_export_plot <- function(data,
                               title = NULL,
                               subtitle = NULL,
                               xlabel,
                               ylabel,
                               font = "Roboto Condensed",
                               fontsize_barlabs = 14,
                               fontsize_title = 20,
                               fontsize_subt = 16,
                               fontsize_axis = 15,
                               figname = NULL,
                               dpi = 96,
                               scale = 1,
                               transparent_bg = FALSE) {
    folder <- here(outputs_folder('img'))

    plot <- plot_results(data,
                         title,
                         subtitle,
                         xlabel,
                         ylabel = ylabel,
                         font = font,
                         fontsize_barlabs = 14,
                         fontsize_title = 20,
                         fontsize_subt = 16,
                         fontsize_axis = 15,
                         savefig = FALSE,
                         figname = NULL,
                         dpi = 96,
                         scale = 1,
                         transparent_bg = FALSE)

    print(plot)

    cli_h3("Do you wish to save this plot?")
    cli_alert_info(paste0(
        "Save plot: press 'y' or 'Y' and hit Enter\n",
        "Discard: press 'n' or 'N' and hit Enter\n\n")
    )

    answer <- invisible(readline())

    if ((answer == 'y') || (answer == 'Y')) {
        plot <- plot_results(data,
                             title,
                             subtitle,
                             xlabel,
                             ylabel,
                             font,
                             fontsize_barlabs,
                             fontsize_title,
                             fontsize_subt,
                             fontsize_axis,
                             savefig = TRUE,
                             figname = NULL,
                             dpi = 96,
                             scale = 1,
                             transparent_bg = FALSE)

        export_plot(plot,
                    figname,
                    transparent_bg = transparent_bg,
                    dpi = dpi,
                    scale = scale)
    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(
            glue("You should introduce either 'y' / 'Y' or 'n' / 'N' ",
                 "but introduced {answer}. Try again"))
        prompt_export_plot(data = data,
                           title = title,
                           subtitle = subtitle,
                           xlabel = xlabel,
                           ylabel = ylabel,
                           font = font,
                           fontsize_barlabs = fontsize_barlabs,
                           fontsize_title = fontsize_title,
                           fontsize_subt = fontsize_subt,
                           fontsize_axis = fontsize_axis,
                           figname = figname,
                           dpi = dpi,
                           scale = scale)
    }
}


prompt_export_graph <- function(mapping_res,
                                concentric = FALSE,
                                figname = NULL,
                                title = "Interactions between the SDGs",
                                subtitle = "In the World Bank's portfolio",
                                font = "Roboto Condensed",
                                fontsize_base = 15,
                                fontsize_title = 20,
                                fontsize_subt = 16,
                                dpi = 96,
                                scale = 1) {
    folder <- outputs_folder('img')

    g <- plot_network(mapping_res,
                      concentric,
                      FALSE,
                      figname,
                      title,
                      subtitle,
                      font,
                      fontsize_base = 15,
                      fontsize_title = 20,
                      fontsize_subt = 16,
                      dpi = 96,
                      scale = 1)

    print(g)

    cli_h3("Do you wish to save this graph?")
    cli_alert_info(paste0(
        "The position of the nodes is different for each iteration of the ",
        "graph. This means the exported result will have different node ",
        "positions\n\n",
        "Save plot: press 'y' or 'Y' and hit Enter\n",
        "Discard: press 'n' or 'N' and hit Enter\n\n")
    )

    answer <- invisible(readline())

    if ((answer == 'y') || (answer == 'Y')) {
        g <- plot_network(mapping_res,
                          concentric,
                          TRUE,
                          figname,
                          title,
                          subtitle,
                          font,
                          fontsize_base,
                          fontsize_title,
                          fontsize_subt,
                          dpi,
                          scale)

        # cli_alert_success(glue(
        #     "Plot successfully exported to the path ",
        #     style_underline(style_italic(col_br_red(glue(
        #         "\'{folder}/{figname}.png\'\n\n"))))
        # ))
    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(
            glue("You should introduce either 'y' / 'Y' or 'n' / 'N' ",
                 "but introduced {answer}. Try again")
        )

        prompt_export_graph(mapping_res,
                            concentric,
                            figname,
                            title,
                            subtitle,
                            font,
                            fontsize_base,
                            fontsize_title,
                            fontsize_subt,
                            dpi,
                            scale)
    }
}

prompt_export_histogram <- function(data,
                                    binwidth = 2,
                                    title = NULL,
                                    subtitle = NULL,
                                    xlabel = "Number of Goals",
                                    ylabel = "Number of projects",
                                    font = "Roboto Condensed",
                                    fontsize_title = 20,
                                    fontsize_subt = 16,
                                    fontsize_axis = 15,
                                    kde = FALSE,
                                    figname = "SDG_distribution_proj",
                                    dpi = 96,
                                    scale = 1,
                                    transparent_bg = FALSE,
                                    test = FALSE) {
    folder <- outputs_folder('img')

    histogram <- plot_SDG_distribution(data,
                                       binwidth,
                                       title,
                                       subtitle,
                                       xlabel,
                                       ylabel,
                                       font,
                                       fontsize_title = 20,
                                       fontsize_subt = 16,
                                       fontsize_axis = 15,
                                       kde = kde,
                                       savefig = FALSE,
                                       figname = figname,
                                       dpi = 96,
                                       scale = 1,
                                       transparent_bg = FALSE,
                                       test = test)

    print(histogram)

    cli_h3("Do you wish to save this plot?")
    cli_alert_info(paste0(
        "Save plot: press 'y' or 'Y' and hit Enter\n",
        "Discard: press 'n' or 'N' and hit Enter\n\n")
    )

    answer <- invisible(readline())

    if ((answer == 'y') || (answer == 'Y')) {
        histogram <- plot_SDG_distribution(data,
                                           binwidth,
                                           title,
                                           subtitle,
                                           xlabel,
                                           ylabel,
                                           font,
                                           fontsize_title,
                                           fontsize_subt,
                                           fontsize_axis,
                                           kde,
                                           TRUE,
                                           figname,
                                           dpi,
                                           scale,
                                           transparent_bg,
                                           test)

        # cli_alert_success(glue(
        #     "Plot successfully exported to the path ",
        #     style_underline(style_italic(col_br_red(glue(
        #         "\'{folder}/{figname}.png\'\n\n"))))
        # ))
    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(
            glue("You should introduce either 'y' / 'Y' or 'n' / 'N' ",
                 "but introduced {answer}. Try again")
        )
    }
}


export_summary <- function(summary, filename) {
    folder <- outputs_folder('data')

    write.csv(summary,
              glue("{folder}/{filename}.csv"),
              row.names = FALSE)
    cli_alert_success(glue(
        "Data successfully exported to the path ",
        style_underline(style_italic(col_br_red(glue(
            "\'{folder}/{filename}.csv\'"))))
    ))
}


prompt_export_summary <- function() {
    cli_h2(glue(
        "Do you wish to ", col_br_green("export the results data"), "?"
    ))
    cli_alert_info(paste0(
        "Save plot: press 'y' or 'Y' and hit Enter\n",
        "Discard: press 'n' or 'N' and hit Enter\n\n")
    )
    answer <- invisible(readline())

    if ((answer == 'y') | (answer == 'Y')) {
        cli_alert_info("Results data will be exported")
    } else if ((answer == 'n') | (answer == 'N')) {
        cli_alert_warning("Results data will not be exported")
    } else {
        cli_alert_warning(
            glue("You should introduce either 'y' / 'Y' or 'n' / 'N' ",
                 "but introduced {answer}. Try again")
        )
        prompt_export_summary()
    }
    return(answer)
}




# Found in StackOverflow.
# Answer by https://stackoverflow.com/users/303052/jamesatha
# Thread: https://stackoverflow.com/questions/38385521/r-determine-if-a-variable
# -is-a-string
isSingleString <- function(input) {
    is.character(input) & length(input) == 1
}
