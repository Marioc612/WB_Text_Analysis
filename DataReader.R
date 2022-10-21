library(cli)
library(glue)
library(jsonlite)
library(pdftools)
library(stringr)
library(tidyverse)
library(tidytext)


extract <- function(folder_path, absolute_path = FALSE) {
    cli::cli_h1(
        glue::glue(
            "The extraction process has started. Please, be patient, this ",
            " may take a while."
        )
    )

    # Initializes the function's timer
    t_0_general <- Sys.time()

    # Creates the stop words regex for cleaning the data using the TidyText's
    # stop word list
    stopwords_regex <- paste(tidytext::stop_words$word, collapse = '\\b|\\b')
    stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')

    # Sets the conditions for treating the path input as a relative or absolute
    # path
    if (absolute_path == FALSE) {
        cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
        path <- paste(cwd, folder_path, sep = "/")
    }

    if (absolute_path == TRUE) {
        path <- folder_path
    }

    # Get a list of all PDF files in a folder
    filenames <- list.files(path, pattern = "*.pdf", full.names = TRUE)

    # Iterates through all PDF files in the folder, reads, and cleans their
    # content
    texts <- list()

    # Creates the progress bar
    cli::cli_alert_info("Preparing to start extracting the texts")
    cli::cli_progress_bar('Extracting texts',
                          total = length(filenames),
                          clear = FALSE)
    for (name in filenames) {
        # Iterates through each document's strings and concatenates it into one
        # single string
        text <- ""
        for (str in pdftools::pdf_text(name)) {
            text <- paste(text, str, sep = " ")
        }

        # Removes stop words from the text, as well as non-alphanumeric and
        # non-punctuation characters
        text <- trimws(gsub("\\s+", " ", text))
        text <- trimws(gsub("Public Disclosure Authorized", "", text))
        text <- trimws(gsub("Document of The World Bank", "", text))
        text <- trimws(gsub("Document o f The World Bank", "", text))
        text <- trimws(gsub("FOR OFFICIAL USE ONLY", "", text))
        text <- trimws(gsub("For Official Use Only", "", text))
        text <- trimws(gsub("FOR OFFICIAL, USE ONLY", "", text))
        text <- trimws(gsub("The World Bank", "", text))
        text <- trimws(gsub("[^[:alpha:] .,]", '', text))
        text <- stringr::str_squish(text)

        # Concatenates all documents into one list containing all of them. Each
        # item in the list is a whole document
        texts <- c(texts, text)

        # Updates the progress bar for each document iteration
        cli::cli_progress_update()
    }

    # Creates the regex string for cleaning the filenames
    p_types <- c('PAD_', 'PID_', 'PGD_')
    p_types = paste(p_types, collapse = '\\b|\\b')
    p_types = paste0('\\b', p_types, '\\b')

    # Removes all path components but the file name
    filenames <- gsub(path, "", filenames)
    filenames <- gsub(".pdf", "", filenames)
    filenames <- gsub("/", "", filenames)
    filenames <- gsub("^.{0,4}", "", filenames)

    # Creates the tibble containing all the file names and the extracted texts
    result <- tibble(Project = filenames,
                     Text = texts)

    # Updates the progress bar to 'Done' status
    cli::cli_progress_done(result = "done")

    # Ends the function's timer
    t_f_general <- Sys.time()

    # Prints that the process is done and the time it took to complete it
    cli::cli_alert_success(
        glue::glue(
            "\n\nDone! The process took ",
            "{round(difftime(t_f_general, t_0_general, units = 'mins'), 2)} ",
            "minutes"
        )
    )

    return(result)
}


tidify <- function(df,
                   token = 'sentences',
                   n = 2,
                   low_lim = 0,
                   up_lim = 1,
                   network_mode = FALSE,
                   export_json = FALSE,
                   json_name = NULL) {
    cli::cli_h1(glue::glue(
        "The tidying process has started. Please, be patient, this may ",
        "take a while"
    ))

    # Initializes the function's timer
    t_0_general <- Sys.time()

    # Creates the stop words regex for cleaning the tokens
    stopwords_regex <- c('[^a-zA-Z\\d\\s:]', as.list(stop_words$word))
    stopwords_regex <- paste(stopwords_regex, collapse = '\\b|\\b')
    stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')

    # Iterates through the tibble with the documents and their texts, and
    # tokenizes them. This will create several rows for each document, every row
    # containing a token (sentence, n-gram, etc.)
    tibblist <- list()

    # Creates the progress bar
    cli::cli_alert_info("Preparing to start tidying the texts")
    cli::cli_progress_bar('Tidying texts',
                          total = nrow(df),
                          clear = FALSE)
    for (i in 1:nrow(df)) {
        # Extract every individual document by slicing the input tibble
        document <- df %>%
            slice(i)

        # Decides what to do regarding the input tokens
        if (token == 'ngrams') {
            document <- tidytext::unnest_tokens(document,
                                                Text,
                                                Text,
                                                "ngrams",
                                                n = n,
                                                to_lower = TRUE)
        } else {
            document <- tidytext::unnest_tokens(document,
                                                Text,
                                                Text,
                                                token,
                                                to_lower = TRUE)
        }

        # Cleans each document's token tibble
        document <- document %>%
            count(Project, Text, sort = TRUE, name = 'Frequency') %>%
            filter(stringr::str_detect(Text, "[:alpha:]")) %>%
            filter(!stringr::str_detect(Text, '[.]{3}|[. ]{4}')) %>%
            filter(nchar(Text) > 15)

        # Slices each document's token tibble with the range of data required by
        # the user and set by the parameters 'low_lim' and 'up_lim'. These are
        # retrieved by the frequency of each token
        document <- document %>%
            slice(round(nrow(document) * low_lim, 0):round(nrow(document) *
                                                               up_lim, 0))

        # Cleans further the remaining tokens
        document <- document %>%
            mutate(Text = trimws(
                stringr::str_replace_all(Text, stopwords_regex, ''))
            ) %>%
            mutate(Text = stringr::str_squish(
                trimws(stringr::str_replace_all(Text, '[^[:alpha:] ]', '')))
            )

        # Concatenates all the document's token tibbles into a list of tibbles
        # called 'tibblist' (one document per iteration)
        tibblist <- c(tibblist, list(document))

        # Updates the progress bar for each document iteration
        cli::cli_progress_update()
    }

    # Concatenates the list of tibble into a single tibble
    tibblist <- as_tibble(data.table::rbindlist(tibblist))

    tibblist %>%
        mutate(Frequency=NULL)

    # Updates the progress bar to "Done" status
    cli::cli_progress_done(result = "done")

    # Saves the results to a JSON file
    if (export_json == TRUE) {
        cwd <- dirname(rstudioapi::getSourceEditorContext()$path)

        write(jsonlite::toJSON(tibblist),
              file = glue("{cwd}/Saves/{json_name}.json"))

        cli::cli_alert_success(glue(
            "Data successfully exported to the path ",
            cli::style_underline(cli::style_italic(
                cli::col_br_red("\'{cwd}/Saves/{json_name}.json\'")
            ))
        ))
    }

    # Ends the function's general timer
    t_f_general <- Sys.time()

    # Prints that the process is done and the time it took to complete it
    cli::cli_alert_success(
        glue::glue(
            "\n\nDone! The process took ",
            "{round(difftime(t_f_general, t_0_general, units = 'mins'), 2)} ",
            "minutes"
        )
    )

    return(tibblist)
}


from_saves <- function(json_name) {
    cwd <- dirname(rstudioapi::getSourceEditorContext()$path)

    path = glue::glue('{cwd}/Saves/{json_name}.json')

    json_file <- as_tibble(jsonlite::fromJSON(path))

    cli::cli_alert_success('Successfully imported JSON')

    return(json_file)
}
