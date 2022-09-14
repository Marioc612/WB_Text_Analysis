library(pdftools)
library(tidyverse)
library(glue)


#' Extract the texts of all the PDF files in a folder
#'
#' @param folder_path (character). A relative or absolute path to a folder.
#' If it is an absolute path, you should set the \code{absolute_path}
#' parameter to \code{TRUE}.
#' @param absolute_path (logical). By default, set as \code{FALSE}.
#' It should be set to TRUE if you want to introduce an absolute 
#' \code{folder_path} parameter.
#'
#' @return A Tibble with the texts of all PDF files indexed by file name. It
#' contains two columns: File and Text.
#' - File: The name of the file found in the folder.
#' - Text: The text extracted of each File.
#' @export
#'
#' @examples
#' texts <- extract('relative/path_to_folder', absolute_path = FALSE)
#' texts <- extract('C:/Users/Documents/SDG_map/folder_with_PDFs',
#'   absolute_path = TRUE)
extract <- function(folder_path, absolute_path = FALSE) {
  # Checks if the introduced path is absoulte or relative
  if (absolute_path == FALSE) {
    cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
    path <- paste(cwd, folder_path, sep = "/")
  }

  if (absolute_path == TRUE) {
    path <- folder_path
  }

  # Gets the names of the PDF files in the specified folder
  filenames <- list.files(path, pattern = "*.pdf", full.names = TRUE)

  # Initialises auxiliar variables for the next for loop
  texts <- list()
  i <- 1

  # Iterates through the files and retrieves their text
  for (name in filenames) {
    text <- ""
    for (str in pdf_text(name)) {
      text <- paste(text, str, sep = " ")
    }

    text <- trimws(gsub("\\s+", " ", text))
    text <- trimws(gsub("Public Disclosure Authorized", "", text))
    text <- trimws(gsub("Document of The World Bank", "", text))
    text <- trimws(gsub("Document o f The World Bank", "", text))
    text <- trimws(gsub("FOR OFFICIAL USE ONLY", "", text))
    text <- trimws(gsub("For Official Use Only", "", text))
    text <- trimws(gsub("FOR OFFICIAL, USE ONLY", "", text))
    text <- trimws(gsub("The World Bank", "", text))

    texts <- c(texts, text)

    # Keeps the count of the processed files
    print(glue("Extracted document {i} of {length(filenames)}"))
    i <- i + 1
  }

  filenames <- gsub(path, "", filenames)
  filenames <- gsub(".pdf", "", filenames)
  filenames <- gsub("/", "", filenames)

  return(tibble(
    File = filenames,
    Text = texts)
}


#' Separates the texts contained in a tibble into bigrams
#'
#' @param df (tbl_df). A tibble with two columns: File and Text. The File
#' column must contain the name of each document and the Text column must
#' contain the text to be separated into tokens -- tidied.
#' @param get_top (numeric). A number between 0 and 1. It indicates the top
#' fraction of bigrams (according to their frequency in each text).
#' \code{get_top = 0.5} will get the 50% most frequent bigrams in each text.
#' @param export_json (logical). By default, set to \code{FALSE} 
#' <If \code{export_json = TRUE}, it will save the results to a json file 
#' in the 'test/outputs' folder.
#' @param json_name (character). It is the name for the json file saved in
#' the 'Saves/Tidy' folder. It must not contain the file extension.
#'
#' @return A tibble with five columns: File, Bigram, Source, Target, and
#' Weight.
#' - File:   Names of the files containing the text.
#' - Bigram: Bigram found in text.
#' - Source: First word in the Bigram. It is useful for creating networks and
#' analysis based in the separate words of the bigram.
#' - Target: Second word in the Bigram. It is useful for creating networks and
#' analysis based in the separate words of the bigram.
#' - Weight: Frequency in which the bigrams appear in each document (File).
#' @export
#'
#' @examples
#' tidy_df <- tidify(df, get_top = 0.5,
#'   export_json = TRUE, json_name = 'Saved_results')
#' tidy_df <- tidify(df, get_top = 0.75)
tidify <- function(df, get_top = 1, export_json = FALSE, json_name = NULL) {
  tibblist <- list()

  # Slices the dataset into rows and iterates through them
  for (i in 1:nrow(df)) {
    t_result <- df %>%
      slice(i)

    # Extracts the bigrams from each text
    t_result <- unnest_tokens(t_result, Bigram, Text, "ngrams", n = 2)

    # Counts the number of ocurrences of the bigrams in each file
    t_result <- t_result %>%
      count(File, Bigram, sort = TRUE) %>%
      filter(str_detect(Bigram, "[:alpha:]"))

    # Splits the bigrams into two columns and saves them in a temporary
    # variable
    temp <- data.frame(do.call(
      "rbind",
      strsplit(as.character(t_result$Bigram),
        " ",
        fixed = TRUE
      )
    ))

    # Adds the separated bigram columns to the main tibble. Also removes 
    # stopwords, non-alphabetic bigrams, and words smaller than two letters
    t_result <- t_result %>%
      mutate(Source = temp$X1, Target = temp$X2, Weight = n, n = NULL) %>%
      filter(!(Source %in% stop_words$word)) %>%
      filter(!(Target %in% stop_words$word)) %>%
      filter(str_detect(Source, "[:alpha:]")) %>%
      filter(str_detect(Target, "[:alpha:]")) %>%
      filter(nchar(Source) > 1 & nchar(Target) > 1)

    # Slices the list of bigrams to retrieve the top selected percentage
    t_result <- t_result %>%
      slice(1:round(nrow(t_result) * get_top, 0))

    # Appends the bigrams of all documents
    tibblist <- c(tibblist, list(t_result))

    print(glue("Tidied {i} of {nrow(df)}."))
  }

  # Creates a tibble with the bigrams of all documents
  tibblist <- as_tibble(data.table::rbindlist(tibblist))

  # Exports the results to a json file
  if (export_json == TRUE) {
    cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
    write(jsonlite::toJSON(tibblist),
      file = glue("{cwd}/test/outputs/{json_name}.json")
    )
  }

  return(tibblist)
}


#' Imports the saved results (json file) from the \code{tidify()}
#' function.
#'
#' @param json_path (character). A relative or absolute path to a folder.
#' If it is an absolute path, you should set the \code{absolute_path}
#' parameter to \code{TRUE}.
#' @param absolute_path (logical). By default, set as \code{FALSE}.
#' It should be set to TRUE if you want to introduce an absolute 
#' \code{folder_path} parameter.
#' 
#' @return A tibble with five columns: File, Bigram, Source, Target, and
#' Weight.
#' - File:   Names of the files containing the text.
#' - Bigram: Bigram found in text.
#' - Source: First word in the Bigram. It is useful for creating networks and
#' analysis based in the separate words of the bigram.
#' - Target: Second word in the Bigram. It is useful for creating networks and
#' analysis based in the separate words of the bigram.
#' - Weight: Frequency in which the bigrams appear in each document (File).
#' @export
#'
#' @examples
#' import_json()
import_json <- function(json_path, absolute_path = FALSE) {
  # Checks if the given path is absolute or relative
  if (absolute_path == FALSE) {
    cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
    path <- paste(cwd, json_path, sep = "/")
  }

  if (absolute_path == TRUE) {
    path <- json_path
  }

  return(
    as_tibble(import_json()(path))
  )
}
