library(pdftools)
library(tidyverse)
library(glue)


extract <- function(folder_path, absolute_path = FALSE) {
  print(glue("The extraction process has started. Please, be patient, this ",
             "process may take a while.\n\n"))
  
  stopwords_regex = paste(stop_words$word, collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')  
  
  if (absolute_path == FALSE) {
    cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
    path <- paste(cwd, folder_path, sep = "/")
  }
  
  if (absolute_path == TRUE) {
    path <- folder_path
  }
  
  filenames <- list.files(path, pattern = "*.pdf", full.names = TRUE)
  
  texts <- list()
  
  i <- 1
  for (name in filenames) {
    t_0 <- Sys.time()
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
    
    text <- trimws(gsub("[^[:alpha:] .,]", '', text))
    
    text <- stringr::str_squish(stringr::str_replace_all(text, stopwords_regex, ''))
    
    texts <- c(texts, text)
    
    t_f <- Sys.time()
    
    print(glue(paste("{round(i/length(filenames)*100, 0)} % ({round(t_f - t_0, 1)} seconds)",
                     "Extracted document {i} of {length(filenames)}",
                     sep = '\t-\t')))
    
    i <- i + 1
  }
  
  filenames <- gsub(path, "", filenames)
  filenames <- gsub(".pdf", "", filenames)
  filenames <- gsub("/", "", filenames)
  
  result <- tibble(
    File = filenames,
    Text = texts
  )
  
  return(result)
}


tidify <- function(df, token='sentences', n=2, low_lim = 0, up_lim=1,
                   network_mode=FALSE,
                   export_json = FALSE, json_name = NULL) {
  
  t_0_general <- Sys.time()
  
  stopwords_regex = c('[^a-zA-Z\\d\\s:]', as.list(stop_words$word))
  stopwords_regex = paste(stopwords_regex, collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')  
  
  tibblist <- list()
  for (i in 1:nrow(df)) {
    
    t_0 <- Sys.time()
    
    document <- df %>% 
      slice(i)
    
    if (token == 'ngrams') {
      document <- tidytext::unnest_tokens(document,
                                          Token,
                                          Text,
                                          "ngrams",
                                          n = n,
                                          to_lower = TRUE)
    } else {
      document <- tidytext::unnest_tokens(document,
                                          Token,
                                          Text,
                                          token,
                                          to_lower = TRUE)
    }
    
    document <- document %>%
      count(File, Token, sort = TRUE) %>%
      filter(str_detect(Token, "[:alpha:]")) %>% 
      filter(!str_detect(Token, '[.]{3}|[. ]{4}')) %>% 
      filter(nchar(Token)>15)
    
    document <- document %>% 
      slice(round(nrow(document)*low_lim, 0) : round(nrow(document)*up_lim, 0))
    
    document <- document %>% 
      mutate(Token=trimws(stringr::str_replace_all(Token,
                                                   stopwords_regex,
                                                   '')))
    
    tibblist <- c(tibblist, list(document))
    
    t_f <- Sys.time()
    
    print(glue(paste("{round(i/nrow(df)*100, 0)} % ({round(t_f - t_0, 1)} seconds)",
                     "Tidied document {i} of {nrow(df)}",
                     sep = '\t-\t')))
  }
  
  tibblist <- as_tibble(data.table::rbindlist(tibblist))
  
  t_f_general <- Sys.time()
  
  print(glue("\n\nDone! The process took",
             "{round(difftime(t_f_general, t_0_general), 2)}",
             sep=' '))
  
  # if (export_json == TRUE) {
  #   cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
  #   write(jsonlite::toJSON(tibblist),
  #         file = glue("{cwd}/Saves/Tidy/{json_name}.json")
  #   )
  # }
  
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
