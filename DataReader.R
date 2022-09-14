library(pdftools)
library(tidyverse)
library(glue)


extract <- function(folder_path, absolute_path = FALSE) {
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

    print(glue("Extracted document {i} of {length(filenames)}"))

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


tidify <- function(df, get_top = 1, export_json = FALSE, json_name = NULL) {
  tibblist <- list()

  for (i in 1:nrow(df)) {
    t_result <- df %>%
      slice(i)

    t_result <- unnest_tokens(t_result, Bigram, Text, "ngrams", n = 2)

    t_result <- t_result %>%
      count(File, Bigram, sort = TRUE) %>%
      filter(str_detect(Bigram, "[:alpha:]"))

    temp <- data.frame(do.call(
      "rbind",
      strsplit(as.character(t_result$Bigram),
        " ",
        fixed = TRUE
      )
    ))

    t_result <- t_result %>%
      mutate(Source = temp$X1, Target = temp$X2, Weight = n, n = NULL) %>%
      filter(!(Source %in% stop_words$word)) %>%
      filter(!(Target %in% stop_words$word)) %>%
      filter(str_detect(Source, "[:alpha:]")) %>%
      filter(str_detect(Target, "[:alpha:]")) %>%
      filter(nchar(Source) > 1 & nchar(Target) > 1)

    t_result <- t_result %>%
      slice(1:round(nrow(t_result) * get_top, 0))

    tibblist <- c(tibblist, list(t_result))

    print(glue("Tidied {i} of {nrow(df)}."))
  }

  tibblist <- as_tibble(data.table::rbindlist(tibblist))

  if (export_json == TRUE) {
    cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
    write(jsonlite::toJSON(tibblist),
      file = glue("{cwd}/Saves/Tidy/{json_name}.json")
    )
  }

  return(tibblist)
}


import_json <- function(json_path, absolute_path = FALSE) {
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
