library(pdftools)
library(tidyverse)
library(glue)


extract <- function(folder_path, absolute_path=FALSE){
  
  cwd <- dirname(rstudioapi::getSourceEditorContext()$path)
  
  if (absolute_path == FALSE){
    path <- paste(cwd, folder_path, sep='/')
  }
  
  if (absolute_path == TRUE){
    path <- folder_path
  }
  
  filenames <- list.files(path, pattern="*.pdf", full.names=TRUE)
  
  texts <- list()
  
  i <- 1
  
  for (name in filenames){
    text <- ""
    for (str in pdf_text(name)){
      text <- paste(text, str, sep=' ')
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
    
    print(glue('Extracted document {i} of {length(filenames)}'))
    
    i <- i + 1
  }  
  
  result <- tibble(File=filenames,
                   Text=texts)
  
  return(result)
}

textos <- extract_many('Test', absolute_path = FALSE)
