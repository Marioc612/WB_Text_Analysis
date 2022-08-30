library(tidytext)
library(tidyverse)
library(readr)
library(widyr)
library(ggplot2)
library(ggraph)
library(igraph)


### Leer el archivo y convertirlo en string

cwd <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(cwd)

text <- read_file('FAO_2017_GenderStrat.txt')

text <- gsub("[\r]", " ", text)
text <- gsub("  ", " ", text)


### Convertir el string en una lista de palabras

list <- as.list(strsplit(text,"\n")[[1]])


### Crear un tibble con la lista de *líneas* del documento y un id de cada línea
### (pensé que esto se podría utilizar para dar un valor diferencial a cada relación
### de palabras basado en qué tan cerca están la una de la otra, pero aún no sé
### cómo hacerlo)

index <- as.list(seq(1, length(list), by=1))

df <- tibble(index, list)


### Quita los dobles espacios entre palabras

df <- df %>% 
  filter(list!=" ") %>% 
  filter(list!='')


### Extrae los bigrams del texto

tidy_df <- df %>% 
  mutate(strings=list, list=NULL, group=NULL) %>% 
  filter(!strings %in% stop_words$word) %>% 
  unnest_tokens(bigram, strings, token='ngrams', n=2) %>% 
  distinct() %>% 
  na.omit()


### Cuenta cuántas veces aparece cada bigram en el texto

bi_count <- tidy_df %>% 
  count(bigram, sort=TRUE)


### Separa los bigrams: una palabra por columna (tipo source --> target) y
### los almacena en un dataframe temporal. Luego, se crean las columnas
### word1 (source) y word2 (target) en la tabla bi_count

temp <- data.frame(do.call("rbind", strsplit(as.character(bi_count$bigram), " ",
                                     fixed = TRUE)))

bi_count <- bi_count %>% 
  mutate(word1=temp$X1,
         word2=temp$X2)


### Reordena las columnas

bi_count <- bi_count[c(1, 3, 4, 2)]


### Elimina los bigrams con stopwords

bi_count <- bi_count %>% 
  filter(!(word1 %in% stop_words$word | word2 %in% stop_words$word)) %>% 
  filter(str_detect(word1, "[:alpha:]")) %>% 
  filter(str_detect(word2, "[:alpha:]"))


### Crea la tabla de edges

edges <- bi_count %>% 
  select(c(2, 3, 4))

edges <- edges %>% 
  mutate(Source=word1, Target=word2, word1=NULL, word2=NULL, Weight=n) %>% 
  mutate(n=NULL)


### Normaliza el peso de cada bigram

total <- sum(edges$Weight)

edges <- edges %>% 
  mutate(Weight=Weight/total)


### Filtra los bigrams de acuerdo a su peso

edges <- edges %>% 
  filter(Weight >= 3/total)


### Crea la tabla de nodos

nodes <- append(as.list(unique(edges$Source)), as.list(unique(edges$Target)))

nodes <- nodes %>% 
  as.list(unique())


### Grafica la red

ggraph(edges, layout = 'igraph', algorithm='fr')+
  geom_node_point()+
  geom_edge_link(aes(alpha=Weight))+
  geom_node_text(aes(label=name), repel=TRUE)
