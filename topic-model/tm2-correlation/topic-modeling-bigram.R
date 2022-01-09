### topic-modeling-bigram.R
### by Marcellinus Jerricho
### last updated: 05-08-21

## import libraries ------------------------------------------------------------
# data loading
library(readxl)
# data wrangling
library(dplyr)
library(tidyr)
# topic model
library(tidytext)
#visualization
library(ggplot2)
library(igraph)
library(ggraph)

### load data -----------------------------------------------------------------
narratives <- data.frame(narrative = character())
folder.name <- "batch_1"
# iterate through files in directory ------
file.names <- dir(paste("./narratives/",folder.name, sep = ""), pattern = ".xlsx")
for (j in 1:length(file.names)){
  print(paste("processing", file.names[j]))
  
  # load data -----
  print(paste("Loading ./narratives/", folder.name, "/", file.names[j], sep = ""))
  deb <- read_excel(paste("./narratives/", folder.name, "/", file.names[j], sep = ""))
  narratives <- narratives %>% add_row(narrative = deb$narrative)
  # print(paste("Loaded ./narratives/", folder.name, "/", file.names[j], sep = ""))
  
}
narratives_df <- tibble(text = narratives$narrative)

### processing data bigrams-----------------------------------------------------
## writing functions
count_bigrams <- function(dataset, stopwords) {
  dataset %>%
    mutate(text = gsub(x = text, 
                       pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", 
                       replacement = "")) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stopwords$word,
           !word2 %in% stopwords$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, 
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 3) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1, repel = TRUE) +
    theme_void()
}

## removing stopwords
# uploading the stopwords
stopwords <- read.table("stopwords-final.txt")
stopwords_df <- tibble(word = stopwords$V1)

# finally processing
nar_bigrams <- narratives_df %>%
  count_bigrams(stopwords_df)

# View(nar_bigrams)

#filter out rare combinations and visualize bigrams
jpeg("./img/img3-bigrams.jpeg")
nar_bigrams %>% 
  filter(n > 40,
         !word1 %in% c("kabupaten", "kota")) %>%
  visualize_bigrams()
dev.off()

# this gives us insights into the pairs of adjacent words.
# we merge all the debates together


