### topic-modeling-correlation.R
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
library(forcats)
library(widyr)
library(igraph)
library(ggraph)

### load data for attack -------------------------------------------------------
narratives <- data.frame(filename = character(), narrative = character())
folder.name <- "batch_1"
# iterate through files in directory ------
file.names <- dir(paste("./narratives/",folder.name, sep = ""), pattern = ".xlsx")
for (j in 1:length(file.names)){
  print(paste("processing", file.names[j]))
  
  # load data -----
  print(paste("Loading ./narratives/", folder.name, "/", file.names[j], sep = ""))
  deb <- read_excel(paste("./narratives/", folder.name, "/", file.names[j], sep = ""))
  narratives <- narratives %>% 
    add_row(filename = file.names[j], narrative = deb$narrative)
  # print(paste("Loaded ./narratives/", folder.name, "/", file.names[j], sep = ""))
  
}
narratives_df <- tibble(filename = narratives$filename,
                        text = narratives$narrative)

### processing data pairwise-----------------------------------------------------------------
## cleaning the data
# uploading the stopwords
stopwords <- read.table("stopwords-final.txt")
# View(stopwords)
stopwords_df <- tibble(word = stopwords$V1)
# removing stopwords and tokenizing
nar_file_words <- narratives_df %>%
  mutate(text = gsub(x = text, 
                     pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", 
                     replacement = "")) %>%
  unnest_tokens(word, text) %>%
  anti_join(stopwords_df, by="word")
# View(nar_file_words)  

## pairwise count
# function for looking at specific word 
pair_lookup <- function(dataset, word_lookup) {
  dataset %>%
    filter(item1 == word_lookup)
}

# count words co-occuring within documents
word_pairs <- nar_file_words %>%
  pairwise_count(word, filename, sort = TRUE)
# View(word_pairs)
# word_pairs %>% pair_lookup("pemerintah")

## pairwise correlation
# correlation 
# -> how often they appear together relative to how often they appear separately
# phi coefficient
word_cors <- nar_file_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, filename, sort = TRUE) %>%
  filter(!is.nan(correlation))
# View(word_cors)
# looking at specific word
# word_cors %>% pair_lookup("allah")

### Visualisation  -------------------------------------------------------------
set.seed(2016)
jpeg("./img/img-correlation.jpeg")
word_cors %>%
  filter(correlation > .60) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
dev.off()
# the graph shows words that are highly correlated to each other
