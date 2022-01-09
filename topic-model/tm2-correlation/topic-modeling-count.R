### topic-modeling-3-count.R
### by Marcellinus Jerricho
### last updated: 04-08-21

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

### processingdata -----------------------------------------------------------------
## looking at tibble and tokenising using Tidytext
# View(narratives)
narratives_df <- tibble(text = narratives$narrative)
narratives_df <- narratives_df %>% 
  mutate(text = gsub(x = text, 
                     pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", 
                     replacement = ""))
# View(narratives_df)
# tokenizing
narratives_tokenized <- narratives_df %>% unnest_tokens(word, text)
# View(narratives_tokenized)

## removing stopwords
# uploading the stopwords
stopwords <- read.table("stopwords-final.txt")
# View(stopwords)
stopwords_df <- tibble(word = stopwords$V1)
# removing stopwords
narratives_tokenized <- narratives_tokenized %>% anti_join(stopwords_df)
# View(narratives_tokenized)

# looking for count
narratives_count <- narratives_tokenized %>% 
  count(word, sort = TRUE) 
# View(narratives_count[narratives_count$n < 100,])

### visualizing data -----------------------------------------------------------
jpeg("./img/img1-count.jpeg")
narratives_count %>% 
  filter(n > 400) %>%
  mutate(word = reorder(word,n)) %>% 
  ggplot(aes(n,word)) +
  geom_col()
dev.off()

