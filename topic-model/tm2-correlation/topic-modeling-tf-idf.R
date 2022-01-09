### topic-modeling-tf-idf.R
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

### load data for attack -----------------------------------------------------------------
narratives <- data.frame(filename = character(), narrative = character(), attack = numeric())
folder.name <- "batch_1"
# iterate through files in directory ------
file.names <- dir(paste("./narratives/",folder.name, sep = ""), pattern = ".xlsx")
for (j in 1:length(file.names)){
  print(paste("processing", file.names[j]))
  
  # load data -----
  print(paste("Loading ./narratives/", folder.name, "/", file.names[j], sep = ""))
  deb <- read_excel(paste("./narratives/", folder.name, "/", file.names[j], sep = ""))
  narratives <- narratives %>% 
    add_row(filename = file.names[j], narrative = deb$narrative, attack = deb$attack)
  # print(paste("Loaded ./narratives/", folder.name, "/", file.names[j], sep = ""))
  
}
narratives_attack <- narratives %>%
  filter(!is.na(attack)) %>%
  select(filename, narrative)  
# View(narratives_attack)

### processingdata -----------------------------------------------------------------
## looking at tibble and tokenising using Tidytext
nar_att_df <- tibble(filename = narratives_attack$filename, 
                     text = narratives_attack$narrative)
nar_att_df <- nar_att_df %>%
  mutate(text = gsub(x = text, 
                     pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", 
                     replacement = ""))
# View(nar_att_df)
# tokenizing
nar_att_tokenized <- nar_att_df %>% unnest_tokens(word, text)
# View(nar_att_tokenized)

## removing stopwords
# uploading the stopwords
stopwords <- read.table("stopwords-final.txt")
# View(stopwords)
stopwords_df <- tibble(word = stopwords$V1)
# removing stopwords
nar_att_tokenized <- nar_att_tokenized %>% anti_join(stopwords_df)
# View(nar_att_tokenized)

# looking for count
nar_att_count <- nar_att_tokenized %>% 
  count(filename, word, sort = TRUE) 
nar_att_count_total <- nar_att_count %>%
  group_by(filename) %>%
  summarize(total = sum(n))
nar_att_count <- left_join(nar_att_count, nar_att_count_total)
# View(nar_att_count)

## processing tf-idf
nar_att_tf_idf <- nar_att_count %>%
  bind_tf_idf(word, filename, n)
# View(nar_att_tf_idf)

### visualizing data -----------------------------------------------------------
# looking at terms with high tf-idf
nar_att_high_tf_idf <- nar_att_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
# View(nar_att_high_tf_idf)

# ignoring the first word: tobasa
jpeg("./img/img2-attack-relevant-words")
nar_att_high_tf_idf[2:nrow(nar_att_high_tf_idf),] %>%
  slice_max(tf_idf, n = 50) %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf))) + 
  geom_col() + 
  labs(x = "tf-idf", y = "word") + 
  ggtitle("Relevant Words (tf-idf) of attack statements")
dev.off()
