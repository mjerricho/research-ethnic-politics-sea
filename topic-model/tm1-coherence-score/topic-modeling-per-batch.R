### topic-modeling-per-batch.R
### topic modeling for individual file per statement
### by Marcellinus Jerricho
### last updated: 04-06-21

## import libraries ------------------------------------------------------------
# data wrangling
library(dplyr)
library(tidyr)
library(lubridate)
# visualization
library(ggplot2)
# dealing with text
library(textclean)
library(tm)
library(SnowballC)
library(stringr)
# topic model
library(tidytext)
library(topicmodels)
library(textmineR)
library(reshape2)
# data loading
library(readxl)
# wordcloud
library(wordcloud)

# build text_cleaner_tidytext -------------------------------------------------
textcleaner_tidytext <- function (x, stopwordfile, phrasesfile){
  # load stopword
  stopword_1 <- read.table(stopwordfile)
  # eliminate special characters
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  # clean text using tm package -----
  nar_cleaned <- x %>% 
    tm_map(toSpace, "/") %>%
    tm_map(toSpace, "@") %>%
    tm_map(toSpace, "\\|") %>%
    # Convert the text to lower case
    tm_map(content_transformer(tolower)) %>%
    # Remove numbers
    tm_map(removeNumbers) %>%
    # Remove stopwords
    tm_map(removeWords, stopword_1$V1)  %>%
    # Remove punctuation
    tm_map(removePunctuation) %>%
    # Eliminate extra white spaces
    tm_map(stripWhitespace)
  
  return(nar_cleaned)
}

# build textclean_textmine function
text_cleaner_textmine <- function(x, stopwordfile){
  stopwords <- read.table(stopwordfile)
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    removeWords(stopwords$V1) %>% # remove stopwords
    str_trim() %>% # removes whitespace from start and end of string
    str_squish() # reduces repeated whitespace inside a string.
  
  return(x)
  
}

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

### using textmine ------------------------------------------------------------

# apply textcleaner function for review text
deb_nar_cleaned <- text_cleaner_textmine(narratives$narrative, "stopwords-final.txt") 

#tokenize
tokens <- data.frame(id = 1:length(deb_nar_cleaned), text = deb_nar_cleaned)

#create DTM
dtm <- CreateDtm(tokens$text, 
                 doc_names = tokens$id, 
                 ngram_window = c(1, 2))

#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)

# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]

dtm = dtm

# Running LDA ------------------------------------------------------------------
k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines

#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
jpeg("./try/coherence_score.jpeg", width = 2000, height = 1000)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")
dev.off()

#select models based on max average
model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
k_best <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]$k

#1. Top 20 terms based on phi  -------------------------------------------------
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)

#3. word, topic relationship ---------------------------------------------
#looking at the terms allocated to the topic and their pr(word|topic)
allterms <- data.frame(t(model$phi))
allterms$word <- rownames(allterms)
rownames(allterms) <- 1:nrow(allterms)
allterms <- melt(allterms,idvars = "word") 
allterms <- allterms %>% rename(topic = variable)
FINAL_allterms <- allterms %>% group_by(topic) %>% arrange(desc(value))

# 2. Topic,word,freq ------------------------------------------------------
final_summary_words <- data.frame(top_terms = t(model$top_terms))
final_summary_words$topic <- rownames(final_summary_words)
rownames(final_summary_words) <- 1:nrow(final_summary_words)
final_summary_words <- final_summary_words %>% melt(id.vars = c("topic"))
final_summary_words <- final_summary_words %>% rename(word = value) %>% select(-variable)
final_summary_words <- left_join(final_summary_words,allterms)
final_summary_words <- final_summary_words %>% group_by(topic,word) %>%
  arrange(desc(value))
final_summary_words <- final_summary_words %>% group_by(topic, word) %>% filter(row_number() == 1) %>% 
  ungroup() %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
word_topic_freq <- left_join(final_summary_words, original_tf, by = c("word" = "term"))

#4. per-document-per-topic probabilities ---------------------------------------
#trying to see the topic in each document
theta_df <- data.frame(model$theta)
theta_df$document <-rownames(theta_df) 
rownames(theta_df) <- 1:nrow(theta_df)
theta_df$document <- as.numeric(theta_df$document)
theta_df <- melt(theta_df,id.vars = "document")
theta_df <- theta_df %>% rename(topic = variable) 
theta_df <- theta_df %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
FINAL_document_topic <- theta_df %>% group_by(document) %>% 
  arrange(desc(value)) %>% filter(row_number() ==1)

#5. Visualising of topics in a dendrogram --------------------------------------
#probability distributions called Hellinger distance, distance between 2 probability vectors
model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
jpeg("./try/dendogram.jpeg", width = 1000, height = 1000)
plot(model$hclust)
dev.off()


#visualising topics of words based on the max value of phi
set.seed(1234)
jpeg("./try/cluster.jpeg", width = 1000, height = 1000)
for(i in 1:length(unique(final_summary_words$topic)))
{  wordcloud(words = subset(final_summary_words ,topic == i)$word, 
             freq = subset(final_summary_words ,topic == i)$value, 
             min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, 
             colors=brewer.pal(8, "Dark2")) }

dev.off()

### using tidytext package -----------------------------------------------------
## look for the relevant words for each topic

# make a corpus out of the source
nar_corp <- VCorpus(VectorSource(narratives$narrative))

# apply textcleaner function for review text
nar_corp_cleaned <- textcleaner_tidytext(nar_corp, "stopwords-final.txt", "phrases-final.txt") 

# build a document-term matrix -------------------------------------------------
dtm <- DocumentTermMatrix(nar_corp_cleaned)
# find most frequent terms (at least appear in 10 rows)
freqterm <- findFreqTerms(dtm, 2)
freqterm
# subset the dtm to only choose those selected words
dtm <- dtm[,freqterm]
# only choose words that appear once in each rows
rownum <- apply(dtm, 2, sum)
dtm <- dtm[rownum>0,]

# applying LDA -----------------------------------------------------------------
# apply to LDA function 
lda <- LDA(dtm,k = k_best,control = list(seed = 1502))

# apply auto tidy using tidy and use beta as per-topic-per-word probabilities
topic <- tidy(lda,matrix = "beta")
# choose 15 words with highest beta from each topic
top_terms <- topic %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

# plot the topic for interpretation -------------------------------------------
plot_topic <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
jpeg("./try/term.jpeg", width = 1000, height = 1000)
plot_topic
dev.off()

