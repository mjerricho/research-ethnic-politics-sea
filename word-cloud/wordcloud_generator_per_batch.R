# wordcloud_generator_per_batch.R 
# latest version: 10/03/21
# load libraries -----
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("readxl")
library("stringr")
library("tools")
library("dplyr")
library("tidyverse")

# iterate through each batch in directory -----
folder.names <- dir("./narratives")
for (i in 1:length(folder.names)){
  print(paste("processing", folder.names[i]))
  narratives <- data.frame(narrative = character())
  
  # iterate through files in directory ------
  file.names <- dir(paste("./narratives/",folder.names[i], sep = ""), pattern = ".xlsx")
  for (j in 1:length(file.names)){
    print(paste("processing", file.names[j]))
    
    # load data -----
    print(paste("Loading ./narratives/", folder.names[i], "/", file.names[j], sep = ""))
    deb <- read_excel(paste("./narratives/", folder.names[i], "/", file.names[j], sep = ""))
    narratives <- narratives %>% add_row(narrative = deb$narrative)
    # print(paste("Loaded ./narratives/", folder.names[i], "/", file.names[j], sep = ""))
    
  }
  
  # load the data as a corpus
  nar <- Corpus(VectorSource(narratives$narrative))
  # print(paste("Loaded ./narratives/", folder.names[i], "/", file.names[j], sep = ""))
  
  # clean text using tm package -----
  # eliminate special characters
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  nar_trans <- tm_map(nar, toSpace, "/")
  nar_trans <- tm_map(nar_trans, toSpace, "@")
  nar_trans <- tm_map(nar_trans, toSpace, "\\|")
  
  # Convert the text to lower case
  nar_trans <- tm_map(nar_trans, content_transformer(tolower))
  
  # Remove numbers
  nar_trans <- tm_map(nar_trans, removeNumbers)
  # print(paste("lower cased and number and special characters eliminated ./narratives/", folder.names[i], "/", file.names[j], sep = ""))
  
  # Remove stopword
  stopword_1 <- read.table("stopwords-final.txt")
  
  # in case we want to create our own list of stop words
  # stopword_2 <- read.table("./ID-Stopwords-master/stopword-edited.txt")
  
  # specify your stopwords as a character vector
  nar_trans <- tm_map(nar_trans, removeWords, stopword_1$V1) 
  
  # Remove punctuation
  nar_trans <- tm_map(nar_trans, removePunctuation)
  
  # Eliminate extra white spaces
  nar_trans <- tm_map(nar_trans, stripWhitespace)
  
  # print(paste("cleaned ./narratives/", folder.names[i], "/", file.names[j], sep = ""))
  
  # count the frequency of phrases -----
  # import phrases
  phrases <- read.table("phrases-final.txt")$V1
  phrasecount <- numeric(length(phrases))
  for (k in 1:length(nar_trans)) {
    if (!is.na(nar_trans[[k]]$content)) {
      # print(paste(k, str_count(nar_trans[[k]]$content, pattern = phrases)))
      phrasecount <- phrasecount+
        str_count(nar_trans[[k]]$content, pattern = phrases)
    }
  }
  
  phrasesum <- data.frame(row.names = phrases, 
                          word = phrases, 
                          freq = phrasecount)
  
  # print(paste("phrase included ./narratives/", folder.names[i], "/", file.names[j], sep = ""))
  
  # build a term-document matrix from tm package -----
  dtm <- TermDocumentMatrix(nar_trans)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  # Binding with phrases and frequency
  d_binded <- rbind(d, phrasesum)
  d_binded_sorted <- d_binded[order(-d_binded$freq),]
  
  # print(paste("words and phrase frequency ./narratives/", folder.names[i], "/", file.names[j], sep = ""))
  # save as all the terms as terms.txt
  # write.table(d_binded, "terms.txt", row.names = FALSE)
  
  # generate word cloud -----
  # set seed for reproducibility
  set.seed(1234)
  
  # Save wordcloud as jpeg 
  jpeg(paste("./wordcloud/", folder.names[i],".jpg", sep = ""), width = 1000, height = 1000)
  wordcloud(words = d_binded_sorted$word, freq = d_binded_sorted$freq, 
            min.freq = 5,max.words=100, random.order=FALSE, rot.per=0.3, 
            colors=brewer.pal(8, "Dark2"), scale = c(10,3))
  dev.off()
  print(paste("Wordcloud generated ", folder.names[i], ".jpg saved", sep = ""))
  
}

# for statistical analysis -----
# findFreqTerms(dtm, lowfreq = 50)
# assoc <- findAssocs(dtm, terms = "terima", corlimit = 0.5)
# write.table(assoc, "assoc.txt")
# barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
#         col ="lightblue", main ="Most frequent words",
#         ylab = "Word frequencies")
