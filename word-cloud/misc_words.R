library(dplyr)

# editing stopwords
# stopword_base <- read.table("./ID-Stopwords-master/id.stopwords.02.01.2016.txt")
stopword_base_2 <- read.table("./ID-Stopwords-master/stopword-edited.txt")
stopword_addition <- read.table("stopwords-31-03.txt")
stopword_combined <- rbind(stopword_base_2, stopword_addition)
stopword_combined_sorted <- data.frame(sort(stopword_combined$V1)) %>% distinct()
write.table(stopword_combined_sorted, "stopwords-final.txt", col.names = FALSE, row.names = FALSE)
stopwords_check <- read.table("stopwords-final.txt")
View(stopwords_check)

#ediitng two-words clauses
phrases_district <- read.table("./phrases/districts/phrases-district.txt", sep = "\n")
phrases_others <- read.table("./phrases/phrases-others.txt", sep = "\n")
phrases_combined <- rbind(phrases_district, phrases_others)
phrases_combined_sorted <- data.frame(sort(phrases_combined$V1)) %>% distinct()
write.table(phrases_combined_sorted, "phrases-final.txt", col.names = FALSE, row.names = FALSE)
phrases_check <- read.table("phrases-final.txt")
View(phrases_check)
