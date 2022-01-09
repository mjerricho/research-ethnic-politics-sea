
library(rvest)
library(dplyr)
library(textreadr)
library(stringr)

bengkulu <- read_html("https://www.pdiperjuangan.id/dpd/28/Bengkulu")

bengkulu
# View(bengkulu)
bengkulu  %>% 
  html_element("body") %>%
  html_element("div")

test <- read_rtf("bengkulu.rtf")
# View(test)

test_separated <- test %>% 
  strsplit(split = c("<p><strong>", "</strong><br />"))
# View(test_separated)
test_separated_2 <- test_separated[[1]] %>%  strsplit(split = "</strong><br />") 
test_separated_2
test_separated_3 <- test_separated_2 %>%
  str_remove_all("<strong>") %>%
  str_remove_all("&nbsp;")

test_separated_3
