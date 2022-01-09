## import libraries ------------------------------------------------------------
# data loading
library(readxl)
# data wrangling
library(dplyr)
#string
library(stringr)

### load data -----------------------------------------------------------------
folder.name <- "batch_1"
container <- data.frame(replace = character())
# iterate through files in directory ------
file.names <- dir(paste("./narratives/",folder.name, sep = ""), pattern = ".xlsx")
for (j in 1:length(file.names)){
  print(paste("processing", file.names[j]))
  
  # load data -----
  print(paste("Loading ./narratives/", folder.name, "/", file.names[j], sep = ""))
  nar_file <- read_excel(paste("./narratives/", folder.name, "/", file.names[j], sep = ""))
  
  # add the import line
  container <- container %>%
    add_row(replace = paste0("import excel using ${pathraw}", 
                             file.names[j],
                           ", firstrow clear\n"))
  
  # add the replace speaker_id lines
  speaker_names <- unique(nar_file$speaker_name)
  for (i in 1:length(speaker_names)) {
    container <- container %>% 
      add_row(replace = paste0("replace speaker_id = ", 
                               i, ' if speaker_name == "', speaker_names[i], '"'))
  }
  
  # extracting information from file names
  file_name_sep <- str_split(file.names[j], "_")
  verifier_name = file_name_sep[[1]][4]
  dta_file = str_remove(paste0("narrative_",
                               str_remove(file_name_sep[[1]][2], "kab"),
                               file_name_sep[[1]][3],
                               "_verified.dta"
                               ), " ")
  dta_path = paste0("saveold ${pathraw}", 
                    dta_file,
                    ", replace")
  # add the verifier lines
  container <- container %>%
    add_row(replace = paste0("\ngen verifier = .")) %>%
    add_row(replace = paste0("tostring verifier, replace")) %>%
    add_row(replace = paste0('replace verifier = "', verifier_name, '"\n')) %>%
    add_row(replace = paste0('drop if speaker_name == ""\n')) %>%
    add_row(replace = paste0(dta_path, "\n\n\n\n\n"))
  
}
write.table(container, "test.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
container

# tryfile <- read_excel("./narratives/batch_1/narrative_kabbanggai_2015_bryan_verified.xlsx")
# names <- unique(tryfile$speaker_name)
# container <- data.frame(replace = character())
# for (i in 1:length(names)) {
#   container <- container %>% 
#     add_row(replace = paste0("replace speaker_id = ", 
#                              i, ' if speaker_name == "', names[i], '"')) %>% 
#     add_row(replace = "\n")
#   
# }
# 
# write.table(container, "test.txt", quote = FALSE, row.names = FALSE)
# container
# 
# # looking at files name
# test_file_name <- "narrative_kabbanggai_2015_bryan_verified.xlsx"
# test_file_name_sep <- str_split(test_file_name,"_")
# test_file_name_sep[[1]][4]
# test_file_name_sep_df <- data.frame(words = test_file_name_sep)
# test_file_name_sep_df
# 
# str_remove(test_file_name, "kab")
# str_remove("narrative_kabbogor_2018_bryan_verified.xlsx", "kab")
