### data-scrape-demokrat.r
### by Marcellinus Jerricho
### last updated: 27/08/21

## import libraries ------------------------------------------------------------
# data loading
library("tidyverse")
library("readxl")
library("dplyr")
library("writexl")

### writing importing function -------------------------------------------------
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

### load data -----------------------------------------------------------------
demokrat_raw <- read_excel_allsheets("dpd-dpc-demokrat-raw.xlsx")

# set up the variables --------------------------------------------------------
dist_name <- character()
temp_dist_name <- character()
temp_ketua <- character()
temp_sek <- character()
temp_bend <- character()

party_leadership <- data.frame(district = character(),
                               party = character(),
                               name_ketua = character(),
                               gender_ketua = character(),
                               name_wakil = character(),
                               gender_wakil = character(),
                               name_bendahara = character(),
                               gender_bendahara = character(),
                               name_sek = character(),
                               gender_sek = character(),
                               count_male = numeric(),
                               count_female = numeric())

# accessing the sheets
districts <- names(demokrat_raw)
districts[1]
nrow(demokrat_raw[districts[1]][[districts[1]]])

# extracting through all the sheets through iteration
for (district in names(demokrat_raw)) {
  print(paste("processing", district, "-----------------------"))
  district_df <- demokrat_raw[district][[district]]
  
  for (j in 1:nrow(district_df)){
    # check for district name and update the storage for district name
    if (!is.na(district_df[j, ]$`PROV/KAB/KOTA`)) {
      # text cleaning district name
      temp_dist_name <- gsub('[[:digit:][:punct:]]+', '', 
                             district_df[j, ]$`PROV/KAB/KOTA`)
      temp_dist_name <- gsub('^ ', '', temp_dist_name) %>% tolower() 
      temp_dist_name <- gsub('  ', ' ', temp_dist_name)
      print(temp_dist_name)
      
      # update the ketua, sekretaris, bendahara
      temp_ketua <- district_df[j, ]$NAMA %>% tolower()
      temp_sek <- district_df[j + 1, ]$NAMA%>% tolower()
      temp_ben <- district_df[j + 2, ]$NAMA %>% tolower()
      
      # update the party_leadership
      party_leadership <- party_leadership %>% add_row(district = temp_dist_name,
                                                       party = "demokrat",
                                                       name_ketua = temp_ketua,
                                                       name_sek = temp_sek,
                                                       name_bendahara = temp_ben)
    }  
  }
}


# write the excel file ---------------------------------------------------------
write_xlsx(party_leadership, "party_leadership_demokrat.xlsx")




