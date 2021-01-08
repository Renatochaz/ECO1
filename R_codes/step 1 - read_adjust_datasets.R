### Reading all datasets, making adjustments and checking integrity of information
### Read all comments to see all adjustments done

## Starting packages
{
  library(readr)
  library(dplyr)
  library(stringi)
}

## Setting working directory
setwd("G:/Meu Drive/GIT/ECO1/raw_data")

## Starting string of base name of files
name_base_files <- c("dados_st2000_")

## Reading all data as UTF-8, data.frame, excluding first 2 columns and 
## columns as: 1 = char, 2:26 = numeric
## Creating vector of datasets names
# Setting a index column for quarters which will have value 1 to 80, indicating quarter
count_datasets <- 0

vec_names_dfs <- 0

for (i in 1:80) {
  
  db <- read.csv(paste0(name_base_files,i,".csv"), stringsAsFactors = FALSE)
  
  colnames(db) <- stri_encode(colnames(db), "", "UTF-8")
  
  db[ ,3] <- as.factor(db[ ,3])
  
  db <- db %>% mutate_if (is.character, as.numeric)
  
  db <- db %>% mutate_if (is.factor, as.character)
  
  db$Quarter <- i
  db <- db[ , c(1:3,29,4:28)]
  
  db[db == 0] <- NA
  
  count_datasets <- count_datasets +1
  
  assign(paste0("dataset",i), db[ , 3:29])
  
  vec_names_dfs <- c(vec_names_dfs, paste0("dataset",i))
  
  rm(db)
  
}

vec_names_dfs <- vec_names_dfs[-c(1)]

str(dataset1)
str(dataset80)

## Function to check wrong information (repeated columns = wrong data collect)
## Test should be TRUE

fc_get_colnames <- function (x) {
  
  vec_names <- 0
  
  for (i in 1:count_datasets) {
    
    namedb <- paste0("dataset",i)
    tmp <- get(namedb)
    vec_names <- c(vec_names, colnames(tmp)[3:27])
    
  }
  
  return(vec_names[-c(1)])
  
}
fc_check_information <- function (x) {
  
  vec_colnames <- fc_get_colnames(x)
  
  test <- length(vec_colnames) == length(unique(vec_colnames))
  
  return (test)
  
}

fc_check_information(x)
