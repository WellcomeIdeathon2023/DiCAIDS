#######################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# Read Study Datasets | SDY296 - 2011-12 | SDY301 - 2012-13
#
#######################################################################

rm(list = ls())

#### Read the first study dataset [SDY296] into R.

    path = "data/SDY296/"
    list_csv_files <- list.files(path) ## get all files within the study path.
    list_csv_files <- list_csv_files[!list_csv_files == "Protocols"] ## exclude the protocol folder from the list of files.
    ColNums_NotAllMissing <- function(df){ # R-function to identify columns in which all data is missing and remove them from the data.
      as.vector(which(colSums(is.na(df)) != nrow(df)))
    }
    
    for (i in list_csv_files) {
      # Read dataset into R and remove file type (.csv) from file name.
      df <- read.csv(paste0(path, i)) %>%
            select(ColNums_NotAllMissing(.)) %>% 
            select (-c(X))
      label <- str_remove(i, ".csv")
      
      ## Assign dataset name (SDY296) to all datasets read into R session.
      assign(paste0(label, "_296"), df)
      
    }

#### Read the second study dataset [SDY301] into R.
    
    path = "data/SDY301/"
    list_csv_files <- list.files(path)     ## get all files within the study path.
    list_csv_files <- list_csv_files[!list_csv_files == "Protocols"]  ## exclude the protocol folder from the list of files.
    
    ColNums_NotAllMissing <- function(df){ # R-function to identify columns in which all data is missing and remove them from the data.
      as.vector(which(colSums(is.na(df)) != nrow(df)))
    }
    
    for (i in list_csv_files) {
      
      # Read dataset into R and remove file type (.csv) from file name.
      df <- read.csv(paste0(path, i)) %>%
        select(ColNums_NotAllMissing(.)) %>% 
        select (-c(X))
      label <- str_remove(i, ".csv")
      
      ## Assign dataset name (SDY301) to all datasets read into R session.
      assign(paste0(label, "_301"), df)
      
    }
