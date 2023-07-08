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
    list_csv_files <- list.files(path)
    list_csv_files <- list_csv_files[!list_csv_files == "Protocols"]
    ColNums_NotAllMissing <- function(df){ # helper function
      as.vector(which(colSums(is.na(df)) != nrow(df)))
    }
    
    for (i in list_csv_files) {
      
      df <- read.csv(paste0(path, i)) %>%
            select(ColNums_NotAllMissing(.)) %>% 
            select (-c(X))
      label <- str_remove(i, ".csv")
      
      assign(paste0(label, "_296"), df)
      
    }

#### Read the second study dataset [SDY301] into R.
    
    path = "data/SDY301/"
    list_csv_files <- list.files(path)
    list_csv_files <- list_csv_files[!list_csv_files == "Protocols"]
    
    ColNums_NotAllMissing <- function(df){ # helper function
      as.vector(which(colSums(is.na(df)) != nrow(df)))
    }
    
    for (i in list_csv_files) {
      
      df <- read.csv(paste0(path, i)) %>%
        select(ColNums_NotAllMissing(.)) %>% 
        select (-c(X))
      label <- str_remove(i, ".csv")
      
      assign(paste0(label, "_301"), df)
      
    }
