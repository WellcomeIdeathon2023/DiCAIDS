################################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# R session preparation - Load/Install Libraries
#
################################################################################


# these are the required packages
pkgs <- c("tidyverse",      ## Access Google Trends API
          "readr",          ## Accessing Data on Country Codes
          "compareGroups",  ## Data Management
          "kableExtra",     ## Publication Ready GGPlot
          "janitor")        ## Clean variable names


# replaced w pacman, basically the same
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

library(pacman)


p_load(pkgs, character.only = TRUE)
rm(pkgs)