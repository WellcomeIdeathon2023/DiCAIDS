################################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# R session preparation - Load/Install Libraries
#
################################################################################

# these are the required packages
pkgs <- c("dplyr",          ## Data cleaning and wrangling
          "stringr",        ## String manipulation
          "readr",          ## Read data into R
          "compareGroups",  ## Bivariate publication-ready table
          "kableExtra",     ## Publication ready-tables
          "janitor",        ## Clean variable names
          "colorspace",     ## For sequential fill color in visualisations
          "showtext")       ## For importing fonts into the R session.


# replaced w pacman, to load libraries or install them if they haven't been installed.
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

library(pacman)


p_load(pkgs, character.only = TRUE)
rm(pkgs)