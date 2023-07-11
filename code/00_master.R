
################################################################################
#
## Correlates of Antibody Responses to Influenza Vaccination
## DIgital and Computational Approaches to Infectious Diseases Study
#
# MASTER SCRIPT
#
################################################################################

# This is a master script that calls other scripts one-by-one
# to replicate the figure discussed in the paper. 

## Step 1: Package installation ------------------------------
source ("code/01_libraries.R")

## Step 2: Read independent connected study datasets in R-----
source ("code/02_read_datasets.R")


## Step 3: Data Linkage for each independent study data sets -
source ("code/03_link_datasets_sdy296.R")
source ("code/03_link_datasets_sdy301.R")

## Step 4: Data Wrangling and analysis for each data sets --
source ("code/04_wrangling_datasets.R")


## Step 4: Data Visualization ------------------------------
# Figures and results are saved in "/results/"
source("code/05_visualisations.R")

#' Step 6: Logistic Regression Models Examining -----------
#' the Correlates of Protections --------------------------
source("code/06_models.R")
