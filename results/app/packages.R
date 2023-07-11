

# these are the required packages
pkgs <- c("shiny",
          "shinydashboard",
          "shinydashboardPlus",
          "DT",
          "fontawesome",
          "compareGroups",
          "kableExtra",
          "tidyverse",
          "janitor",
          "mosaic",
          "shinybusy",
          "colorspace",
          "showtext",
          "shinycssloaders")


# replaced w pacman, to load libraries or install them if they haven't been installed.
if (!require("pacman", character.only = TRUE)){
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}

library(pacman)


p_load(pkgs, character.only = TRUE)
rm(pkgs)


