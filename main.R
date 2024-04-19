# one R file to do everything ####

# Setup ####

# Convenience package(s)
if (!require(here)) {
  install.packages("here")
  library(here)
}


# Recode from survey data
source(here::here("rebuilding_data", "rebuilding_data/BrownReplicationCleaning.R"))


# 


# render the book ####
#system("quarto render")
  
