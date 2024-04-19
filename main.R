# one R file to do everything ####

# Setup ####

# Convenience package(s)
if (!require(here)) {
  install.packages("here")
  library(here)
}

# Recode from survey data
source(here::here("rebuilding_data", "rebuilding_data/BrownReplicationCleaning.R"))
#this also checks that the constructed data set is the same as the one provided

# Next we run the authors' provided Stata code (cannot easily be run in this interface)
# Run "OriginalFiles/Replication_Code_Stata.do". 
#Output is put into the file `stata_replication_log...`

# Run authors' R code for main resulsts, with minor modifications for paths and packages:

source(here::here("replication_code_and_logs", "Replication_Code_R_minor_fixes.R"))

# Recoded R code with some adjustment

# render the book ####
#system("quarto render")
  
