#Getting packages they use

packages_required <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats', 
                       'estimatr')

for (package in packages_required) {
  if (!(package %in% installed.packages())) {
    install.packages(package)}}     ##gglabeller not available for this version of R  

Packages <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats', 'estimatr')

lapply(Packages, library, character.only = TRUE) 

survey_data <- read_dta("OriginalFiles/Survey.dta")

