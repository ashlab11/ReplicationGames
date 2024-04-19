# This script `BrownReplicationCleaning.R` reconstructs the replication data from the survey data, and checks it against the provided data.

# Convenience package(s)
if (!require(here)) {
  install.packages("here")
  library(here)
}


#Getting packages they use

packages_required <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats', 
                       'estimatr')

for (package in packages_required) {
  if (!(package %in% installed.packages())) {
    install.packages(package)}}     ##gglabeller not available for this version of R  

Packages <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats', 'estimatr')

lapply(Packages, library, character.only = TRUE) 

survey_data <- read_dta("OriginalFiles/Survey.dta")

our_data <- survey_data %>%
  select(age, class, fuel)

#Climate neutrality
our_data$climate_neutrality <- ifelse(survey_data$q_neutrality > 3, 1, 0)

#Compensation
our_data$compensated <- ifelse(
  survey_data$q_comp_new_car == 0 & 
  survey_data$q_comp_used_car == 0 & 
  survey_data$q_comp_bike == 0 &
  survey_data$q_comp_subscription == 0 & 
  survey_data$q_comp_motorcycle == 0, 
  0, 
  1
)

#Cost 
#8 is DONT KNOW! Maybe remove it since it's technically NA
our_data$cost_area_b = survey_data$cost_area_b


#Fuel / Class dummy columns
our_data$target = survey_data$target
our_data$dummy_euro_4 = survey_data$class == 5
our_data$dummy_euro_5 = survey_data$class == 6
our_data$dummy_diesel = survey_data$fuel == 1
our_data$dummy_petrol = survey_data$fuel == 2
our_data$dummy_car_unknown = survey_data$target == 4
our_data$dummy_euro_4_ass = with(our_data, 
                                 dummy_euro_4 | (dummy_car_unknown & survey_data$ban == 1))
our_data$dummy_diesel_ass = with(our_data, 
                                 dummy_diesel | (dummy_car_unknown & survey_data$ban == 1))
#Later do assigned columns

our_data <- our_data %>% 
  mutate(diesel_euro4 = dummy_euro_4 * dummy_diesel, 
         diesel_euro5 = dummy_euro_5 * dummy_diesel, 
         diesel_euro4_ass = dummy_euro_4_ass * dummy_diesel_ass
         )

#Environmental Variables
our_data$dummy_buy <- survey_data$dummy_buy
our_data$dummy_genitori_click <- survey_data$dummy_genitori_click
our_data$dummy_newsletter <- survey_data$dummy_newsletter
our_data$dummy_social <- survey_data$dummy_social
our_data$dummy_podcast <- survey_data$dummy_podcast
our_data$dummy_watch_video <- survey_data$dummy_watch_video
our_data$dummy_donation <- survey_data$dummy_donation
our_data$dummy_zeroco2_click <- survey_data$dummy_zeroco2_click
our_data$eco_mode <- 6 - survey_data$q_eco_mode
our_data$recycled_materials <- 6 - survey_data$q_recycled
our_data$short_shower <- 6 - survey_data$q_showers
our_data$water_bottle <- 6 - survey_data$q_bottles
our_data$gov_firms_responsibility <- survey_data$q_responsibility > 3
our_data$green_policies_positive <- survey_data$q_policies_positive > 3
our_data$pay_eco_friendly <- survey_data$q_prices < 3
our_data$taxes_eco_friendly <- survey_data$q_taxes < 3


#Education Level
our_data$education_level_it_original <- survey_data$education_level_it_original
education_level_it <- with(survey_data, ifelse(education_level_it_original < 7, 1,
                                         ifelse(education_level_it_original %in% c(7, 9), 2,
                                          ifelse(education_level_it_original %in% c(8, 10, 11, 12), 3,
                                          ifelse(education_level_it_original %in% c(13, 14), 4, NA)))))

EDU <- model.matrix(~ factor(education_level_it) - 1, data = our_data)

# Renaming dummy variables to include the 'EDU' prefix
colnames(EDU) <- paste0("EDU", colnames(EDU))
colnames(EDU) <- str_replace(colnames(EDU), "factor\\(education_level_it\\)", "")

# Binding the dummy variables back to the original data frame
our_data <- cbind(our_data, EDU)

#DEMOGRAPHICS
our_data$female <- survey_data$gender == 2
INC <- model.matrix(~ factor(profile_gross_personal_eu) - 1, data = survey_data)
colnames(INC) <- paste0("INC", colnames(INC)) %>%
  str_replace(., "factor\\(profile_gross_personal_eu\\)", "") 
colnames(INC) <- ifelse(colnames(INC) == "INC98", "INC15", 
                        ifelse(colnames(INC) == "INC99", "INC16", colnames(INC)))

our_data <- cbind(our_data, INC)

#CAR USE
our_data <- our_data %>%
  mutate(
    kmdontknow = ifelse(survey_data$q_kms == 997, 1, 0),
    km_less_1k = ifelse(survey_data$q_kms == 1, 1, 0),
    km_1k_to_5k = ifelse(survey_data$q_kms == 2, 1, 0),
    km_5k_to_10k = ifelse(survey_data$q_kms == 3, 1, 0),
    km_10k_to_20k = ifelse(survey_data$q_kms == 4, 1, 0),
    km_20k_to_30k = ifelse(survey_data$q_kms == 5, 1, 0),
    km_more_30k = ifelse(survey_data$q_kms == 6, 1, 0),
    ten_km = ifelse(survey_data$q_kms %in% c(4, 5, 6), 1, 0), 
    use_day = survey_data$q_car_use == 6, 
    use_week = survey_data$q_car_use == 7, 
    use_month = survey_data$q_car_use == 8, 
    use_year = survey_data$q_car_use == 9, 
    everyweek = survey_data$q_car_use %in% c(6, 7)
  )

our_data <- our_data %>%
  mutate(
    workcar_day = survey_data$q_use_work == 1, 
    workcar_week = survey_data$q_use_work == 2, 
    workcar_month = survey_data$q_use_work == 3, 
    workcar_year = survey_data$q_use_work == 4, 
    work_everyweek = survey_data$q_use_work %in% c(1, 2)
  )


#ELECTION INFORMATION
#999 means a person did not vote in the given election
our_data <- our_data %>%
  mutate(
  no_answer_2018 = ifelse(survey_data$vote_2018 %in% c(994, 997, 999), 1, 0),
  no_answer_2018_rob = ifelse(survey_data$vote_2018 %in% c(994, 997), 1, 0),
  no_answer_euro = ifelse(survey_data$vote_euro %in% c(994, 997, 999), 1, 0),
  no_answer_municipal = ifelse(survey_data$vote_municipal %in% c(994, 997, 999), 1, 0),
  no_answer_municipal_rob = ifelse(survey_data$vote_municipal %in% c(994, 997), 1, 0),
  no_answer_regional = ifelse(survey_data$vote_regional %in% c(994, 997, 999), 1, 0),
  no_answer_regional_rob = ifelse(survey_data$vote_regional %in% c(994, 997), 1, 0)
)

our_data <- our_data %>%
  mutate(
    vote_forzaitalia_2018 = survey_data$vote_2018 == 2, 
    vote_forzaitalia_euro = survey_data$vote_euro == 2, 
    vote_forzaitalia_municipal = survey_data$vote_municipal == 2, 
    vote_forzaitalia_regional = survey_data$vote_regional == 2, 
    vote_lega_2018 = survey_data$vote_2018 == 1, 
    vote_lega_euro = survey_data$vote_euro == 1, 
    vote_lega_municipal = survey_data$vote_municipal == 1, 
    vote_lega_regional = survey_data$vote_regional == 1, 
    vote_m5s_2018 = survey_data$vote_2018 == 5, 
    vote_m5s_euro = survey_data$vote_euro == 5, 
    vote_m5s_municipal = survey_data$vote_municipal == 5, 
    vote_m5s_regional = survey_data$vote_regional == 6,
    vote_pd_2018 = survey_data$vote_2018 == 6, 
    vote_pd_euro = survey_data$vote_euro == 6, 
    vote_pd_municipal = survey_data$vote_municipal == 6, 
    vote_pd_regional = survey_data$vote_regional == 6
  )

our_data <- our_data %>% 
  mutate(
    sw_to_lega_16_18 = case_when(
      !vote_lega_municipal & vote_lega_2018 ~ 1, 
      !vote_lega_municipal & !vote_lega_2018 & (no_answer_municipal == 0) ~ 0, 
      TRUE ~ NA_real_), 
    sw_to_lega_16_19 = case_when(
      !vote_lega_municipal & vote_lega_euro ~ 1, 
      !vote_lega_municipal & !vote_lega_euro & (no_answer_municipal == 0) ~ 0, 
      TRUE ~ NA_real_), 
    sw_to_lega_16_reg = case_when(
      !vote_lega_municipal & vote_lega_regional ~ 1, 
      !vote_lega_municipal & !vote_lega_regional & (no_answer_municipal == 0) ~ 0, 
      TRUE ~ NA_real_),
    sw_to_lega_18_19 = case_when(
      !vote_lega_2018 & vote_lega_euro ~ 1, 
      !vote_lega_2018 & !vote_lega_euro & (no_answer_2018 == 0) ~ 0, 
      TRUE ~ NA_real_), 
    sw_to_lega_reg_19 = case_when(
      !vote_lega_regional & vote_lega_euro ~ 1, 
      !vote_lega_regional & !vote_lega_euro & (no_answer_regional == 0) ~ 0, 
      TRUE ~ NA_real_),
    switch_descriptive = case_when(
      !vote_lega_2018 & vote_lega_euro ~ 1, 
      !vote_lega_2018 & !vote_lega_euro & (no_answer_2018_rob == 0) ~ 0, 
      TRUE ~ NA_real_
    ), 
    switch_descriptive_mun = case_when(
      !vote_lega_municipal & vote_lega_euro ~ 1, 
      !vote_lega_municipal & !vote_lega_euro & (no_answer_municipal_rob == 0) ~ 0, 
      TRUE ~ NA_real_), 
    switch_descriptive_reg = case_when(
      !vote_lega_regional & vote_lega_euro ~ 1, 
      !vote_lega_regional & !vote_lega_euro & (no_answer_regional_rob == 0) ~ 0, 
      TRUE ~ NA_real_)
  )

our_data$profile_gross_personal_eu <- survey_data$profile_gross_personal_eu

our_data <- our_data %>%
  mutate(across(where(is.logical), as.integer))


if (!dir.exists("../constructed_data")) {
  dir.create("../constructed_data")
}
write_dta(our_data, here::here("../constructed_data", "OurReplication_dta.dta"))

