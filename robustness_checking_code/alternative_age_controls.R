# +++++++++++++++++++++++++++++++++++++++++++++++++++++
# REPLICATE TABLE 2 WITH ALTERNATIVE AGE CONTROLS +++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++
# FCC20240419

# load libraries
library(data.table)
library(tidyverse)
library(haven)
library(fixest)
library(mice)
library(Hmisc)
library(here)

# load data
data_replication <- fread(here("constructed_data/replication_data_extra_vars.csv"))


# ___Table 2 with binned age dummies ------
# without controls
mod1 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4,
  vcov = "hetero"
)
summary(mod1)

# baseline
mod2 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  | age_cat
  + female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod2)

# including unknown car
mod3 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3),
      no_answer_euro == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + dummy_car_unknown
  | age_cat
  + female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod3)

# baseline, including control for past vote for Lega: Legislative 2018
mod4 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
      no_answer_2018 == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + vote_lega_2018
  | age_cat
  + female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod4)

# baseline, including control for past vote for Lega: Regional 2018
mod5 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
      no_answer_regional == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + vote_lega_regional
  | age_cat
  + female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod5)

# baseline, including control for past vote for Lega: Municipal 2018
mod6 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
      no_answer_municipal == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + vote_lega_municipal
  | age_cat
  + female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod6)

# save as tex table
etable(
  mod1, mod2, mod3, mod4, mod5, mod6,
  keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
  tex = T,
  file = here("robustness_checking_output/Table2_age_cat.tex")
)


# ___Table 2 with quadratic age controls ------
# without controls
mod1 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4,
  vcov = "hetero"
)
summary(mod1)

# baseline
mod2 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + age
  + age^2
  | female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod2)

# including unknown car
mod3 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3),
      no_answer_euro == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4_ass
  + dummy_diesel_ass
  + dummy_euro_4_ass
  + dummy_car_unknown
  + age
  + age^2
  | female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod3)

# baseline, including control for past vote for Lega: Legislative 2018
mod4 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
      no_answer_2018 == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + vote_lega_2018
  + age
  + age^2
  | female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod4)

# baseline, including control for past vote for Lega: Regional 2018
mod5 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
      no_answer_regional == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + vote_lega_regional
  + age
  + age^2
  | female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod5)

# baseline, including control for past vote for Lega: Municipal 2018
mod6 <- feols(
  data = data_replication %>%
    filter(
      !target %in% c(3, 4),
      no_answer_euro == 0,
      no_answer_municipal == 0,
    ),
  fml = 
    vote_lega_euro ~
    diesel_euro4
  + dummy_diesel
  + dummy_euro_4
  + vote_lega_municipal
  + age
  + age^2
  | female
  + education_raw
  + income_raw,
  vcov = "hetero"
)
summary(mod6)

# save as tex table
etable(
  mod1, mod2, mod3, mod4, mod5, mod6,
  keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
  tex = T,
  file = here("robustness_checking_output", "Table2_age_quadratic.tex")
)
gc()


