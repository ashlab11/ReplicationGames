# +++++++++++++++++++++++++++++++++++++++++++++++
# REPLICATE REGRESSION USING IMPUTED INCOME +++++
# +++++++++++++++++++++++++++++++++++++++++++++++
# FCC20240419

# load libraries
library(data.table)
library(tidyverse)
library(haven)
library(fixest)
library(mice)
library(Hmisc)
library(here)

# load survey data from replication package
data_replication <- read_dta(here("constructed_data/OurReplication_dta.dta"))

# prepare education variable
data_replication$education_raw <- as.character(
  data_replication$EDU1 * 1
  + data_replication$EDU2 * 2
  + data_replication$EDU3 * 3
  + data_replication$EDU4 * 4
)
data_replication$education_wo_miss <- data_replication$education_raw
data_replication$education_wo_miss[data_replication$EDU4 == 1] <- NA

# prepare income variable
data_replication$income_raw <- as.character(data_replication$profile_gross_personal_eu)
data_replication$income_wo_miss <- ifelse(
  data_replication$profile_gross_personal_eu > 90,
  NA,
  data_replication$income_raw
)
describe(data_replication$income_wo_miss)

# generate age group dummies
data_replication$age_18_24 <- data_replication$age %in% c(18:24)
data_replication$age_25_34 <- data_replication$age %in% c(25:34)
data_replication$age_35_44 <- data_replication$age %in% c(35:44)
data_replication$age_45_54 <- data_replication$age %in% c(45:54)
data_replication$age_55_plus <- data_replication$age >= 55
data_replication$age_cat <- as.character(
  data_replication$age_18_24 * 1
  + data_replication$age_25_34 * 2
  + data_replication$age_35_44 * 3
  + data_replication$age_45_54 * 4
  + data_replication$age_55_plus * 5
)

# impute income
data_replication$income_f <- factor(data_replication$income_wo_miss)
data_replication$income_imputed <- ifelse(
  is.na(data_replication$income_wo_miss),
  mice.impute.rf(
    data_replication$income_f,
    !is.na(data_replication$education_wo_miss) & !is.na(data_replication$income_wo_miss),
    data_replication[, c("age", "female", "education_raw")],
    meth = "rf"
  ),
  data_replication$income_wo_miss
)
describe(data_replication$income_wo_miss)
describe(data_replication$income_imputed)

# impute education
data_replication$education_f <- factor(data_replication$education_wo_miss)
data_replication$education_imputed <- ifelse(
  is.na(data_replication$education_wo_miss),
  mice.impute.rf(
    data_replication$education_f,
    !is.na(data_replication$education_wo_miss) & !is.na(data_replication$income_wo_miss),
    data_replication[, c("age", "female", "income_raw")],
    meth = "rf"
  ),
  data_replication$education_wo_miss
)
describe(data_replication$education_wo_miss)
describe(data_replication$education_imputed)


# ___save data with new variables -----
fwrite(
  data_replication,
  here("constructed_data/replication_data_extra_vars.csv"),
  na = NA
)
gc()


