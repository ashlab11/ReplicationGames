library(pacman)
p_load(tidyverse, data.table, haven, glue, Hmisc)

rm(list=ls())
gc()

path_to_project = "/Users/mclarars/ReplicationGames"


# read in dta file
survey = read_dta(glue("{path_to_project}/OriginalFiles/Survey.dta"))
dts    = read_dta(glue("{path_to_project}/OriginalFiles/Replication_Dataset.dta")) %>% data.table()

# describe datasets
Hmisc::describe(survey) # ??
# summary(survey)

# Hmisc::describe(dts) # ??
summary(dts)


# ============================================================================ #
# Section 2: Reproduce tables -----
# ============================================================================ #

# Table 1 -------

### Panel A
dts[, summary(age)]
dts[, age_18_24 := age %in% 18:24]
dts[, age_25_34 := age %in% 25:34]
dts[, age_35_44 := age %in% 35:44]
dts[, age_45_54 := age %in% 45:54]
dts[, age_55_above := age >= 55]

age_indicators = names(dts)[names(dts) %like% "age_"]
age_fullsample = dts[, lapply(.SD, mean), .SDcols = age_indicators] %>% .[, sample := "Full Sample"]
age_diesel_euro4 = dts[diesel_euro4 == T, lapply(.SD, mean), .SDcols = age_indicators] %>% .[, sample := "Diesel-Euro4"]
age_diesel_euro5 = dts[diesel_euro5 == T, lapply(.SD, mean), .SDcols = age_indicators] %>% .[, sample := "Diesel-Euro5"]
age_petrol_euro4 = dts[diesel_euro4 == F, lapply(.SD, mean), .SDcols = age_indicators] %>% .[, sample := "Petrol-Euro4"]
age_petrol_euro5 = dts[diesel_euro5 == F, lapply(.SD, mean), .SDcols = age_indicators] %>% .[, sample := "Petrol-Euro5"]

panel_a = rbindlist(list(age_fullsample,age_diesel_euro4,age_diesel_euro5,age_petrol_euro4,age_petrol_euro5)) %>%
  melt(id.vars = "sample") %>%
  dcast(variable ~ sample, value.var = "value")

### Panel B
dts[, summary(female)]

female_fullsample = dts[, .(Female = mean(female),Male = mean(!female))] %>% .[, sample := "Full Sample"]
female_diesel_euro4 = dts[diesel_euro4 == T, .(Female = mean(female),Male = mean(!female))] %>% .[, sample := "Diesel-Euro4"]
female_diesel_euro5 = dts[diesel_euro5 == T, .(Female = mean(female),Male = mean(!female))] %>% .[, sample := "Diesel-Euro5"]
female_petrol_euro4 = dts[diesel_euro4 == F, .(Female = mean(female),Male = mean(!female))] %>% .[, sample := "Petrol-Euro4"]
female_petrol_euro5 = dts[diesel_euro5 == F, .(Female = mean(female),Male = mean(!female))] %>% .[, sample := "Petrol-Euro5"]

panel_b = rbindlist(list(female_fullsample,
                         female_diesel_euro4,female_diesel_euro5,
                         female_petrol_euro4,female_petrol_euro5)) %>%
  melt(id.vars = "sample") %>%
  dcast(variable ~ sample, value.var = "value")

### Panel C
dts[, summary(education_level_it_original)]

dts[, education_levels_original := case_when(
  education_level_it_original < 7 ~        "High School",
  education_level_it_original %in% c(7,9) ~   "Bachelors",
  education_level_it_original %in% c(8,10:12) ~  "MA and higher",
  education_level_it_original > 13 ~       "Unknown",
)]

dts[, education_levels_alt := case_when(
  education_level_it_original < 7 ~        "High School",
  education_level_it_original %in% 7:8 ~   "Bachelors",
  education_level_it_original %in% 9:12 ~  "MA and higher",
  education_level_it_original > 13 ~       "Unknown",
)]

educ_fullsample = dts[, prop.table(table(education_levels_original))] %>% data.table() %>% .[, sample := "Full Sample"]
educ_diesel_euro4 = dts[diesel_euro4 == T, prop.table(table(education_levels_original))] %>% data.table() %>% .[, sample := "Diesel-Euro4"]
educ_diesel_euro5 = dts[diesel_euro5 == T, prop.table(table(education_levels_original))] %>% data.table() %>% .[, sample := "Diesel-Euro5"]
educ_petrol_euro4 = dts[diesel_euro4 == F, prop.table(table(education_levels_original))] %>% data.table() %>% .[, sample := "Petrol-Euro4"]
educ_petrol_euro5 = dts[diesel_euro5 == F, prop.table(table(education_levels_original))] %>% data.table() %>% .[, sample := "Petrol-Euro5"]

panel_c_original = rbindlist(list(educ_fullsample,
                                  educ_diesel_euro4,educ_diesel_euro5,
                                  educ_petrol_euro4,educ_petrol_euro5)) %>%
  dcast(education_levels_original ~ sample, value.var = "N")

educ_fullsample = dts[, prop.table(table(education_levels_alt))] %>% data.table() %>% .[, sample := "Full Sample"]
educ_diesel_euro4 = dts[diesel_euro4 == T, prop.table(table(education_levels_alt))] %>% data.table() %>% .[, sample := "Diesel-Euro4"]
educ_diesel_euro5 = dts[diesel_euro5 == T, prop.table(table(education_levels_alt))] %>% data.table() %>% .[, sample := "Diesel-Euro5"]
educ_petrol_euro4 = dts[diesel_euro4 == F, prop.table(table(education_levels_alt))] %>% data.table() %>% .[, sample := "Petrol-Euro4"]
educ_petrol_euro5 = dts[diesel_euro5 == F, prop.table(table(education_levels_alt))] %>% data.table() %>% .[, sample := "Petrol-Euro5"]

panel_c_alt = rbindlist(list(educ_fullsample,
                             educ_diesel_euro4,educ_diesel_euro5,
                             educ_petrol_euro4,educ_petrol_euro5)) %>%
  dcast(education_levels_alt ~ sample, value.var = "N")

# dts[, table(education_levels_original,education_levels_alt)]

### Panel D
dts[, summary(profile_gross_personal_eu)]

dts[, income_levels := case_when(
  profile_gross_personal_eu %in% 1:3 ~   "Below EUR 14,999 per year",
  profile_gross_personal_eu %in% 4:6 ~   "Between EUR 15,000-29,999 per year",
  profile_gross_personal_eu %in% 7:9 ~   "Between EUR 30,000-49,999 per year",
  profile_gross_personal_eu %in% 10:12 ~ "Between EUR 45,000-69,999 per year",
  profile_gross_personal_eu %in% 13:14 ~ "Above EUR 70,000 per year",
  profile_gross_personal_eu > 14 ~       "Don't know/Prefer not to say",
)]

inc_fullsample = dts[, prop.table(table(income_levels))] %>% data.table() %>% .[, sample := "Full Sample"]
inc_diesel_euro4 = dts[diesel_euro4 == T, prop.table(table(income_levels))] %>% data.table() %>% .[, sample := "Diesel-Euro4"]
inc_diesel_euro5 = dts[diesel_euro5 == T, prop.table(table(income_levels))] %>% data.table() %>% .[, sample := "Diesel-Euro5"]
inc_petrol_euro4 = dts[diesel_euro4 == F, prop.table(table(income_levels))] %>% data.table() %>% .[, sample := "Petrol-Euro4"]
inc_petrol_euro5 = dts[diesel_euro5 == F, prop.table(table(income_levels))] %>% data.table() %>% .[, sample := "Petrol-Euro5"]

panel_d = rbindlist(list(inc_fullsample,
                         inc_diesel_euro4,inc_diesel_euro5,
                         inc_petrol_euro4,inc_petrol_euro5)) %>%
  dcast(income_levels ~ sample, value.var = "N")

### Combine all to make Table 1
table_1 = rbindlist(list(panel_a,panel_b,panel_c_original,panel_c_alt,panel_d))
setcolorder(table_1, c("variable", "Full Sample", "Diesel-Euro4", "Diesel-Euro5", "Petrol-Euro4", "Petrol-Euro5"))

panel_titles = list()
panel_titles$pos <- list(0,
                         nrow(panel_a),
                         nrow(panel_a)+nrow(panel_b),
                         nrow(panel_a)+nrow(panel_b)+nrow(panel_c_original),
                         nrow(panel_a)+nrow(panel_b)+nrow(panel_c_original)+nrow(panel_c_alt))

panel_titles$command <- c("Panel A & \\multicolumn{5}{c}{} \\\\ \n",
                          "Panel B & \\multicolumn{5}{c}{} \\\\ \n",
                          "Panel C (Paper version) & \\multicolumn{5}{c}{} \\\\ \n",
                          "Panel C (Alt.) & \\multicolumn{5}{c}{} \\\\ \n",
                          "Panel D & \\multicolumn{5}{c}{} \\\\ \n")

print(xtable(table_1),
      add.to.row = panel_titles,
      file = glue("/Users/mclarars/ReplicationGames/our_reproduction/Table1.tex"))



# Table 2 ------
library(fixest)

mod1 = lm(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4, data = dts[target!=3 & target!=4 & no_answer_euro==0, ])
summary(mod1)

mod1_allrespondents = lm(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4, data = dts[no_answer_euro==0, ])
summary(mod1_allrespondents)
