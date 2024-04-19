# This code reproduces the authors' tables with new code, and provides some tables with alternative assumptions

# Compare the original tables with those labeled 'modified'.

library(pacman)
p_load(tidyverse, data.table, haven, glue, Hmisc, here, fixest, xtable)

rm(list=ls())
gc()


# read in dta file, Clara likes working with data table format

survey = read_dta(here::here("OriginalFiles", "Survey.dta"))

dts = read_dta(here::here("OriginalFiles", "Replication_Dataset.dta")) %>% data.table()


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

# Save
print(xtable(table_1),
      add.to.row = panel_titles,
      file = here("replicated_output", "tables", "Table1.tex"))


# Table 2 ------

# Keep the same choices made by the authors
library(fixest)

mod1 = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4,
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ])
summary(mod1)

mod2 = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + as.factor(female) | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod2)

mod3 = feols(vote_lega_euro ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass
             + age + female + dummy_car_unknown | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod3)

mod4 = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_lega_2018 | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, ],
             vcov = "hetero")
summary(mod4)

mod5 = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_lega_regional | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, ],
             vcov = "hetero")
summary(mod5)

mod6 = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_lega_municipal | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, ],
             vcov = "hetero")
summary(mod6)

etable(mod1,mod2,mod3,mod4,mod5,mod6,
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = here("replicated_output", "Table2.tex"))

# Table 2 with Poisson ------

mod1p = fepois(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4,
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ])
summary(mod1p)

mod2p = fepois(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + as.factor(female) | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod2p)

mod3p = fepois(vote_lega_euro ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass
             + age + female + dummy_car_unknown | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod3p)

mod4p = fepois(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_lega_2018 | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, ],
             vcov = "hetero")
summary(mod4p)

mod5p = fepois(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_lega_regional | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, ],
             vcov = "hetero")
summary(mod5p)

mod6p = fepois(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_lega_municipal | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, ],
             vcov = "hetero")
summary(mod6[])

# Save
etable(mod1p,mod2p,mod3p,mod4p,mod5p,mod6p,
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = here("replicated_output/", "Table2p.tex"))

# etable(mod1,mod2,mod3,mod4,mod5,mod6,
#   keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4")) %>%
#   kable(format = "html", escape = FALSE)

# How I would set up the specification: grouping education levels, grouping income, age as non-linear control
mod1x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4,
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ])
summary(mod1x)

mod2x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + as.factor(female) | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod2x)

mod3x = feols(vote_lega_euro ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass
             + age_cat + female + dummy_car_unknown | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod3x)

mod4x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + female + vote_lega_2018 | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, ],
             vcov = "hetero")
summary(mod4x)

mod5x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + female + vote_lega_regional | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, ],
             vcov = "hetero")
summary(mod5x)

mod6x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + female + vote_lega_municipal | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, ],
             vcov = "hetero")
summary(mod6x)

etable(mod1x,mod2x,mod3x,mod4x,mod5x,mod6x,
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = here("replicated_output", "Table2_Modified.tex"),
       notes = "\\textit{Notes:} Modified specifications. Controlling for 5 age bins instead of linear age. Observations with missing responses for education level and income dropped.")


# Table 3 ------
mod1 = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
              + age + female | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
              data = dts[target!=3 & target!=4 & no_answer_euro==0, ],
              vcov = "hetero")

mod2 = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_m5s_2018 | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, ],
             vcov = "hetero")

mod3 = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_m5s_regional | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, ],
             vcov = "hetero")

mod4 = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_m5s_municipal | EDU1 + EDU2 + EDU3 + EDU4 + profile_gross_personal_eu,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, ],
             vcov = "hetero")

etable(mod1[1],mod2[1],mod3[1],mod4[1],
       mod1[2],mod2[2],mod3[2],mod4[2],
       mod1[3],mod2[3],mod3[3],mod4[3],
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = here("replicated_output", "Table3.tex"))


# How I would set up the specification: grouping education levels, grouping income, age as non-linear control
mod1x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + female | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ],
             vcov = "hetero")

mod2x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + female + vote_m5s_2018 | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, ],
             vcov = "hetero")

mod3x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + female + vote_m5s_regional | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, ],
             vcov = "hetero")

mod4x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age_cat + female + vote_m5s_municipal | education_wo_miss + income_wo_miss,
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, ],
             vcov = "hetero")


# Save

etable(mod1x[1],mod2x[1],mod3x[1],mod4x[1],
       mod1x[2],mod2x[2],mod3x[2],mod4x[2],
       mod1x[3],mod2x[3],mod3x[3],mod4x[3],
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T, replace = T,
       file = glue("/Users/mclarars/ReplicationGames/replicated_output/tables/Table3_Modified.tex"),
       notes = "\\textit{Notes:} Modified specifications. Controlling for 5 age bins instead of linear age. Observations with missing responses for education level and income dropped.")

#Table 4 -----
euro_5_noage <- feols(vote_lega_euro ~ dummy_diesel + dummy_euro_5 + dummy_diesel * dummy_euro_5
                        | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                      data = dts[!(target %in% c(3, 4) & no_answer_euro == 0), ],
                      vcov = 'hetero')
euro_5_age <- feols(vote_lega_euro ~ dummy_diesel + dummy_euro_5 + dummy_diesel * dummy_euro_5
                    + age + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                    data = dts[!(target %in% c(3, 4) & no_answer_euro == 0), ],
                    vcov = 'hetero')
euro_5_agex <- feols(vote_lega_euro ~ dummy_diesel + dummy_euro_5 + dummy_diesel * dummy_euro_5
                     + age + I(age^2) + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                     data = dts[!(target %in% c(3, 4) & no_answer_euro == 0), ],
                     vcov = 'hetero')

etable(euro_5_noage, euro_5_age,
       keep = c("dummy_diesel", "dummy_euro_5"),
       tex = T,
       replace = T,
       file = here("replicated_output/tables/Table4.tex"),
       notes = "\\textit{Notes:} Modified specifications. Fixed effects: grouped education levels, grouped income.")

etable(euro_5_noage, euro_5_agex,
       keep = c("dummy_diesel", "dummy_euro_5"),
       tex = T,
       replace = T,
       file = here("replicated_output/tables/Table4_Modified.tex"),
       notes = "\\textit{Notes:} Modified specifications. Fixed effects: grouped education levels, grouped income. Age included as non-linear control.")



#Table 5 -----
lega_vote <- feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                     age + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                   data = dts[!(target %in% c(3, 4)) & no_answer_euro == 0, ],
                   vcov = 'hetero')
lega_votex <- feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                        age + I(age^2) + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                      data = dts[!(target %in% c(3, 4)) & no_answer_euro == 0, ],
                      vcov = 'hetero')

switch_lega_leg <- feols(switch_descriptive ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                           age + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                         data = dts[!(target %in% c(3, 4)) & no_answer_2018_rob == 0 &
                                      !vote_lega_2018, ])
switch_lega_legx <- feols(switch_descriptive ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                           age + I(age^2) + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                         data = dts[!(target %in% c(3, 4)) & no_answer_2018_rob == 0 &
                                      !vote_lega_2018, ])

switch_lega_reg <- feols(switch_descriptive_reg ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                           age + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                         data = dts[!(target %in% c(3, 4)) & no_answer_regional_rob == 0 &
                                      !vote_lega_regional, ])
switch_lega_regx <- feols(switch_descriptive_reg ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                            age + I(age^2) + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                          data = dts[!(target %in% c(3, 4)) & no_answer_regional_rob == 0 &
                                       !vote_lega_regional, ])

switch_lega_mun <- feols(switch_descriptive_mun ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                           age + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                         data = dts[!(target %in% c(3, 4)) & no_answer_municipal_rob == 0 &
                                      !vote_lega_municipal, ])
switch_lega_munx <- feols(switch_descriptive_mun ~ dummy_diesel + dummy_euro_4 + diesel_euro4 +
                            age + I(age^2) + female | EDU1 + EDU2 + EDU3 + EDU4 + income_levels,
                          data = dts[!(target %in% c(3, 4)) & no_answer_municipal_rob == 0 &
                                       !vote_lega_municipal, ])

etable(lega_vote, switch_lega_leg, switch_lega_reg, switch_lega_mun,
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       replace = T,
       file = here("replicated_output/tables/Table5.tex"),
       notes = "\\textit{Notes:} Modified specifications. Fixed effects: grouped education levels, grouped income.")

etable(lega_votex, switch_lega_legx, switch_lega_regx, switch_lega_munx,
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       replace = T,
       file = here("replicated_output/tables/Table5_Modified.tex"),
       notes = "\\textit{Notes:} Modified specifications. Fixed effects: grouped education levels, grouped income. Age included as non-linear control.")


