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

# Save
print(xtable(table_1),
      add.to.row = panel_titles,
      file = glue("/Users/mclarars/ReplicationGames/our_reproduction/Table1.tex"))



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

# Save
etable(mod1,mod2,mod3,mod4,mod5,mod6,
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = glue("/Users/mclarars/ReplicationGames/our_reproduction/Table2.tex"))

# How I would set up the specification: grouping education levels, grouping income, age as non-linear control
mod1x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ])
summary(mod1x)

mod2x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + as.factor(age) + as.factor(female) | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod2x)

mod3x = feols(vote_lega_euro ~ dummy_diesel_ass + dummy_euro_4_ass + diesel_euro4_ass
             + as.factor(age) + female + dummy_car_unknown | education_levels_alt + income_levels, 
             data = dts[target!=3 & no_answer_euro==0, ],
             vcov = "hetero")
summary(mod3x)

mod4x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + as.factor(age) + female + vote_lega_2018 | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, ],
             vcov = "hetero")
summary(mod4x)

mod5x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + as.factor(age) + female + vote_lega_regional | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, ],
             vcov = "hetero")
summary(mod5x)

mod6x = feols(vote_lega_euro ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + as.factor(age) + female + vote_lega_municipal | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, ],
             vcov = "hetero")
summary(mod6x)

# Save
etable(mod1x,mod2x,mod3x,mod4x,mod5x,mod6x,
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = glue("/Users/mclarars/ReplicationGames/our_reproduction/Table2_Modified.tex"),
       notes = "\\textit{Notes:} Modified specifications. Fixed effects: grouped education levels, grouped income. Age included as non-linear control.")


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

# Save
etable(mod1[1],mod2[1],mod3[1],mod4[1],
       mod1[2],mod2[2],mod3[2],mod4[2],
       mod1[3],mod2[3],mod3[3],mod4[3],
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = glue("/Users/mclarars/ReplicationGames/our_reproduction/Table3.tex"))


# How I would set up the specification: grouping education levels, grouping income, age as non-linear control
mod1x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0, ],
             vcov = "hetero")

mod2x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_m5s_2018 | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, ],
             vcov = "hetero")

mod3x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_m5s_regional | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, ],
             vcov = "hetero")

mod4x = feols(c(vote_pd_euro,vote_forzaitalia_euro,vote_m5s_euro) ~ dummy_diesel + dummy_euro_4 + dummy_diesel*dummy_euro_4
             + age + female + vote_m5s_municipal | education_levels_alt + income_levels, 
             data = dts[target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, ],
             vcov = "hetero")


# Save
etable(mod1x[1],mod2x[1],mod3x[1],mod4x[1],
       mod1x[2],mod2x[2],mod3x[2],mod4x[2],
       mod1x[3],mod2x[3],mod3x[3],mod4x[3],
       keep = c("dummy_diesel", "dummy_euro_4", "diesel_euro4"),
       tex = T,
       file = glue("/Users/mclarars/ReplicationGames/our_reproduction/Table3_Modified.tex"),
       notes = "\\textit{Notes:} Modified specifications. Fixed effects: grouped education levels, grouped income. Age included as non-linear control.")
