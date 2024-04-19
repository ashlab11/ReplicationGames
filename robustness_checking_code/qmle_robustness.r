
# This R-Script allows to replicate Table 1, Figure 3, and all the coefficient plots presented in the paper.

# The analysis was performed on R Version 4.1.2

#####################
# Clear environment #
#####################

rm(list = ls())
gc()


###################################
# Installing and Loading Packages #
###################################

packages_required <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats')

for (package in packages_required) {
  if (!(package %in% installed.packages())) {
    install.packages(package)}}     ##gglabeller not available for this version of R  

Packages <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats')

lapply(Packages, library, character.only = TRUE)     #to loud more than one package at once


################################################
# Set Working Directory and Import the dataset #
################################################

tabpath <- "insert here"
Replication_data <- read_dta("Replication_Dataset.dta")


###########
# Table 1 #
###########

Replication_data<-Replication_data %>%
  mutate(education=case_when(education_level_it_original<7~1,
                             education_level_it_original==7 |education_level_it_original==9~2,
                             education_level_it_original==8 | education_level_it_original==10 | education_level_it_original==11 | education_level_it_original==12~3,
                             education_level_it_original==13 | education_level_it_original==14~4))

Replication_data$education_fac<-factor(Replication_data$education,
                                 levels=c(1,2,3,4),
                                 labels=c("High school diploma", 
                                          "Bachelors", 
                                          "MA or higher", 
                                          "Unknown"))

Replication_data$EDU<-factor(Replication_data$education_fac)
Replication_data$INC<-factor(Replication_data$profile_gross_personal_eu)

# Groups
Replication_data<-Replication_data%>%
  mutate(groups=case_when(
    dummy_euro_4==1 & dummy_diesel==1~"Diesel Euro 4",
    dummy_euro_5==1 & dummy_diesel==1~"Diesel Euro 5",
    dummy_euro_4==1 & dummy_petrol==1~"Petrol Euro 4",
    dummy_euro_5==1 & dummy_petrol==1~"Petrol Euro 5"))

Replication_data<-Replication_data %>%
  mutate(age_rc=case_when(age==18 | age==19 | age==20 |age==21 | age==22 | age==23 | age==24 ~1,
                             age==25 | age==26 | age==27 |age==28 | age==29 |age==30 | age==31 |age==32 | age==33 |age==34 ~2,
                             age==35 | age==36 | age==37 |age==38 | age==39 |age==40 | age==41 |age==42 | age==43 |age==44 ~3,
                             age==45 | age==46 | age==47 |age==48 | age==49 |age==50 | age==51 |age==52 | age==53 |age==54 ~4,
                             age==55 | age>55~5))

Replication_data$age_rc2<-factor(Replication_data$age_rc,
                           levels=c(1,2,3,4,5),
                           labels=c("18-24", 
                                    "25-34", 
                                    "35-44",
                                    "45-54",
                                    "55+"))


# Income
Replication_data<-Replication_data %>%
  mutate(profile_gross_personal_eu_2=case_when(profile_gross_personal_eu==1 | profile_gross_personal_eu==2 | profile_gross_personal_eu==3~1,
                                               profile_gross_personal_eu==4 | profile_gross_personal_eu==5 | profile_gross_personal_eu==6~2,
                                               profile_gross_personal_eu==7 | profile_gross_personal_eu==8 | profile_gross_personal_eu==9~3,
                                               profile_gross_personal_eu==10| profile_gross_personal_eu==11| profile_gross_personal_eu==12~4,
                                               profile_gross_personal_eu==13| profile_gross_personal_eu==14~5,
                                               profile_gross_personal_eu==98| profile_gross_personal_eu==99~6))

Replication_data$profile_gross_personal_eu_2_fac<-factor(Replication_data$profile_gross_personal_eu_2,
                                                   levels=c(1,2,3,4,5,6),
                                                   labels=c("Less than 14.999 \u20ac per year", 
                                                            "From 15.000 \u20ac to 29.999 \u20ac per year", 
                                                            "From 30.000 \u20ac to 44.999 \u20ac per year",
                                                            "From 45.000 \u20ac 69.999 \u20ac per year", 
                                                            "From 70.000 \u20ac and more", 
                                                            "No Answer / DK"))

############
# Figure 3 #
############

Replication_data$diesel_euro4_fac<-factor(Replication_data$diesel_euro4)
Replication_data$area_b_cost_fact<-factor(Replication_data$cost_area_b, 
                                    levels=c(1,2,3,4,5,6,7,8),
                                    labels = c("No cost",
                                               "Less than \u20ac500", 
                                               "\u20ac500 - \u20ac1,500", 
                                               "\u20ac1,500  - \u20ac2,500",
                                               "\u20ac2,500 - \u20ac5,000",
                                               "\u20ac5,000 - \u20ac10,000", 
                                               "Above \u20ac10,000", 
                                               "Don't know"))



## QMLE Model
qmle_model_ms <- glm(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
 data=Replication_data, subset=c(target!=3 & target!=4 &
 no_answer_euro==0 & no_answer_2018==0), family = poisson)

qmle_model_cont <-glm(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+diesel_euro4+
 age+female+EDU+INC, data=Replication_data, subset=c(target!=3 & target!=4 & 
 no_answer_euro==0  & no_answer_2018==0), family = poisson)

qmle_model_cont_Unknowncar <-glm(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+
 diesel_euro4+age+female+EDU+INC+dummy_car_unknown, data=Replication_data,
 subset=c(target!=3 & target!=4 & no_answer_euro==0  & no_answer_2018==0),
 family = poisson)