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

packages_required <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats', 
                       'estimatr')

for (package in packages_required) {
  if (!(package %in% installed.packages())) {
    install.packages(package)}}     ##gglabeller not available for this version of R  

Packages <- c('ggplot2', 'tidyverse', 'broom', 'car','dplyr','modelr','haven', 'forcats', 'estimatr')

lapply(Packages, library, character.only = TRUE)     #to loud more than one package at once


################################################
# Set Working Directory and Import the dataset #
################################################

Replication_data <- read_dta("OriginalFiles/Replication_Dataset.dta")
plotpath <- ""

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

age_table <- table(Replication_data$age_rc2, Replication_data$groups)
full <- table(Replication_data$age_rc2)
age_combined <- cbind(full, age_table)
colnames(age_combined) <- c('Full Sample', 
                            'Diesel Euro 4',
                            'Diesel Euro 5', 
                            'Petrol Euro 4',
                            'Petrol Euro 5')

N <- colSums(age_combined)
age_combined <- t(apply(age_combined, MAR = 1,
                        function (x) (x / N) * 100))
age_combined <- rbind(round(age_combined,1),N)

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

income_table <- table(Replication_data$profile_gross_personal_eu_2_fac, Replication_data$groups)

full <- table(Replication_data$profile_gross_personal_eu_2_fac)

income_combined <- cbind(full, income_table)
colnames(income_combined) <- c('Full Sample', 
                               'Diesel Euro 4',
                               'Diesel Euro 5', 
                               'Petrol Euro 4',
                               'Petrol Euro 5')

# Fix up table for presentation

N <- colSums(income_combined)
income_combined <- t(apply(income_combined, MAR = 1,
                           function (x) (x / N) * 100))
income_combined <- rbind(round(income_combined,1),
                         N)

# Education

education_table <- table(Replication_data$education_fac, Replication_data$groups)
full <- table(Replication_data$education_fac)

education_combined <- cbind(full, education_table)
colnames(education_combined) <- c('Full Sample', 
                                  'Diesel Euro 4',
                                  'Diesel Euro 5', 
                                  'Petrol Euro 4',
                                  'Petrol Euro 5')

# Fix up table for presentation

N <- colSums(education_combined)
education_combined <- t(apply(education_combined, MAR = 1,
                              function (x) (x / N) * 100))
education_combined <- rbind(round(education_combined,1),
                            N)

# Gender

gender_table <- table(Replication_data$female, Replication_data$groups)
full <- table(Replication_data$female)

gender_combined <- cbind(full, gender_table)
colnames(gender_combined) <- c('Full Sample', 
                               'Diesel Euro 4',
                               'Diesel Euro 5', 
                               'Petrol Euro 4',
                               'Petrol Euro 5')

# Fix up table for presentation

N <- colSums(gender_combined)
gender_combined <- t(apply(gender_combined, MAR = 1,
                           function (x) (x / N) * 100))
gender_combined <- rbind(round(gender_combined,1),
                         N)

# Final Table 
library(xtable)

combined_table <- rbind(age_combined, income_combined, education_combined, gender_combined)


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

Figure3 <- Replication_data %>%
  dplyr::filter(target==1) %>%
  mutate(area_b_cost_fact = fct_rev(area_b_cost_fact)) %>%
  ggplot()+aes(area_b_cost_fact)+
  geom_bar(aes(y = (..count..)/sum(..count..)),
           fill="#852525", position = "dodge", width = 0.7, alpha=0.8)+
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1))+
  theme_minimal()+theme(legend.position = "none",
                        strip.text.x = element_text(size = 15),
                        axis.text.x=element_text(size=15),
                        axis.text.y=element_text(size=14), 
                        legend.text = element_text(size=14),
                        plot.margin = unit(c(1, 1, 1, 1), "cm"),
                        panel.spacing = unit(1.7, "lines"))+
  labs(x = "", y = "",
       colour="", fill="", shape="", group="" ) +coord_flip()

ggsave(Figure3, dpi = 1000,
       filename = paste0(plotpath, "Figure3_v3.png"),
       height = 4, width = 7, device = "png")


############
# Figure 4 #
############

# Panel (a) From Legislative Elections 2018  --------
# 1. no control
Legaswitch_legis_lm_ms<-lm_robust(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
                           data=Replication_data, subset=c(target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0))
# 2. Including individual controls
Legaswitch_legis_lm_cont<-lm_robust(sw_to_lega_18_19~dummy_diesel+dummy_euro_4+diesel_euro4+
                               age+female+EDU+INC, 
                             data=Replication_data, subset=c(target!=3 & target!=4 & 
                                                         no_answer_euro==0  & no_answer_2018==0))
# 3. Including unkowncar and assigning the treatment
Legaswitch_legis_lm_cont_Unknowncar<-lm_robust(sw_to_lega_18_19~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                          age+female+EDU+INC+dummy_car_unknown, 
                                        data=Replication_data, subset=c(target!=3 & no_answer_euro==0 & no_answer_2018==0))

M.Legaswitch_legis_lm_ms <-  tidy(Legaswitch_legis_lm_ms, conf.int = TRUE)
M.Legaswitch_legis_lm_cont <-  tidy(Legaswitch_legis_lm_cont, conf.int = TRUE)
M.Legaswitch_legis_lm_cont_Unknowncar <-  tidy(Legaswitch_legis_lm_cont_Unknowncar, conf.int = TRUE)

Legaswitch_legis_all<- bind_rows(M.Legaswitch_legis_lm_ms, 
                                 M.Legaswitch_legis_lm_cont, 
                                 M.Legaswitch_legis_lm_cont_Unknowncar,  
                                 .id="Model")

Legaswitch_legis_all<-subset(Legaswitch_legis_all, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                               term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
Legaswitch_legis_all<-Legaswitch_legis_all %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

Legaswitch_legis_all$term2<-factor(Legaswitch_legis_all$term2)

Legaswitch_legis_all$Model<-factor(Legaswitch_legis_all$Model, levels=c("1","2","3"),
                                   labels=c("No Controls", "With Controls", 
                                            "Incl. Noncategorized Cars"))
Legaswitch_legis_all$term3<-factor(Legaswitch_legis_all$term2, 
                                   levels =c("Euro 4", "Diesel", "Diesel Euro 4"))
# Coefplot    
coefplot_2018Legislative<-
  ggplot(Legaswitch_legis_all, aes(term3, estimate,colour=term3, fill=term3))+ 
  scale_fill_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  scale_colour_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, size=2, 
             colour = "#6C7B8B", alpha=1) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 1,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", 
                        plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"))+
  labs(x = "", y = " ", title="",  
       colour="", fill="", shape="", group="" ) 

ggsave(coefplot_2018Legislative, dpi = 400,
       filename = paste0(plotpath, "Figure4_a.png"),
       height = 4, width = 10, device = "png")

# Panel (b) From Regional Elections 2018 --------
# 1. no control
Legaswitch_region_lm_ms<-lm_robust(sw_to_lega_reg_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
                            data=Replication_data, subset=c(target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0))
# 2. with controls
Legaswitch_region_lm_cont<-lm_robust(sw_to_lega_reg_19~dummy_diesel+dummy_euro_4+diesel_euro4+
                                age+female+EDU+INC, 
                              data=Replication_data, subset=c(target!=3 & target!=4 & 
                                                          no_answer_euro==0 & no_answer_regional==0))
# 3. Assigning Unknown-car models based on self-report of ban effect
Legaswitch_region_lm_cont_Unknowncar<-lm_robust(sw_to_lega_reg_19~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                           age+female+EDU+INC+dummy_car_unknown, 
                                         data=Replication_data, subset=c(target!=3 & no_answer_euro==0 & no_answer_regional==0))

# data
M.Legaswitch_region_lm_ms <-  tidy(Legaswitch_region_lm_ms, conf.int = TRUE)
M.Legaswitch_region_lm_cont <-  tidy(Legaswitch_region_lm_cont, conf.int = TRUE)
M.Legaswitch_region_lm_cont_Unknowncar <-  tidy(Legaswitch_region_lm_cont_Unknowncar, conf.int = TRUE)
Legaswitch_region_all<- bind_rows(M.Legaswitch_region_lm_ms, 
                                  M.Legaswitch_region_lm_cont, 
                                  M.Legaswitch_region_lm_cont_Unknowncar,  .id="Model")
Legaswitch_region_all<-subset(Legaswitch_region_all, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                                term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
Legaswitch_region_all<-Legaswitch_region_all %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

Legaswitch_region_all$term2<-factor(Legaswitch_region_all$term2)

Legaswitch_region_all$Model<-factor(Legaswitch_region_all$Model, levels=c("1","2","3"),
                                    labels=c("No Controls", "With Controls", 
                                             "Incl. Noncategorized Cars"))
Legaswitch_region_all$term3<-factor(Legaswitch_region_all$term2, 
                                    levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

# coefplot
coefplot_2019Regional<-
  ggplot(Legaswitch_region_all, aes(term3, estimate,colour=term3, fill=term3))+ 
  scale_fill_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  scale_colour_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, size=2, 
             colour = "#6C7B8B", alpha=1) +
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 0.7,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"))+  
  labs(x = "", y = " ", title="",  
       colour="", fill="", shape="", group="" ) 

ggsave(coefplot_2019Regional, dpi = 400,
       filename = paste0(plotpath, "Figure4_b.png"),
       height = 4, width = 10, device = "png")

# Panel (c) From Municipal Elections 2016  ----------
# 1. no control
Legaswitch_munic_lm_ms<-lm_robust(sw_to_lega_16_19~dummy_diesel+dummy_euro_4+diesel_euro4, 
                           data=Replication_data, subset=c(target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0))

# 2. with controls
Legaswitch_munic_lm_cont<-lm_robust(sw_to_lega_16_19~dummy_diesel+dummy_euro_4+diesel_euro4+
                               age+female+EDU+INC, 
                             data=Replication_data, subset=c(target!=3 & target!=4 & 
                                                         no_answer_euro==0 & no_answer_municipal==0))

# 3. Assigning Unknown-car models based on self-report of ban effect
Legaswitch_munic_lm_cont_Unknowncar<-lm_robust(sw_to_lega_16_19~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                          age+female+EDU+INC+dummy_car_unknown, 
                                        data=Replication_data, subset=c(target!=3 & no_answer_euro==0 & no_answer_municipal==0))

# data
M.Legaswitch_munic_lm_ms <-  tidy(Legaswitch_munic_lm_ms, conf.int = TRUE)
M.Legaswitch_munic_lm_cont <-  tidy(Legaswitch_munic_lm_cont, conf.int = TRUE)
M.Legaswitch_munic_lm_cont_Unknowncar <-  tidy(Legaswitch_munic_lm_cont_Unknowncar, conf.int = TRUE)
Legaswitch_munic_all<- bind_rows(M.Legaswitch_munic_lm_ms, 
                                 M.Legaswitch_munic_lm_cont, 
                                 M.Legaswitch_munic_lm_cont_Unknowncar,  .id="Model")
Legaswitch_munic_all<-subset(Legaswitch_munic_all, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                               term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
Legaswitch_munic_all<-Legaswitch_munic_all %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

Legaswitch_munic_all$term2<-factor(Legaswitch_munic_all$term2)

Legaswitch_munic_all$Model<-factor(Legaswitch_munic_all$Model, levels=c("1","2","3"),
                                   labels=c("No Controls", "With Controls", 
                                            "Incl. Noncategorized Cars"))
Legaswitch_munic_all$term3<-factor(Legaswitch_munic_all$term2, 
                                   levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

# coefplot - 2016 Municipal Election 
coefplot_2016Municipal<-
  ggplot(Legaswitch_munic_all, aes(term3, estimate,colour=term3, fill=term3))+ 
  scale_fill_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  scale_colour_manual(values = c("#B8B8B8","#B8B8B8","#821212"))+
  geom_hline(yintercept=0, linetype="longdash", lwd=0.9, 
             colour = "#6C7B8B", alpha=1) +
  
  geom_errorbar(stat = "identity", alpha = 1, 
                position = position_dodge(width = 0.15),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.4, width=0) +
  geom_point(stat = "identity", alpha = 0.7,  size = 3, 
             position = position_dodge(width = 0.15)) + coord_flip() +facet_wrap(~Model)+
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=14.5),
                        axis.text.y=element_text(size=14.5),
                        strip.text.x = element_text(size = 14.5),
                        panel.spacing = unit(1.7, "lines"))+
  labs(x = "", y = " ", title="",  
       colour="", fill="", shape="", group="" ) 

ggsave(coefplot_2016Municipal, dpi = 400,
       filename = paste0(plotpath, "Figure4_c.png"),
       height = 4, width = 10, device = "png")


############
# Figure 6 #
############

# Panel (a) From municipal 2016 to legislative 2018  -----------
# 1. no control
pl16_18v2_lm_ms<-lm_robust(sw_to_lega_16_18~dummy_diesel+dummy_euro_4+diesel_euro4, 
                    data=Replication_data, subset=c( target!=3 & target!=4 & no_answer_2018==0 & no_answer_municipal==0 & vote_lega_municipal==0  ))
# 2. with controls
pl16_18v2_lm_cont<-lm_robust(sw_to_lega_16_18~dummy_diesel+dummy_euro_4+diesel_euro4+
                        age+female+EDU+INC, 
                      data=Replication_data, subset=c(target!=3 & target!=4 & no_answer_2018==0 & no_answer_municipal==0 & vote_lega_municipal==0))
# 3. Including unkown-car and assigning the treatment
pl16_18v2_lm_cont_Unkncar<-lm_robust(sw_to_lega_16_18~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                age+female+EDU+INC+dummy_car_unknown, 
                              data=Replication_data, subset=c(target!=3 & no_answer_2018==0 & no_answer_municipal==0 & vote_lega_municipal==0))
## Merge all models together
M.pl16_18v2_lm_ms <-  tidy(pl16_18v2_lm_ms, conf.int = TRUE)
M.pl16_18v2_lm_cont <-  tidy(pl16_18v2_lm_cont, conf.int = TRUE)
M.pl16_18v2_lm_cont_Unkncar <-  tidy(pl16_18v2_lm_cont_Unkncar, conf.int = TRUE)

M.pl16_18v_all_main<- bind_rows(M.pl16_18v2_lm_ms, 
                                M.pl16_18v2_lm_cont, 
                                M.pl16_18v2_lm_cont_Unkncar, 
                                .id="Model")
M.pl16_18v_all_main<-subset(M.pl16_18v_all_main, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                              term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
M.pl16_18v_all_main<-M.pl16_18v_all_main %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

M.pl16_18v_all_main$term2<-factor(M.pl16_18v_all_main$term2)
M.pl16_18v_all_main$Model<-factor(M.pl16_18v_all_main$Model, levels=c("1","2","3"),
                                  labels=c("No Controls", "With Controls", 
                                           "Incl. Noncategorized Cars"))
M.pl16_18v_all_main$term2<-relevel(M.pl16_18v_all_main$term2, ref="Diesel Euro 4")
M.pl16_18v_all_main$term2<-relevel(M.pl16_18v_all_main$term2, ref="Euro 4")
M.pl16_18v_all_main$term3<-factor(M.pl16_18v_all_main$term2, levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

## coefplot
M.pl16_18v_all_mainsub_Sub<-subset(M.pl16_18v_all_main, term2=="Diesel Euro 4") 
M.pl16_18v_all_mainsub_Sub$Model2<-factor(M.pl16_18v_all_mainsub_Sub$Model, levels =c("Incl. Noncategorized Cars",
                                                                                      "With Controls", "No Controls"))

# Panel (b) From municipal 2016 to regional 2018  ----------
# 1. no control
pl16_regv2_lm_ms<-lm_robust(sw_to_lega_16_reg~dummy_diesel+dummy_euro_4+diesel_euro4, 
                     data=Replication_data, subset=c( target!=3 & target!=4 & no_answer_regional==0 & no_answer_municipal==0 & vote_lega_municipal==0  ))
# 2. with controls
pl16_regv2_lm_cont<-lm_robust(sw_to_lega_16_reg~dummy_diesel+dummy_euro_4+diesel_euro4+
                         age+female+EDU+INC, 
                       data=Replication_data, subset=c(target!=3 & target!=4 & no_answer_regional==0 & no_answer_municipal==0 & vote_lega_municipal==0))
# 3. Including unkown-car and assigning the treatment
pl16_regv2_lm_cont_Unkncar<-lm_robust(sw_to_lega_16_reg~dummy_diesel_ass+dummy_euro_4_ass+diesel_euro4_ass+
                                 age+female+EDU+INC+dummy_car_unknown, 
                               data=Replication_data, subset=c(target!=3 & no_answer_regional==0 & no_answer_municipal==0 & vote_lega_municipal==0))

## Merge all models together
M.pl16_regv2_lm_ms <-  tidy(pl16_regv2_lm_ms, conf.int = TRUE)
M.pl16_regv2_lm_cont <-  tidy(pl16_regv2_lm_cont, conf.int = TRUE)
M.pl16_regv2_lm_cont_Unkncar <-  tidy(pl16_regv2_lm_cont_Unkncar, conf.int = TRUE)

M.pl16_regv_all_main<- bind_rows(M.pl16_regv2_lm_ms, 
                                 M.pl16_regv2_lm_cont, 
                                 M.pl16_regv2_lm_cont_Unkncar, 
                                 .id="Model")
M.pl16_regv_all_main<-subset(M.pl16_regv_all_main, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4"|
                               term== "dummy_diesel_ass"|term=="dummy_euro_4_ass"|term=="diesel_euro4_ass") 
M.pl16_regv_all_main<-M.pl16_regv_all_main %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

M.pl16_regv_all_main$term2<-factor(M.pl16_regv_all_main$term2)

M.pl16_regv_all_main$Model<-factor(M.pl16_regv_all_main$Model, levels=c("1","2","3"),
                                   labels=c("No Controls", 
                                            "With Controls", 
                                            "Incl. Noncategorized Cars"))
M.pl16_regv_all_main$term2<-relevel(M.pl16_regv_all_main$term2, ref="Diesel Euro 4")
M.pl16_regv_all_main$term2<-relevel(M.pl16_regv_all_main$term2, ref="Euro 4")
M.pl16_regv_all_main$term3<-factor(M.pl16_regv_all_main$term2, levels =c("Euro 4", 
                                                                         "Diesel", 
                                                                         "Diesel Euro 4"))
M.pl16_regv_all_mainsub_Sub<-subset(M.pl16_regv_all_main, term2=="Diesel Euro 4") 
M.pl16_regv_all_mainsub_Sub$Model2<-factor(M.pl16_regv_all_mainsub_Sub$Model, 
                                           levels =c("Incl. Noncategorized Cars",
                                                     "With Controls", "No Controls"))

# Coefplot 
switch_16_19_placebo<- bind_rows(M.pl16_18v_all_mainsub_Sub, 
                                 M.pl16_regv_all_mainsub_Sub, 
                                 .id="election18")

switch_16_19_placebo$election18<-factor(switch_16_19_placebo$election18)

switch_16_19_placebo$election18<-factor(switch_16_19_placebo$election18, levels=c("1","2"),
                                        labels=c("(a) From Municipal 2016 to Legislative 2018", 
                                                 "(b) From Municipal 2016 to Regional 2018"))

coef_switch_16_19_placebo<- ggplot(switch_16_19_placebo, aes(Model2, estimate))+ 
   scale_y_continuous(limits = c(-0.3, 0.4))+
  geom_hline(yintercept=0, linetype="dashed", lwd=1.2, 
             colour = "#6E7B8B", alpha=0.7) +
  geom_errorbar(stat = "identity", alpha = 0.8, 
                position = position_dodge(width = 0.3),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error),
                lwd=1.5, width = 0,  colour="#821212",)+
  geom_point(stat = "identity", alpha = 0.7,  size = 3.5,  colour="#821212",
             position = position_dodge(width = 0.15)) + coord_flip() +
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=13, colour = "#000000"),
                        axis.text.y=element_text(size=13.5, colour = "#000000"),
                        strip.text.x = element_text(size=16.5, colour = "#000000"),
                        panel.spacing = unit(2, "lines"))+
  labs(x = "", y = " ", title=" ",  
       colour="", fill="", shape="", group="" ) + facet_wrap(facets=~election18,nrow=1)

ggsave(coef_switch_16_19_placebo, dpi = 400,
       filename = paste0(plotpath, "Figure6.png"),
       height = 4, width = 12, device = "png")


############
# Figure 7 #
############

# Panel (a) Environment-friendly behavior and attitudes ------
# Short showers
short_shower_binary.lm<-lm_robust(short_shower~dummy_diesel+dummy_euro_4+diesel_euro4+
                             age+female+EDU+INC, 
                           data=Replication_data, subset=c(target!=3 & target!=4 ))

# Recycled materials
recycled_materials_binary.lm<-lm_robust(recycled_materials~dummy_diesel+dummy_euro_4+diesel_euro4+
                                   age+female+EDU+INC, 
                                 data=Replication_data, subset=c(target!=3 & target!=4  ))

# Eco mode
eco_mode_binary.lm<-lm_robust(eco_mode~dummy_diesel+dummy_euro_4+diesel_euro4+
                         age+female+EDU+INC, 
                       data=Replication_data, subset=c(target!=3 & target!=4 ))

# Reusable buttles
water_bottle_binary.lm<-lm_robust(water_bottle~dummy_diesel+dummy_euro_4+diesel_euro4+
                             age+female+EDU+INC, 
                           data=Replication_data, subset=c(target!=3 & target!=4  ))

# Climate Neutrality
climate_neutrality.lm<-lm_robust(climate_neutrality~dummy_diesel+dummy_euro_4+diesel_euro4+
                            age+female+EDU+INC, 
                          data=Replication_data, subset=c(target!=3 & target!=4  ))

# Green transition positive
green_policies.lm<-lm_robust(green_policies_positive~dummy_diesel+dummy_euro_4+diesel_euro4+
                        age+female+EDU+INC, 
                      data=Replication_data, subset=c(target!=3 & target!=4 ))

## Merge all models together
M.short_shower_binary.lm <-  tidy(short_shower_binary.lm, conf.int = TRUE)
M.recycled_materials_binary.lm <-  tidy(recycled_materials_binary.lm, conf.int = TRUE)
M.eco_mode_binary.lm <-  tidy(eco_mode_binary.lm, conf.int = TRUE)
M.water_bottle_binary.lm <-  tidy(water_bottle_binary.lm, conf.int = TRUE)
M.climate_neutrality.lm = tidy(climate_neutrality.lm, conf.int = TRUE)
M.green_policies.lm = tidy(green_policies.lm, conf.int = TRUE)

M.lm_all_ecoBehavioral<- bind_rows(M.water_bottle_binary.lm,
                                   M.eco_mode_binary.lm,
                                   M.recycled_materials_binary.lm, 
                                   M.short_shower_binary.lm,
                                   M.climate_neutrality.lm,
                                   M.green_policies.lm,
                                   .id="Model")

M.lm_all_ecoBehavioral<- bind_rows(M.recycled_materials_binary.lm,
                                   M.short_shower_binary.lm,
                                   M.eco_mode_binary.lm, 
                                   M.water_bottle_binary.lm,
                                   M.climate_neutrality.lm,
                                   M.green_policies.lm,
                                   .id="Model")

M.lm_all_ecoBehavioral<-subset(M.lm_all_ecoBehavioral, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4") 
M.lm_all_ecoBehavioral<-M.lm_all_ecoBehavioral %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

M.lm_all_ecoBehavioral$term2<-factor(M.lm_all_ecoBehavioral$term2)

M.lm_all_ecoBehavioral$Model<-factor(M.lm_all_ecoBehavioral$Model, levels=c("1","2","3","4", "5", "6"),
                                     labels=c("Recycled materials",
                                              "Short showers",
                                              "Eco mode", 
                                              "Reusable bottles", 
                                              "Climate neutrality", 
                                              "Green transition positive"
                                     ))

M.lm_all_ecoBehavioral$term2<-relevel(M.lm_all_ecoBehavioral$term2, ref="Diesel Euro 4")
M.lm_all_ecoBehavioral$term2<-relevel(M.lm_all_ecoBehavioral$term2, ref="Euro 4")
M.lm_all_ecoBehavioral$term3<-factor(M.lm_all_ecoBehavioral$term2, levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

## coefplot
M.lm_all_ecoBehavioral_sub<-subset(M.lm_all_ecoBehavioral, term2=="Diesel Euro 4") 
M.lm_all_ecoBehavioral_sub$Model2<-factor(M.lm_all_ecoBehavioral_sub$Model, levels =c("Green transition positive", "Climate neutrality",
                                                                                      "Reusable bottles",
                                                                                      "Eco mode", 
                                                                                      "Short showers", 
                                                                                      "Recycled materials"
))
coefplot_eco_Behavioral_2<-
  ggplot(M.lm_all_ecoBehavioral_sub, aes(Model2, estimate))+ 
  geom_hline(yintercept=0, linetype="dashed", lwd=1.2, 
             colour = "#6E7B8B", alpha=0.7) +
  geom_errorbar(stat = "identity", alpha = 0.8, 
                position = position_dodge(width = 0.3),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.2, width=0, colour="#36614B") +
  geom_point(stat = "identity", alpha = 1,  size = 3, 
             position = position_dodge(width = 0.15), colour="#36614B") + coord_flip() +
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=13, colour = "#000000"),
                        axis.text.y=element_text(size=13.5, colour = "#000000"))+
  labs(x = "", y = " ", title=" ",  
       colour="", fill="", shape="", group="" ) 

ggsave(coefplot_eco_Behavioral_2, dpi = 400,
       filename = paste0(plotpath, "Figure7_a.png"),
       height = 5, width = 5.5, device = "png")

# Panel (b) Global Level (ZeroCO2)  ---------
# 1. Click on website 
click<-lm_robust(dummy_zeroco2_click~dummy_diesel+dummy_euro_4+diesel_euro4+
            age+female+EDU+INC, 
          data=Replication_data, subset=c(target!=3 & target!=4))

# 2. Whatched video 
watch<-lm_robust(dummy_watch_video~dummy_diesel+dummy_euro_4+diesel_euro4+
            age+female+EDU+INC, 
          data=Replication_data, subset=c(target!=3 & target!=4))

# 3. Interest in social media
social<-lm_robust(dummy_social~dummy_diesel+dummy_euro_4+diesel_euro4+
             age+female+EDU+INC, 
           data=Replication_data, subset=c(target!=3 & target!=4))

# 4. Interest in podcast
podcast<-lm_robust(dummy_podcast~dummy_diesel+dummy_euro_4+diesel_euro4+
              age+female+EDU+INC, 
            data=Replication_data, subset=c(target!=3 & target!=4))

# 5. Interest in buying a Tree
buy<-lm_robust(dummy_buy~dummy_diesel+dummy_euro_4+diesel_euro4+
          age+female+EDU+INC, 
        data=Replication_data, subset=c(target!=3 & target!=4))

# Put all models together 
M.click <-  tidy(click, conf.int = TRUE)
M.watch <-  tidy(watch, conf.int = TRUE)
M.podcast <-  tidy(podcast, conf.int = TRUE)
M.social <-  tidy(social, conf.int = TRUE)
M.buy <-  tidy(buy, conf.int = TRUE)

M.lm_all_main<- bind_rows(M.click, 
                          M.watch,
                          M.social,
                          M.podcast, 
                          M.buy,
                          .id="Model")

M.lm_all_main<-subset(M.lm_all_main, term=="dummy_diesel"| 
                        term=="dummy_euro_4"|term=="diesel_euro4") 
M.lm_all_main<-M.lm_all_main %>%
  mutate(term2=case_when(term=="dummy_diesel" ~"Diesel",
                         term=="dummy_euro_4" ~"Euro 4",
                         term=="diesel_euro4" ~"Diesel Euro 4"))

M.lm_all_main$term2<-factor(M.lm_all_main$term2)

M.lm_all_main$Model<-factor(M.lm_all_main$Model, levels=c("1","2","3","4","5"),
                            labels=c("Clicked on website", "Watched video", 
                                     "Interest in social media", 
                                     "Interest in podcast", 
                                     "Interest in buying a tree"))

M.lm_all_main$term2<-relevel(M.lm_all_main$term2, ref="Diesel Euro 4")
M.lm_all_main$term2<-relevel(M.lm_all_main$term2, ref="Euro 4")
M.lm_all_main$term3<-factor(M.lm_all_main$term2, levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

M.lm_all_main_sub<-subset(M.lm_all_main, term2=="Diesel Euro 4") 
M.lm_all_main_sub$Model2<-factor(M.lm_all_main_sub$Model, levels =c("Interest in buying a tree",
                                                                    "Interest in podcast",
                                                                    "Interest in social media",
                                                                    "Watched video",
                                                                    "Clicked on website"))
# coefplot
coefplot_ZeroCO2<-
  ggplot(M.lm_all_main_sub, aes(Model2, estimate, fill=Model2, colour=Model2))+ 
  geom_hline(yintercept=0, linetype="dashed", lwd=1.2, 
             colour = "#6E7B8B", alpha= 0.7) +
  geom_errorbar(stat = "identity", alpha = 0.8, 
                position = position_dodge(width = 0.3),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.9, width=0, colour="#36614B") +
  geom_point(stat = "identity", alpha =1,  size = 4, 
             position = position_dodge(width = 0.15),colour="#36614B") + coord_flip() +
  theme_minimal()+theme(legend.position = "none",plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=16, colour = "#000000"),
                        axis.text.y=element_text(size=16, colour = "#000000"))+
  labs(x = "", y = " ", title=" ",  
       colour="", fill="", shape="", group="" ) 

ggsave(coefplot_ZeroCO2, dpi = 1000,
       filename = paste0(plotpath, "Figure7_b.png"),
       height = 5, width = 6, device = "png")

# Panel (c) Local Level (Genitori Antismog) 
dummy_genitori_click.lm<-lm_robust(dummy_genitori_click~dummy_diesel+dummy_euro_4+diesel_euro4+
                              age+female+EDU+INC, 
                            data=Replication_data, subset=c(target!=3 & target!=4))

dummy_newsletter.lm<-lm_robust(dummy_newsletter~dummy_diesel+dummy_euro_4+diesel_euro4+
                          age+female+EDU+INC, 
                        data=Replication_data, subset=c(target!=3 & target!=4 ))

dummy_donation.lm<-lm_robust(dummy_donation~dummy_diesel+dummy_euro_4+diesel_euro4+
                        age+female+EDU+INC, 
                      data=Replication_data, subset=c(target!=3 & target!=4 ))

# Put all models together 
M.dummy_genitori_click.lm <-  tidy(dummy_genitori_click.lm, conf.int = TRUE)
M.dummy_newsletter.lm <-  tidy(dummy_newsletter.lm, conf.int = TRUE)
M.dummy_donation.lm <-  tidy(dummy_donation.lm, conf.int = TRUE)

M.lm_all_Behavioral<- bind_rows(
  M.dummy_genitori_click.lm,
  M.dummy_newsletter.lm,
  M.dummy_donation.lm,
  .id="Model")

M.lm_all_Behavioral<-subset(M.lm_all_Behavioral, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4") 
M.lm_all_Behavioral<-M.lm_all_Behavioral %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

M.lm_all_Behavioral$term2<-factor(M.lm_all_Behavioral$term2)

M.lm_all_Behavioral$Model<-factor(M.lm_all_Behavioral$Model, levels=c("1","2","3"),
                                  labels=c( "Clicked on website", 
                                            "Interest in newsletter",
                                            "Interest in donating" ))

M.lm_all_Behavioral$term2<-relevel(M.lm_all_Behavioral$term2, ref="Diesel Euro 4")
M.lm_all_Behavioral$term2<-relevel(M.lm_all_Behavioral$term2, ref="Euro 4")
M.lm_all_Behavioral$term3<-factor(M.lm_all_Behavioral$term2, levels =c("Euro 4", "Diesel", "Diesel Euro 4"))

## coefplot -
M.lm_all_Behavioral_sub<-subset(M.lm_all_Behavioral, term2=="Diesel Euro 4") 
M.lm_all_Behavioral_sub$Model2<-factor(M.lm_all_Behavioral_sub$Model, levels =c("Interest in donating",
                                                                                "Interest in newsletter",
                                                                                "Clicked on website"))

coefplot_Genitorianti_smog<-
  ggplot(M.lm_all_Behavioral_sub, aes(Model2, estimate))+ 
  geom_hline(yintercept=0, linetype="dashed", lwd=1.2, 
             colour = "#6E7B8B", alpha=0.7) +
  geom_errorbar(stat = "identity", alpha = 0.8, 
                position = position_dodge(width = 0.3),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.9, width=0, colour="#36614B") +
  geom_point(stat = "identity", alpha = 0.7,  size = 4.7, 
             position = position_dodge(width = 0.15)) + coord_flip() +
  geom_point(stat = "identity", alpha = 0.7,  size = 4, 
             position = position_dodge(width = 0.15), colour="#36614B") + coord_flip() +
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=16, colour = "#000000"),
                        axis.text.y=element_text(size=16, colour = "#000000"))+
  labs(x = "", y = " ", title=" ",  
       colour="", fill="", shape="", group="" ) 

ggsave(coefplot_Genitorianti_smog, dpi = 1000,
       filename = paste0(plotpath, "Figure7_c.png"),
       height = 5, width = 6, device = "png")


############
# Figure 8 #
############

pay_eco_friendly_con<-lm_robust(pay_eco_friendly~dummy_diesel+dummy_euro_4+diesel_euro4+
                                  age+female+EDU+INC, 
                                data=Replication_data, subset=c(target!=3 & target!=4))
M.pay_eco_friendly_con <-  tidy(pay_eco_friendly_con, conf.int = TRUE)


taxes_eco_friendly_con<-lm_robust(taxes_eco_friendly~dummy_diesel+dummy_euro_4+diesel_euro4+
                                    age+female+EDU+INC, 
                                  data=Replication_data, subset=c(target!=3 & target!=4))
M.taxes_eco_friendly_con <-  tidy(taxes_eco_friendly_con, conf.int = TRUE)


responsibility_cont_main<-lm_robust(gov_firms_responsibility~dummy_diesel+dummy_euro_4+diesel_euro4+
                               age+female+EDU+INC, 
                             data=Replication_data, subset=c(target!=3 & target!=4 ))
M.responsibility_cont_main <-  tidy(responsibility_cont_main, conf.int = TRUE)


M.lm_all_ptr <- bind_rows(M.responsibility_cont_main,
                          M.taxes_eco_friendly_con,
                          M.pay_eco_friendly_con, 
                          .id="Model")
M.lm_all_ptr<-subset(M.lm_all_ptr, term=="dummy_diesel"|term=="dummy_euro_4"|term=="diesel_euro4") 
M.lm_all_ptr<-M.lm_all_ptr %>%
  mutate(term2=case_when(term=="dummy_diesel" |term=="dummy_diesel_ass"~"Diesel",
                         term=="dummy_euro_4" |term=="dummy_euro_4_ass"~"Euro 4",
                         term=="diesel_euro4" |term=="diesel_euro4_ass"~"Diesel Euro 4"))

M.lm_all_ptr$term2<-factor(M.lm_all_ptr$term2)

M.lm_all_ptr$Model<-factor(M.lm_all_ptr$Model, levels=c("1","2","3"),
                           labels=c("Govt./Firms Responsibility", "Higher Taxes", 
                                    "Higher Prices"))
M.lm_all_ptr$term2<-relevel(M.lm_all_ptr$term2, ref="Diesel Euro 4")
M.lm_all_ptr$term2<-relevel(M.lm_all_ptr$term2, ref="Euro 4")
M.lm_all_ptr$term3<-factor(M.lm_all_ptr$term2, levels =c("Euro 4", "Diesel", "Diesel Euro 4"))
M.lm_all_ptr_sub<-subset(M.lm_all_ptr, term2=="Diesel Euro 4") 
M.lm_all_ptr_sub$Model2<-factor(M.lm_all_ptr_sub$Model, levels =c( "Higher Prices", 
                                                                   "Higher Taxes", 
                                                                   "Govt./Firms Responsibility"))

coefplot_ptr<-
  ggplot(M.lm_all_ptr_sub, aes(Model2, estimate, fill="#CD2626", color="#CD2626"))+ 
   geom_hline(yintercept=0, linetype="dashed", lwd=1.2, 
             colour = "#6E7B8B", alpha=0.7) +
  geom_errorbar(stat = "identity", alpha = 0.8, 
                position = position_dodge(width = 0.3),
                aes(ymin=estimate - 1.96*std.error, 
                    ymax=estimate + 1.96*std.error), 
                lwd=1.9, width=0, colour="#821212") +
  geom_point(stat = "identity", alpha = 0.7,  size = 4, 
             position = position_dodge(width = 0.15), colour="#821212") + coord_flip() +
  theme_minimal()+theme(legend.position = "none", plot.background = element_rect(fill = "white"),
                        axis.text.x=element_text(size=16, colour = "#000000"),
                        axis.text.y=element_text(size=16, colour = "#000000"))+
  
  labs(x = "", y = " ", title=" ",  
       colour="", fill="", shape="", group="" ) 

ggsave(coefplot_ptr, dpi = 400,
       filename = paste0(plotpath, "Figure8.png"),
       height = 5, width = 7, device = "png")


