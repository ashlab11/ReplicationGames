
* This do-file allows to replicate all the analyses presented in the paper

* It was performed on Stata Version 17

use Replication_Dataset.dta,clear


* Set up globals

global main "diesel_euro4 dummy_diesel dummy_euro_4"

global controls "age female EDU* INC*"

global main_ass "diesel_euro4_ass dummy_diesel_ass dummy_euro_4_ass"

global main_placebo "diesel_euro5 dummy_diesel dummy_euro_5"


***********
* Table 1 *
***********

* Table 1 can be produced in R, please see Replication_Code_R.R


***********
* Table 2 *
***********

* Without controls

regress vote_lega_euro ${main} if target!=3 & target!=4 & no_answer_euro==0, robust

* Baseline

regress vote_lega_euro ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

* Including unkown-car 

regress vote_lega_euro ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_euro==0, robust

* Baseline, including control for past vote for Lega: Legislative 2018

regress vote_lega_euro ${main} ${controls} vote_lega_2018 if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust 

* Baseline, including control for past vote for Lega: Regional 2018

regress vote_lega_euro ${main} ${controls} vote_lega_regional if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, robust

* Baseline, including control for past vote for Lega: Municipal 2018

regress vote_lega_euro ${main} ${controls} vote_lega_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, robust


***********
* Table 3 *
***********

*----*
* PD *
*----*

* Baseline

regress vote_pd_euro ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

* Baseline, including control for past vote: Legislative 2018

regress vote_pd_euro ${main} ${controls} vote_pd_2018 if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

* Baseline, including control for past vote: Regional 2018

regress vote_pd_euro ${main} ${controls} vote_pd_regional if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, robust 

* Baseline, including control for past vote: Municipal 2016

regress vote_pd_euro ${main} ${controls} vote_pd_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, robust 

*--------------*
* Forza Italia *
*--------------*

* Baseline

regress vote_forzaitalia_euro ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

* Baseline, including control for past vote: Legislative 2018

regress vote_forzaitalia_euro ${main} ${controls} vote_forzaitalia_2018 if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust 

* Baseline, including control for past vote: Regional 2018

regress vote_forzaitalia_euro ${main} ${controls} vote_forzaitalia_regional if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, robust 

* Baseline, including control for past vote: Municipal 2016

regress vote_forzaitalia_euro ${main} ${controls} vote_forzaitalia_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, robust 

*------------*
* Five Stars *
*------------*

* Baseline

regress vote_m5s_euro ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

* Baseline, including control for past vote: Legislative 2018

regress vote_m5s_euro ${main} ${controls} vote_m5s_2018 if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust 

* Baseline, including control for past vote: Regional 2018

regress vote_m5s_euro ${main} ${controls} vote_m5s_regional if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, robust 

* Baseline, including control for past vote: Municipal 2016

regress vote_m5s_euro ${main} ${controls} vote_m5s_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, robust 


***********
* Table 4 *
***********

* Without controls

regress vote_lega_euro ${main_placebo} if target!=1 & ((target==2 & class==6) | target==3) & target!=4 & no_answer_euro==0, robust

* With controls

regress vote_lega_euro ${main_placebo} ${controls} if target!=1 & ((target==2 & class==6) | target==3) & target!=4 & no_answer_euro==0, robust


***********
* Table 5 *
***********

* Baseline

regress vote_lega_euro compensated ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

* Switching from legislative

regress sw_to_lega_18_19 compensated ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

* Switching from regional

regress sw_to_lega_reg_19 compensated ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, robust

* Switching from municipality

regress sw_to_lega_16_19 compensated ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, robust


**************
* Table S1-1 *
**************

* Lega

regress vote_lega_euro ${main} ${controls} vote_lega_2018 vote_lega_regional vote_lega_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0 & no_answer_regional==0 & no_answer_municipal==0, robust 

* PD

regress vote_pd_euro ${main} ${controls} vote_pd_2018 vote_pd_regional vote_pd_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0 & no_answer_regional==0 & no_answer_municipal==0, robust

* Forza Italia

regress vote_forzaitalia_euro ${main} ${controls} vote_forzaitalia_2018 vote_forzaitalia_regional vote_forzaitalia_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0 & no_answer_regional==0 & no_answer_municipal==0, robust 

* 5 Star

regress vote_m5s_euro ${main} ${controls} vote_m5s_2018 vote_m5s_regional vote_m5s_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0 & no_answer_regional==0 & no_answer_municipal==0, robust 


*************************
* Table SI-2 - Figure 4 *
*************************

*=======================*
* From legislative 2018 *
*=======================*

* Without controls

regress sw_to_lega_18_19 ${main} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

* Including individual controls

regress sw_to_lega_18_19 ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

* Including unkown-car 

regress sw_to_lega_18_19 ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_euro==0 & no_answer_2018==0, robust

*====================*
* From regional 2018 *
*====================*

* Without controls

regress sw_to_lega_reg_19 ${main} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, robust

* Including individual controls

regress sw_to_lega_reg_19 ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0, robust

* Including unkown-car 

regress sw_to_lega_reg_19 ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_euro==0 & no_answer_regional==0, robust 

*=====================*
* From municipal 2016 *
*=====================*

* Without controls

regress sw_to_lega_16_19 ${main} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, robust

* Including individual controls

regress sw_to_lega_16_19 ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0, robust

* Including unkown-car 

regress sw_to_lega_16_19 ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_euro==0 & no_answer_municipal==0, robust


****************************************
* Figure 5, and also Figures SI-2 SI-3 *
****************************************

* Figure 5 can be generated using data available in the file Data_Figure_5.xlsx

* To generate the figure one needs to use https://flourish.studio

* Specifically, the following steps have to be followed: 

* (1) click on "New visualization"

* (2) select graph type "Sankey diagram"

* (3) select version "Alluvial"

* (4) a sample graph will pop up

* (5) click on "Data", and then "Upload data" using "Data_Figure_5.xlsx"

* (6) select as "Source" column "A", and as "Target" column "B"

* (7) select as "Value of Link" column "C"

* (8) click on "Preview", and then on "Match data sheet"

* The same applies for Figures SI-2 and SI-3, starting from Data_Figure_SI_2.xlsx and Data_Figure_SI_3.xlsx, respectively.


*************************
* Table SI-3 - Figure 6 *
*************************

*=========================================*
* From municipal 2016 to legislative 2018 *
*=========================================*

* Without controls

regress sw_to_lega_16_18 ${main} if target!=3 & target!=4 & no_answer_2018==0 & no_answer_municipal==0, robust

* Including individual controls

regress sw_to_lega_16_18 ${main} ${controls} if target!=3 & target!=4 & no_answer_2018==0 & no_answer_municipal==0, robust

* Including unkown-car 

regress sw_to_lega_16_18 ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_2018==0 & no_answer_municipal==0, robust


*======================================*
* From municipal 2016 to regional 2018 *
*======================================*

* Without controls

regress sw_to_lega_16_reg ${main} if target!=3 & target!=4 & no_answer_regional==0 & no_answer_municipal==0, robust

* Including individual controls

regress sw_to_lega_16_reg ${main} ${controls} if target!=3 & target!=4 & no_answer_regional==0 & no_answer_municipal==0, robust

* Including unkown-car 

regress sw_to_lega_16_reg ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_regional==0 & no_answer_municipal==0, robust


************************************
* Table SI-4 -  Figure 7 panel (a) *
************************************

* Recycled materials

regress recycled_materials ${main} ${controls} if target!=3 & target!=4, robust 

* Short showers

regress short_shower ${main} ${controls} if target!=3 & target!=4, robust  

* Eco-mode

regress eco_mode ${main} ${controls} if target!=3 & target!=4, robust  

* Bottles

regress water_bottle ${main} ${controls} if target!=3 & target!=4, robust  

* Climate neutrality

regress climate_neutrality ${main} ${controls} if target!=3 & target!=4, robust  

* Climate transition positive for citizens

regress green_policies_positive ${main} ${controls} if target!=3 & target!=4, robust  


************************************
* Table SI-5 -  Figure 7 panel (b) *
************************************

* Clicked on website

regress dummy_zeroco2_click ${main} ${controls} if target!=3 & target!=4, robust 

* Watched video

regress dummy_watch_video ${main} ${controls} if target!=3 & target!=4, robust 

* Social media

regress dummy_social ${main} ${controls} if target!=3 & target!=4, robust 

* Podcast

regress dummy_podcast ${main} ${controls} if target!=3 & target!=4, robust 

* Buy tree

regress dummy_buy ${main} ${controls} if target!=3 & target!=4, robust 


************************************
* Table SI-6 -  Figure 7 panel (c) *
************************************

* Clicked on website

regress dummy_genitori_click ${main} ${controls} if target!=3 & target!=4, robust 

* Newsletter

regress dummy_newsletter ${main} ${controls} if target!=3 & target!=4, robust 

* Donation

regress dummy_donation ${main} ${controls} if target!=3 & target!=4, robust 


**************************
* Table SI-7 -  Figure 8 *
**************************

regress gov_firms_responsibility ${main} ${controls} if target!=3 & target!=4, robust 

regress taxes_eco_friendly ${main} ${controls} if target!=3 & target!=4, robust  

regress pay_eco_friendly ${main} ${controls} if target!=3 & target!=4, robust 


**************
* Table SI-8 *
**************

* Without controls

regress vote_lega_euro diesel_euro4 if target!=3 & target!=4 & no_answer_euro==0 & fuel==1, robust

* Including individual controls

regress vote_lega_euro diesel_euro4 ${controls} if target!=3 & target!=4 & no_answer_euro==0 & fuel==1, robust

* On the baseline, including control for past vote for Lega: Legislative 2018

regress vote_lega_euro diesel_euro4 ${controls} vote_lega_2018 if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0 & fuel==1, robust 

* On the baseline, including control for past vote for Lega: Regional 2018

regress vote_lega_euro diesel_euro4 ${controls} vote_lega_regional if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0 & fuel==1, robust

* On the baseline, including control for past vote for Lega: Municipal 2016

regress vote_lega_euro diesel_euro4 ${controls} vote_lega_municipal if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0 & fuel==1, robust

* Baseline on the switching regressions - Legislative 2018

regress sw_to_lega_18_19 diesel_euro4 ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0 & fuel==1, robust

* Baseline on the switching regressions - Regional 2018

regress sw_to_lega_reg_19 diesel_euro4 ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional==0 & fuel==1, robust

* Baseline on the switching regressions - Municipal 2016

regress sw_to_lega_16_19 diesel_euro4 ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal==0 & fuel==1, robust


**************
* Table SI-9 *
**************

regress vote_lega_euro km_* ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

regress vote_lega_euro ten_km ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

regress sw_to_lega_18_19 km_* ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

regress sw_to_lega_18_19 ten_km ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust


***************
* Table SI-10 *
***************

regress vote_lega_euro use_* ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

regress vote_lega_euro everyweek ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

regress vote_lega_euro workcar_* ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

regress vote_lega_euro work_everyweek ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0, robust

regress sw_to_lega_18_19 use_* ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

regress sw_to_lega_18_19 everyweek ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

regress sw_to_lega_18_19 workcar_* ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust

regress sw_to_lega_18_19 work_everyweek ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018==0, robust


***************
* Table SI-11 *
***************

regress switch_descriptive ${main} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018_rob==0, robust

regress switch_descriptive ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_2018_rob==0, robust

regress switch_descriptive ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_euro==0 & no_answer_2018_rob==0, robust

regress switch_descriptive_reg ${main} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional_rob==0, robust

regress switch_descriptive_reg ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_regional_rob==0, robust

regress switch_descriptive_reg ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_euro==0 & no_answer_regional_rob==0, robust 

regress switch_descriptive_mun ${main} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal_rob==0, robust

regress switch_descriptive_mun ${main} ${controls} if target!=3 & target!=4 & no_answer_euro==0 & no_answer_municipal_rob==0, robust

regress switch_descriptive_mun ${main_ass} ${controls} dummy_car_unknown if target!=3 & no_answer_euro==0 & no_answer_municipal_rob==0, robust


******************************************************************
* Tables SI-12 and SI-13 do not require any replication commands *
******************************************************************


***************
* Table SI-14 *
***************

* Predicting the probability to vote for Lega just based on controls, only on unaffected car owners

regress vote_lega_euro age female EDU2-EDU4 INC2-INC16 if target==2 & no_answer_euro==0,robust

predict prob, xb

* Obtaining figures reported in footnote 24 in the main text

sum prob if target==1
sum prob if target==2
