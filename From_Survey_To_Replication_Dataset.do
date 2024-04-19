
* This do-file starts from survey data and produces the replication dataset

* Some of the variables do not need any preparation, so there are no related commands


cap cd "$dir/Paper_Area_B/Analysis"

use Survey.dta,clear


*********************
* Education dummies *
*********************

gen education_level_it=.

replace education_level_it=1 if education_level_it_original<7
replace education_level_it=2 if education_level_it_original==7 |education_level_it_original==9
replace education_level_it=3 if education_level_it_original==8 | education_level_it_original==10 | education_level_it_original==11 | education_level_it_original==12
replace education_level_it=4 if education_level_it_original==13 | education_level_it_original==14

tab education_level_it, gen (EDU)


******************
* Income dummies *
******************

tab profile_gross_personal_eu, gen (INC)


*******************************************
* Treated individuals who are compensated *
*******************************************
 
gen compensated=0

replace compensated=1 if q_comp_new_car==1 | q_comp_used_car==1 | q_comp_motorcycle==1 | q_comp_bike==1 | q_comp_subscription==1


*****************
* Types of cars *
*****************

gen dummy_euro_4=0
replace dummy_euro_4=1 if class==5

gen dummy_euro_5=0
replace dummy_euro_5=1 if class==6

gen dummy_diesel=0
replace dummy_diesel=1 if fuel==1

gen dummy_petrol=0
replace dummy_petrol=1 if fuel==2

gen diesel_euro4=dummy_diesel*dummy_euro_4

gen diesel_euro5=dummy_diesel*dummy_euro_5

gen dummy_car_unknown=0

replace dummy_car_unknown=1 if target==4

gen dummy_diesel_ass=dummy_diesel

gen dummy_euro_4_ass=dummy_euro_4

gen diesel_euro4_ass=diesel_euro4

replace dummy_diesel_ass=1 if target==4 & ban==1

replace dummy_euro_4_ass=1 if target==4 & ban==1

replace diesel_euro4_ass=1 if target==4 & ban==1


***********
* Car use *
***********

gen kmdontknow=0
replace kmdontknow=1 if q_kms==997

gen km_less_1k=0
replace km_less_1k=1 if q_kms==1

gen km_1k_to_5k=0
replace km_1k_to_5k=1 if q_kms==2

gen km_5k_to_10k=0
replace km_5k_to_10k=1 if q_kms==3

gen km_10k_to_20k=0
replace km_10k_to_20k=1 if q_kms==4

gen km_20k_to_30k=0
replace km_20k_to_30k=1 if q_kms==5

gen km_more_30k=0
replace km_more_30k=1 if q_kms==6

gen ten_km=0
replace ten_km=1 if q_kms==4 | q_kms==5 | q_kms==6

gen use_day=0
replace use_day=1 if q_car_use==6

gen use_week=0
replace use_week=1 if q_car_use==7

gen use_month=0
replace use_month=1 if q_car_use==8

gen use_year=0
replace use_year=1 if q_car_use==9

gen everyweek=0
replace everyweek=1 if q_car_use==6 | q_car_use==7

gen workcar_day=0
replace workcar_day=1 if q_use_work==1

gen workcar_week=0
replace workcar_week=1 if q_use_work==2

gen workcar_month=0
replace workcar_month=1 if q_use_work==3

gen workcar_year=0
replace workcar_year=1 if q_use_work==4

gen work_everyweek=0

replace work_everyweek=1 if q_use_work==1 | q_use_work==2


**********
* Female *
**********

gen female=0
replace female=1 if gender==2


**********
* Voting *
**********

gen no_answer_2018=0
replace no_answer_2018=1 if vote_2018==994|vote_2018==997|vote_2018==999

gen no_answer_2018_rob=0
replace no_answer_2018_rob=1 if vote_2018==994|vote_2018==997

gen no_answer_euro=0
replace no_answer_euro=1 if vote_euro==994|vote_euro==997|vote_euro==999

gen no_answer_municipal=0
replace no_answer_municipal=1 if vote_municipal==994|vote_municipal==997|vote_municipal==999

gen no_answer_municipal_rob=0
replace no_answer_municipal_rob=1 if vote_municipal==994|vote_municipal==997

gen no_answer_regional=0
replace no_answer_regional=1 if vote_regional==994|vote_regional==997|vote_regional==999

gen no_answer_regional_rob=0
replace no_answer_regional_rob=1 if vote_regional==994|vote_regional==997

gen vote_forzaitalia_2018=0
replace vote_forzaitalia_2018=1 if vote_2018==2

gen vote_forzaitalia_euro=0
replace vote_forzaitalia_euro=1 if vote_euro==2

gen vote_forzaitalia_municipal=0
replace vote_forzaitalia_municipal=1 if vote_municipal==2

gen vote_forzaitalia_regional=0
replace vote_forzaitalia_regional=1 if vote_regional==2

gen vote_lega_2018=0
replace vote_lega_2018=1 if vote_2018==1

gen vote_lega_euro=0
replace vote_lega_euro=1 if vote_euro==1

gen vote_lega_municipal=0
replace vote_lega_municipal=1 if vote_municipal==1

gen vote_lega_regional=0
replace vote_lega_regional=1 if vote_regional==1

gen vote_m5s_2018=0
replace vote_m5s_2018=1 if vote_2018==5

gen vote_m5s_euro=0
replace vote_m5s_euro=1 if vote_euro==5

gen vote_m5s_regional=0
replace vote_m5s_regional=1 if vote_regional==5

gen vote_m5s_municipal=0
replace vote_m5s_municipal=1 if vote_municipal==5

gen vote_pd_2018=0
replace vote_pd_2018=1 if vote_2018==6

gen vote_pd_euro=0
replace vote_pd_euro=1 if vote_euro==6

gen vote_pd_municipal=0
replace vote_pd_municipal=1 if vote_municipal==6

gen vote_pd_regional=0
replace vote_pd_regional=1 if vote_regional==6

gen sw_to_lega_16_18=0

replace sw_to_lega_16_18=1 if vote_lega_municipal==0 & vote_lega_2018==1

replace sw_to_lega_16_18=. if vote_lega_municipal==1 & vote_lega_2018==1

replace sw_to_lega_16_18=. if vote_lega_municipal==1 & vote_lega_2018==0

replace sw_to_lega_16_18=. if no_answer_municipal==1 

gen sw_to_lega_16_19=0

replace sw_to_lega_16_19=1 if vote_lega_municipal==0 & vote_lega_euro==1

replace sw_to_lega_16_19=. if vote_lega_municipal==1 & vote_lega_euro==1

replace sw_to_lega_16_19=. if vote_lega_municipal==1 & vote_lega_euro==0

replace sw_to_lega_16_19=. if no_answer_municipal==1 

gen sw_to_lega_16_reg=0

replace sw_to_lega_16_reg=1 if vote_lega_municipal==0 & vote_lega_regional==1

replace sw_to_lega_16_reg=. if vote_lega_municipal==1 & vote_lega_regional==1

replace sw_to_lega_16_reg=. if vote_lega_municipal==1 & vote_lega_regional==0

replace sw_to_lega_16_reg=. if no_answer_municipal==1 

gen sw_to_lega_18_19=0

replace sw_to_lega_18_19=1 if vote_lega_2018==0 & vote_lega_euro==1

replace sw_to_lega_18_19=. if vote_lega_2018==1 & vote_lega_euro==1

replace sw_to_lega_18_19=. if vote_lega_2018==1 & vote_lega_euro==0

replace sw_to_lega_18_19=. if no_answer_2018==1 

gen sw_to_lega_reg_19=0

replace sw_to_lega_reg_19=1 if vote_lega_regional==0 & vote_lega_euro==1

replace sw_to_lega_reg_19=. if vote_lega_regional==1 & vote_lega_euro==1

replace sw_to_lega_reg_19=. if vote_lega_regional==1 & vote_lega_euro==0

replace sw_to_lega_reg_19=. if no_answer_regional==1 

gen switch_descriptive=0

replace switch_descriptive=1 if vote_lega_2018==0 & vote_lega_euro==1

replace switch_descriptive=. if vote_lega_2018==1 & vote_lega_euro==1

replace switch_descriptive=. if vote_lega_2018==1 & vote_lega_euro==0

replace switch_descriptive=. if vote_2018==997

gen switch_descriptive_reg=0

replace switch_descriptive_reg=1 if vote_lega_regional==0 & vote_lega_euro==1

replace switch_descriptive_reg=. if vote_lega_regional==1 & vote_lega_euro==1

replace switch_descriptive_reg=. if vote_lega_regional==1 & vote_lega_euro==0

replace switch_descriptive_reg=. if vote_regional==997

gen switch_descriptive_mun=0

replace switch_descriptive_mun=1 if vote_lega_municipal==0 & vote_lega_euro==1

replace switch_descriptive_mun=. if vote_lega_municipal==1 & vote_lega_euro==1

replace switch_descriptive_mun=. if vote_lega_municipal==1 & vote_lega_euro==0

replace switch_descriptive_mun=. if vote_municipal==997


********************
* Prices and taxes *
********************

gen pay_eco_friendly=0
replace pay_eco_friendly=1 if q_prices<3

gen taxes_eco_friendly=0
replace taxes_eco_friendly=1 if q_taxes<3


**************************
* Environmental behavior *
**************************

gen recycled_materials=6-q_recycled

gen short_shower=6-q_showers

gen eco_mode=6-q_eco_mode

gen water_bottle=6-q_bottles


***************************
* Environmental Attitudes *
***************************

gen gov_firms_responsibility=0
replace gov_firms_responsibility=1 if q_responsibility>3

gen climate_neutrality=0
replace climate_neutrality=1 if q_neutrality>3

gen green_policies_positive=0
replace green_policies_positive=1 if q_policies_positive>3


******************************************************************************************************
* Dropping the following variables allows to obtain the same structure as in Replication_Dataset.dta *
******************************************************************************************************

drop ban gender q_bottles q_car_use q_comp_bike q_comp_motorcycle q_comp_new_car q_comp_subscription q_comp_used_car q_eco_mode q_kms q_prices q_recycled q_showers q_taxes q_use_work vote_2018 vote_euro vote_municipal vote_regional education_level_it q_neutrality q_policies_positive q_responsibility

