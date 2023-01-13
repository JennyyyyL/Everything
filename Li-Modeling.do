* Name: Project Modeling dofile
* Created by Jenny Li
* Created on April 2022
* Purpose of this program: to construct difference-in-difference model based on cleaned data sets and conduct parallel trends testing on corresponding data set as well 

* Datasets used: 
		* cleaned_model
		* cleaned_pttest
	* The cleaned data set can be obtained from previous data cleaning do file using raw data from Demographic and Health Survey (DHS)
	
* Datasets created: 
		*  cleaned_pttest_final
		*  cleaned_final_model
		
* Graphs created:
		* graph_pltest
		
* Tabels created:
		* results_final.rtf
		
* Comments and important information about this do-file:
* you will also need to the filepath into the path of the newly created folder before running this do-file

* Acknowledgement:
** Raw datasets can be obtained from the Demographic and Health Surveys (https://dhsprogram.com/data/) via request 
** commands about plotting for the parallel trends test are learned from https://www.youtube.com/watch?v=YIC7i3p-iGU&t=86s


*** 1 *** 
clear
set more off

** define filepath 
* define the global macro called "path"
global path "/Users/jennyli/Desktop/ECN-372/Projects/Modeling"

* define three additional global macro "in" and "out" under "path"
global in "$path/In"
global out "$path/Out"

*** 2 ***
** open the dataset "BJPR61FL", the dataset that contains individual information from DHS Benin 2011-12
use "$in/cleaned_model", clear

*** 3 ***
** generate a new variable called "birthyear" based on year of survey and observation's age during the survey year
generate birth_year = hv007 - hv105

** generate a variable called "population" based on information from the World Bank
generate population = 0
replace population = 9460829 if hv000 == "BJ6" & hv007 == 2011
replace population = 9729254 if hv000 == "BJ6" & hv007 == 2012
replace population = 11175192 if hv000 == "BJ7" & hv007 == 2017
replace population = 11485035 if hv000 == "BJ7" & hv007 == 2018
replace population = 10652032 if hv000 == "GN6"
replace population = 12414292 if hv000 == "GN7"

** create the new weight variable, call this variable "weight_adj"
bysort Guinea hv007: egen total = sum(hv005)
gen weight_adj = (hv005/total) * population

*** 3 ***
** build the first regression model without controlling for clustering, sampling weight, and gender difference
regress hv107 Guineapost Guinea Post hv105 if hv105 > 6 & hv105 < 19

*** 4 *** 
** modify the model with clustering and sampling weight 
regress hv107 Guineapost Guinea Post hv105 if hv105 > 6 & hv105 < 19, robust cluster(hv001)

*** 5 *** 
** modify the model with clustering, sampling weight and regional fixed effect 
* regress hv107 Guineapost Guinea Post hv105 i.hv024 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001)
areg hv107 Guineapost Guinea Post hv105 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001) absorb(hv024)
* regress hv107 Guineapost Guinea Post hv105 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001) absorb(hv024)
** can not be combined since absorb and cluster have different variables 

*** 6 ***
** modify the model such that it is a triple difference model that controls for cluster and sampling weight
generate Guineapost_female = Guinea * Post * Female
generate postfemale = Post * Female
generate Guineafemale = Guinea * Female 

regress hv107 Guineapost_female Guineapost postfemale Guineafemale Guinea Post Female hv105 i.hv024 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001) 


** 6.5 **
** add controls for being at secondary school age 
generate secondary = .
replace secondary = 1 if hv105 >= 13
replace secondary = 0 if hv105 < 13

generate Guineapost_sec = Guinea * Post * secondary
generate post_sec = Post * secondary
generate Guinea_sec = Guinea * secondary

areg hv107 Guineapost_sec Guineapost post_sec Guinea_sec Guinea Post secondary hv105 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001) absorb(hv024)


*** 7 *** 
** using diff-in-diff separate based on gender
* regress hv107 Guineapost Guinea Post hv105 hv024 if hv105 > 6 & hv105 < 19 & Female == 1 [pw = weight_adj], robust cluster(hv001)
* regress hv107 Guineapost Guinea Post hv105 hv024 if hv105 > 6 & hv105 < 19 & Female == 0 [pw = weight_adj], robust cluster(hv001)

*** 8 ***
** generate a dummy for being at 7 years old during the time of Ebola (2014)
generate primary_age = 0 
replace primary_age = 1 if birth_year == 2007
** generate another dummy for being at 13 years old during the time of Ebola (2014)
generate secondary_age = 0
replace secondary_age = 1 if birth_year == 2001

*** 7 ***
** clear stored results
eststo clear
** store the regression 
eststo: regress hv107 Guineapost Guinea Post if hv105 > 6 & hv105 < 19
eststo: regress hv107 Guineapost Guinea Post hv105 if hv105 > 6 & hv105 < 19, robust cluster(hv001)
eststo: areg hv107 Guineapost Guinea Post hv105 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001) absorb(hv024)
** export the results to "results_final.rtf"
esttab using results_final.rtf, se ar2 label title (Final Regression Results) replace

** export the regression result for triple difference to another rtf
** clear stored results
eststo clear
** store the regression
eststo: areg hv107 Guineapost_female Guineapost postfemale Guineafemale Guinea Post Female hv105 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001) absorb(hv024)
** export the results to "results_final.rtf"
esttab using results_gender.rtf, se ar2 label title (Final Regression Results) replace

** export the result of controlling for secondary school-age
eststo clear
eststo : areg hv107 Guineapost_sec Guineapost post_sec Guinea_sec Guinea Post secondary hv105 if hv105 > 6 & hv105 < 19 [pw = weight_adj], robust cluster(hv001) absorb(hv024)
** 
esttab using results_secondary.rtf, se ar2 label title (Final Regression Results) replace

**

asdoc sum hv105 hv107 Guinea Post Female secondary if hv105 > 6 & hv105 < 19, save(sumstat_school.rtf) replace


** label variables for graphing
label define Guinea 0 "Benin" 1 "Guinea"
label values Guinea Guinea

** histogram
twoway histogram hv107 if hv105 > 6 & hv105 < 19, discrete by(Guinea)

* export the scatter plot to the out folder called "hist_hv107" in pdf form
graph export "$out/hist_hv107.pdf", replace as(pdf)

* save the current dataset to the out folder, call the saved dataset cleaned_final_model
save "$out/cleaned_final_model", replace 

*** 8 *** 
** open the dataset "cleaned_pttest"
use "$in/cleaned_pttest", clear

** add the dataset "cleaned_model" 
append using "$in/cleaned_model"

** birthyear
generate birth_year = hv007 - hv105

** generate a new variable called "timephrase" such that graphing is easier
generate timephrase = 00
replace timephrase = 11 if hv000 == "BJ6" | hv000 == "GN6"
replace timephrase = 22 if hv000 == "BJ7" | hv000 == "GN7"

** check whether the new generated variable is correct
count if hv000 == "BJ5" |hv000 == "GN4"
count if hv000 == "BJ6" |hv000 == "GN6"
count if hv000 == "BJ7" |hv000 == "GN7"
tab timephrase
* based on the above command, the variable "timephrase" is generated properly

** generate a school_age dummy
generate school_age = 0 
replace school_age = 1 if hv105 > 6 & hv105 < 19

**
* bysort hv000 birth_year school_age: egen avg_school = mean(hv107)

bysort hv000 hv105 school_age: egen avg_school = mean(hv107)

graph twoway (scatter avg_school hv105 if Guinea == 1 & school_age == 1 & timephrase == 00, mcolor(red)) (scatter avg_school hv105 if Guinea == 0 & school_age == 1 & timephrase == 00, mcolor (blue)), title("Average Years of Schooling for Individuals Ages from 7-18 during Pre-1 Period", size(medsmall)) legend( label (1 "Treated") label (2 "Control"))
* export the scatter plot 
graph export "$out/graph_pre-1.pdf", replace as(pdf)

graph twoway (scatter avg_school hv105 if Guinea == 1 & school_age == 1 & timephrase == 11, mcolor(red)) (scatter avg_school hv105 if Guinea == 0 & school_age == 1 & timephrase == 11, mcolor (blue)), title("Average Years of Schooling for Individuals Ages from 7-18 during Pre Period", size(medsmall)) legend( label (1 "Treated") label (2 "Control"))
* export the scatter plot 
graph export "$out/graph_pre.pdf", replace as(pdf)

** graph twoway (scatter avg_school birth_year if Guinea == 1 & school_age == 1 & timephrase == 00, mcolor(purple)) (scatter avg_school birth_year if Guinea == 1 & school_age == 1 & timephrase == 11 , mcolor(pink)) (scatter avg_school birth_year if Guinea == 1 & school_age == 1 & timephrase == 22, mcolor(red)) (scatter avg_school birth_year if Guinea == 0 & school_age == 1 & timephrase == 00, mcolor(orange))(scatter avg_school birth_year if Guinea == 0 & school_age == 1 & timephrase == 11, mcolor(green))(scatter avg_school birth_year if Guinea == 0 & school_age == 1 & timephrase == 22, mcolor(blue))

* export the scatter plot to the out folder called "graph_pltest1" in pdf form
* graph export "$out/graph_pltest1.pdf", replace as(pdf)

** graph for the general average years of schooling for all school-age individuals based on country and time phrase
bysort hv000 school_age : egen all_avg_school = mean(hv107)
graph twoway (scatter all_avg_school timephrase if Guinea == 1 & school_age == 1, mcolor(pink)) (scatter all_avg_school timephrase if Guinea == 0 & school_age == 1, mcolor(blue))

** graph
* bysort hv000 school_age: egen avg_school = mean(hv107)

* graph twoway (scatter avg_school timephrase if Guinea == 1 & school_age == 1, mcolor(blue)) (scatter avg_school timephrase if Guinea == 0 & school_age == 1, mcolor(pink))

* export the scatter plot to the out folder called "graph_pltest" in pdf form
* graph export "$out/graph_pltest.pdf", replace as(pdf)

* save the current dataset to the out folder, call the saved dataset cleaned_pttest_final
save "$out/cleaned_pttest_final", replace 
