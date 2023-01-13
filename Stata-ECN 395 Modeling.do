** ECN 395: Final Modeling
** Author: Jenny Li
** Date: December 2022
** Acknowlegment: the creation of some variables are based on Carrizosa, R., Gaertner, F. B., & Lynch, D. (2019)

** Note: I did not isolate the sample to U.S. domestic firms during data cleaning since checking whether the policy affect the investment behavior of non-US firms will allow us to gain more insight about the robustness of the result. But I isolated the sample to domestic firms while creating the descriptive statistics table

** Please change the following filepath before running the do file 
use "/Users/jennyli/Desktop/data_for_jenny.dta", clear


** GDP deflator from https://fred.stlouisfed.org/series/USAGDPDEFAISMEI
gen deflator = . 
replace deflator = 95.85701 if fyear == 2010
replace deflator = 93.76552 if fyear == 2011
replace deflator = 95.51946 if fyear == 2012
replace deflator = 97.19200 if fyear == 2013
replace deflator = 99.00940 if fyear == 2014
replace deflator = 100 if fyear == 2015
replace deflator = 101.00219 if fyear == 2016
replace deflator = 102.91910 if fyear == 2017 
replace deflator = 105.37785 if fyear == 2018
replace deflator = 107.26279 if fyear == 2019

replace deflator = deflator/100 

gen taxable_income = (txfed + txdc)/deflator/0.35
winsor2 taxable_income, cuts(1 99) replace by(fyear)
label var taxable_income "Taxable Income"

** create the variable ati following the design of Carrizosa et al (2019)
gen ati = xint + pi - idit + dp
winsor2 ati, cuts(1, 99) replace by(fyear)
label var ati "Adjustable Taxable Income"

gen bar = 0.3 * ati if fyear == 2016
egen bar_firm = mean(bar), by(gvkey)

** Constructing and winsorizing key variables of interest
gen i_over_at = xint/at
winsor2 i_over_at, cuts(1 99) replace by(fyear)
label var i_over_at "Interest Expenses Over Asset"

gen ln_xint = log(xint)
winsor2 ln_xint, cuts(1 99) replace by(fyear)
label var ln_xint "Log(Interest Expenses)"

gen debt_ratio = lt/at
winsor2 debt_ratio, cuts (1 99) replace by(fyear)
label var debt_ratio "Debt Ratio"

gen ln_lt = log(lt)
winsor2 ln_lt, cuts(1 99) replace by(fyear)
label var ln_lt "Log(Liabilities)"

** Constructing variables related to fixed effects 
** firm size by year fe (based on at)
egen firm_size = mean(at), by(gvkey)
xtile d5 = firm_size, n(5)

**  the industry by year fixed effect
gen naics_3 = substr(naics, 1, 3)
gen naics_4 = substr(naics, 1, 4)

egen n3_group = group(naics_3)
egen n4_group = group(naics_4)

** Sample Restriction Related
gen sale_15_17 = sale if fyear <= 2017 & fyear >= 2015 
egen avg_sale = mean(sale_15_17), by(gvkey)

drop if avg_sale < 25

** observations in the dataset can be uniquely identified by gvkey and fyear
** isid gvkey fyear

** Relabel some variable for the creation of table
label var xint "Total Interest Expenses"
label var at "Total Assets"
label var lt "Total Liabilities"

** Additional Adjustment for the control and pre groups 

** limit the pre-group to 2013-2017
** create the dummy variable called "post"
gen post_n = . 
replace post_n = 1 if fyear > 2017
replace post_n = 0 if fyear <= 2017 & fyear >= 2013

** new treatment groups (change the control group from 0-30% to 10-20%)
gen cb_lower = 0.1 * ati if fyear == 2016
gen cb_upper = 0.2 * ati if fyear == 2016

egen bound_low = mean(cb_lower), by(gvkey)
egen bound_high = mean(cb_upper), by(gvkey)

gen treat_n = . 
replace treat_n = 1 if xint > bar_firm 
replace treat_n = 0 if xint <= bound_high & xint >= bound_low 

label define trt 0 "Control" 1 "Treated"
label values treat_n trt 

** New pre and the new treatment group together 
gen DD = treat_n * post_n
label var DD "Treat * Post"
label var treat_n "Treat"
label var intan "Total Intangible Assets"

** New regressions with new treatment and pre groups 
reghdfe i_over_at DD treat_n if fic == "USA", absorb (fyear gvkey) cluster(gvkey) 
est store yr_fm_fe
reghdfe i_over_at DD treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear) cluster(gvkey) 
est store yr_fm_idyr4_fe_n
reghdfe i_over_at DD treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store yr_fm_id4_sz_n
	
** export the table
esttab yr_fm_fe yr_fm_idyr4_fe_n yr_fm_id4_sz_n using "iat_reg.rtf", varwidth(12) modelwidth(9) ///
replace label star(* 0.1 ** 0.05 *** 0.01) no se ///
stats( N r2_a , label("Observations" "Adj. R-squared")) ///
nonote nocons

** Robustness
** alternative specifications --> ln_xint, debt_ratio, ln_lt 
reghdfe ln_xint DD treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store yr_fm_id4_sz_n_ln

reghdfe debt_ratio DD treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store yr_fm_id4_sz_n_dr

reghdfe ln_lt DD treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store yr_fm_id4_sz_n_lt

** export the table
esttab yr_fm_id4_sz_n_ln yr_fm_id4_sz_n_dr  yr_fm_id4_sz_n_lt using "alternative_reg.rtf", varwidth(12) modelwidth(9) ///
replace label star(* 0.1 ** 0.05 *** 0.01) no se ///
stats( N r2_a , label("Observations" "Adj. R-squared")) ///
nonote nocons


** Drop the year 2010-2012
drop if fyear == 2010 | fyear == 2011 | fyear == 2012


** Descriptive Statistics Table

estpost tabstat ati xint at pi lt xrd intan if fyear == 2017 & fic == "USA", by(treat_n) stat(mean sd p25 p75 count) col(stat)  
esttab using descriptive.rtf, replace cell((mean sd p25 p75 count)) label title("Descriptive Statistics of Key Variables In 2017 Separated by Treatment Group")nonumber noobs


** Making Graphs

** Further modify the control and treatment group 

gen ati_pre = ati if fyear <= 2016 & fyear >= 2014 
egen mean_ati_pre = mean(ati_pre), by(gvkey)

gen xint_pre = xint if fyear <= 2016 & fyear >= 2014 
egen mean_xint_pre = mean(xint_pre), by(gvkey)

gen bar_avg = 0.3 * ati_pre
egen new_bar_f = mean(bar_avg), by(gvkey)

gen bar_25 = 0.25 * ati_pre
egen bar_qrt = mean(bar_25), by(gvkey)

gen cb_lower_n = 0.1 * mean_ati_pre
gen cb_upper_n = 0.2 * mean_ati_pre

egen bound_low_n = mean(cb_lower_n), by(gvkey)
egen bound_high_n = mean(cb_upper_n), by(gvkey)

gen log_capx = log(capx)
winsor2 log_capx , cuts(1 99) replace by(fyear)

gen i_over_ppnt = xint/ppent

** Remove year 2010-2012 as requested
drop if fyear == 2010 | fyear == 2011 | fyear == 2012

** Coefficient 
tab fyear, gen(y)

** Interest Over Assets Event Study Graph
preserve

** creating year specific treatment interactions
local yr_id "1 2 3 4 6 7 "
foreach id in `yr_id'{
		gen treatment_`id' = treat_n * y`id'
		}
 	
reghdfe i_over_at treatment_* treat_n if fic == "USA", absorb(fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)	

gen beta = . 
gen se_high = . 
gen se_low = . 

forvalues i = 1(1)4{
replace beta = _b[treatment_`i'] if fyear == `i' + 2012
replace se_high = _b[treatment_`i'] + 1.96*_se[treatment_`i'] if fyear == `i' + 2012
replace se_low =_b[treatment_`i'] - 1.96*_se[treatment_`i'] if fyear == `i' + 2012
}

forvalues i = 6(1)7{
replace beta = _b[treatment_`i'] if fyear == `i' + 2012
replace se_high = _b[treatment_`i'] + 1.96*_se[treatment_`i'] if fyear == `i' + 2012
replace se_low =_b[treatment_`i'] - 1.96*_se[treatment_`i'] if fyear == `i' + 2012
}

replace beta = 0 if fyear == 2017

collapse beta se_high se_low, by(fyear)

twoway (rcap se_high se_low fyear) (connect beta fyear), xlabel(2013(1)2019) graphregion(color(white)) bgcolor(white) legend(label(1 "95% Confidence Interval") label(2 "Interest Over Asset Estimates")) xline(2017)

graph export "/Users/jennyli/Desktop/Interest Over Assets Event Study.png", as(png) name("Graph") replace
restore


** Another Graph for Robustnesspreserve--Alternating Sample --> including non-US firms 

preserve 
** creating year specific treatment interactions
local yr_id "1 2 3 4 6 7 "
foreach id in `yr_id'{
		gen treatment_`id' = treat_n * y`id'
		}
 	
reghdfe i_over_at treatment_* treat_n, absorb(fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)	

gen beta = . 
gen se_high = . 
gen se_low = . 

forvalues i = 1(1)4{
replace beta = _b[treatment_`i'] if fyear == `i' + 2012
replace se_high = _b[treatment_`i'] + 1.96*_se[treatment_`i'] if fyear == `i' + 2012
replace se_low =_b[treatment_`i'] - 1.96*_se[treatment_`i'] if fyear == `i' + 2012
}

forvalues i = 6(1)7{
replace beta = _b[treatment_`i'] if fyear == `i' + 2012
replace se_high = _b[treatment_`i'] + 1.96*_se[treatment_`i'] if fyear == `i' + 2012
replace se_low =_b[treatment_`i'] - 1.96*_se[treatment_`i'] if fyear == `i' + 2012
}

replace beta = 0 if fyear == 2017

collapse beta se_high se_low, by(fyear)

twoway (rcap se_high se_low fyear) (connect beta fyear), xlabel(2013(1)2019) graphregion(color(white)) bgcolor(white) legend(label(1 "95% Confidence Interval") label(2 "Interest Over Asset Estimates")) xline(2017)

graph export "/Users/jennyli/Desktop/Interest Over Assets Event Study_All.png", as(png) name("Graph") replace
restore

** Heterogeneity using DDD : high-tangible vs. low-tangible
gen intan_ratio = intan/at 
winsor2 intan_ratio, cuts(1 99) replace by(fyear)

gen tangible = . 
replace tangible = 0 if intan_ratio >= 0.7 
replace tangible = 1 if intan_ratio <= 0.3
label var tangible "Tangible"

gen DDD = treat_n * post_n * tangible
label var DDD "Treat * Post * Tangible"

gen trt_tan = treat_n * tangible
label var trt_tan "Treat * Tangible"

gen pst_tan = post * tangible 
label var pst_tan "Post * Tangible"

** DDD
reghdfe i_over_at DDD DD trt_tan pst_tan tangible treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store ddd_1
reghdfe ln_xint DDD DD trt_tan pst_tan tangible treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store ddd_2
reghdfe debt_ratio DDD DD trt_tan pst_tan tangible treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store ddd_3
reghdfe ln_lt DDD DD trt_tan pst_tan tangible treat_n if fic == "USA", absorb (fyear gvkey i.n4_group#i.fyear i.d5#i.fyear) cluster(gvkey)
est store ddd_4

** export the table
esttab ddd_1 ddd_2 ddd_3 ddd_4 using "hetero_reg.rtf", varwidth(12) modelwidth(9) ///
replace label star(* 0.1 ** 0.05 *** 0.01) no se ///
stats( N r2_a , label("Observations" "Adj. R-squared")) ///
nonote nocons


