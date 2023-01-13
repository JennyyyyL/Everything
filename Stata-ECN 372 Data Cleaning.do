* Name: Project Data Cleaning Dofile
* Created by Jenny Li
* Created on April 2022
* Purpose of this program: to clean and combine different DHS datasets such that it is ready to be used for building regression models later to study the impact of Ebola outbreak on the educational attainment of school-age individuals; and create another dataset that can be used for testing parallel trends

* Datasets used: 
		* BJPR61FL
		* BJPR71FL
		* GNPR62FL
		* GNPR71FL
		* BJPR51FL
		* GNPR53FL
	* Raw datasets can be accessed through Demographic and Health Surveys (DHS) via request
	
* Datasets created: 
		* cleaned_model
		* cleaned_pttest 
		
* Graphs created:
		* histogram_hv107
		* histogram_hv107sch
		
* Tabels created:
		* sumstat
		* sumstat_school
		
* Comments:
* you will need to create a general folder with at least two subfolders: one is the unzipped raw data folder called "Raw Data" and another subfolder called "Out" such that the global macros can work properly 
* you will also need to the filepath into the path of the newly created folder before running this do-file
* you will need to install the asdoc command to obtain the summary statistics table and the corresponding command are included in this do-file 
* some variable names might be changed later during model building such that it is easier to understand by audience. For instance, the variable "hv107" might be changed to "years of schooling" in the future. However, the rename process is not included in this do-file. 
* the sex dummy "Female" is created for further analysis to see if the the impact of Ebola varies between male and female; and this depends on the result we obtain for initial analysis

* Acknowledgement:
** Raw datasets can be obtained from the Demographic and Health Surveys (https://dhsprogram.com/data/) via request 
** details and syntax for the "asdoc" command and the installation Stata command are learned from https://www.youtube.com/watch?v=zdI65G6AhdU

*** 1 *** 
clear
set more off

** define filepath 
* define the global macro called "path"
global path "/Users/jennyli/Desktop/ECN-372/Projects/Data Cleaning and Modeling"

* define three additional global macro "in_benin", "in_guinea" and "out" under "path"
global in_benin "$path/Raw Data/Benin"
global in_guinea "$path/Raw Data/Guinea"
global out "$path/Out"

*** 2 ***
** open the dataset "BJPR61FL", the dataset that contains individual information from DHS Benin 2011-12
use "$in_benin/2011-12/BJ_2011-12_DHS_03132022_235_173864/BJPR61DT/BJPR61FL", clear

** count the number of observations in BJPR61FL
count
* there are 88,174 observations in BJPR61FL

** check the number of school-age individuals (7-18) in BJPR61FL and distribution of each age
tabulate hv105 if hv105 > 6 & hv105 < 19
* there are 29,002 school age individuals in BJPR61FL

** check  hv107 for individuals who are above 6 and below 19 
tabulate hv107 if hv105 > 6 & hv105 < 19
** check hv107 without labels to see how "Don't know" is coded in the dataset
tabulate hv107, nolabel
* "Don't know" is coded as 98 in the dataset for hv107, this is useful in later cleaning process
 
*** 3 *** 
* add the dataset "BJPR71FL", the dataset that contains individual level information from DHS Benin 2017-18
append using "$in_benin/2017-18/BJ_2017-18_DHS_03132022_236_173864/BJPR71DT/BJPR71FL"

** count the number of observation within the dataset after appending
count
* there are 162,847 observations after appending 

** check the number of school-age individuals (7-18)
tabulate hv105 if hv105 > 6 & hv105 < 19 
* there are 51,860 school-age individuals in the dataset at this time

*** 4 *** 
** add the dataset "GNPR62FL", the dataset that ccontains individual level information from DHS Guinea 2012
append using "$in_guinea/2012/GN_2012_DHS_03132022_239_173864 /GNPR62DT/GNPR62FL"
** count the number of observation
count
* there are 207,896 observations after joining GNPR62FL

** add the dataset "GNPR71FL", the dataset that contains individual level information from DHS Guinea 2018
append using "$in_guinea/2018/GN_2018_DHS_03132022_238_173864/GNPR71DT/GNPR71FL"
** count the number of obseravtions after appending 
count 
* there are 257,439 observations after joining GNPR71FL

** check the number of school-age individuals (7-18) and distribution of each age
tabulate hv105 if hv105 > 6 & hv105 < 19 
* there are 82,775 school-age individuals in the updated dataset 

*** 5 ***
** check the variable hv000 by using tabulate such that we can further create the country dummy variable
tabulate hv000

** Generate a country dummy called "Guinea" such that Guinea = 1 if the observation is from Guinea and Guinea = 0 if the observation is from Benin
generate Guinea = 0
replace Guinea = 1 if hv000 == "GN6" | hv000 == "GN7"

** double check if the dummy varibale is created properly using count and tab
count if hv000 == "GN6"| hv000 == "GN7"
* there are 94,492 observations from Guinea 
count if hv000 == "BJ6"| hv000 == "BJ7"
* there are 162,847 observations form Benin
tab Guinea
* Based on above work, we have verified that the dummy variable Guinea is created properly 

*** 6 *** 
* Generate a dummy variable called "Post" such that Post = 1 if the individual is from post-Ebola period (after 2015) and Post = 0 if the individual is from pre-Ebola period (before 2015)
generate Post = 0
replace Post = 1 if hv007 >  2015

** verify if the dummy variable is created properly using count and tabulate
count if hv000 == "GN6" | hv000 == "BJ6"
* there are 133,223 observations from pre-Ebola period 
count if hv000 == "GN7" | hv000 == "BJ7"
* there are 124,216 observations from post-Ebola period
tab Post
* Based on the above work, the dummy variable "Post" is created properly

*** 7 *** 
** create the interaction term of the dummy variable "Guinea" and "Post", call this interaction term "Guineapost"
generate Guineapost = Guinea * Post

*** 8 ***
** check the variable hv104 by using tabulate to make sure the variable hv104 is behaving the way we want
tabulate hv104, nolabel
** check the result above by not dropping the labels
tabulate hv104 
* Based on work done above, female is coded as 2 in the variable hv104 while male is coded as 1 

** Generate a sex dummy called "Female" such that Female = 1 ifthe individual is a female and Female = 0 if the observation is a male
generate Female = 0 
replace Female = 1 if hv104 == 2

** verify if the sex dummy is created properly using count and tabulate
count if hv104 == 1
* there are 124,875 male observations
count if hv104 == 2
* there are 132,563 female observations
tab Female
* Based on the above work, the sex dummy "Female" is created properly 

*** 9 *** 
** check the key outcome variable "hv107", which represents the highest years of education completed 
tab hv107
tab hv107, nolabel
* based on the above work, "Don't know" is coded as 98 in the variable

** replace the representation of "Don't know" in hv107
replace hv107 = . if hv107 == 98

** check the key outcome variable "hv107" using summarize 
summarize hv107

** count the number of school-age observations in the dataset
count if hv105 > 6 & hv105 < 19
* there are 82,775 school-age individuals in the datasets

** check "hv107" for school-age individuals especially 
summarize hv107 if hv105 > 6 & hv105 < 19
* noticing that we do not have hv107 input for all school-age individuals, we have 59,087 observations for school-age individuals instead of 82,775 obtained above 

** make sure the variables "hhid, hvidx, hv000" or "hhid, hvidx, Guinea" can be used to uniquely identify each observation 
isid hhid hvidx hv000
isid hhid hvidx Guinea
* since Stata does not return any error message 

*** 10 ***
** save the current dataset to the out folder, call the saved dataset cleaned_model, and the dataset "cleaned_model" will be used for building regression model
save "$out/cleaned_model", replace 

*** 11 *** 
** start cleaning the dataset that can be used for parallel trends testing
** open a new dataset "BJPR51FL", 
use "$in_benin/2006/BJ_2006_DHS_03132022_237_173864/BJPR51DT/BJPR51FL", clear

** count the number of observations in the dataset
count
* there are 90,650 observations

** add the dataset "GNPR53FL", the dataset that contains individual level information from DHS Guinea 2005
append using "$in_guinea/2005/GN_2005_DHS_03132022_2312_173864 2/GNPR53DT/GNPR53FL"
** count the number of obseravtions after appending 
count
* there are 128,832 observations after adding the new dataset

*** 12 *** 
** check the key outcome variable 
tab hv107
tab hv107, nolabel
** noticing the strange input within hv107: 98 and 99, replace them with . since they both represent don't know or missing information 
replace hv107 = . if hv107 == 98 | hv107 == 99
tab hv107

*** 13 *** 
** Also generate a country dummy called "Guinea" such that Guinea = 1 if the observation is from Guinea and Guinea = 0 if the observation is from Benin
generate Guinea = 0
replace Guinea = 1 if hv000 == "GN4" 

** double check if the dummy varibale is created properly using count and tab
count if hv000 == "GN4"
* there are 38,182 observations from Guinea 
count if hv000 == "BJ5"
* there are 90,650 observations form Benin
tab Guinea
* Based on above work, we have verified that the dummy variable Guinea is created properly 

*** 14 *** 
** check the variable hv104 by using tabulate to make sure the variable hv104 is behaving the way we want
tabulate hv104, nolabel
** check the result above by not dropping the labels
tabulate hv104 
* Based on work done above, female is coded as 2 in the variable hv104 while male is coded as 1 

** replace the input 3 with . such that Stata recognize it as missing information
replace hv104 = . if hv104 == 3

** Also, generate a sex dummy called "Female" such that Female = 1 if the individual is a female and Female = 0 if the observation is a male
generate Female = . 
replace Female = 0 if hv104 == 1
replace Female = 1 if hv104 == 2

** verify if the sex dummy is created properly using count and tabulate
count if hv104 == 1
* there are 62,465 male observations
count if hv104 == 2
* there are 66,364 female observations
tab Female
* Based on the above work, the sex dummy "Female" is created properly 

*** 15 ***
** save the current dataset to the out folder, call the saved dataset cleaned_pttest, we will use this dataset for parallel trend testing
save "$out/cleaned_pttest", replace 

*** 16 ***
** reload the cleaned dataset for model building into Stata
use "$out/cleaned_model", clear

** install the asdoc command such that we can use it to output the summary statistics 
ssc install asdoc
* more information about asdoc can learned from "help asdoc" 

** obtain the summary statistics of "hv105, hv107, Guinea, Post" and call the output file "sumstat.rtf"
asdoc sum hv105 hv107 Guinea Post, save(sumstat.rtf) replace
** focus the summary statistics of those variables of school-age individuals especially, call the output file "sumstat_school.rtf"
asdoc sum hv105 hv107 Guinea Post if hv105 > 6 & hv105 < 19, save(sumstat_school.rtf) replace

*** 17 *** 
** show the distribution of hv107 via histogram
histogram hv107
graph export "$out/histogram_hv107.pdf", replace as(pdf)

** show the distribution of hv107 for school-age individuals via histogram_hv107
histogram hv107 if hv105 > 6 & hv105 < 19
graph export "$out/histogram_hv107sch.pdf", replace as(pdf)
