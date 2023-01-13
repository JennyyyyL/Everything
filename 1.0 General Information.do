*** 1.0 General Information.do 
*** Author: Jenny Li (JL)
*** Date created: Fall 2022
*** project: Child Foster

*** purpose: to obtain general information about the sample and do necessary variable transformation such that the data is ready for regression models 

* dataset used: Standard DHS dataset 
			
* datasets created: full_data_cleaned
			
/**** Sections
	**** #1 Filepaths and global variables 
	**** #2 General Variable Creation
	**** #3 Relationship Crisis 
	**** #4 Partner/husband characteristics
	**** #5 Contextual factors
	**** #6 Characterisitics of the child
	**** #7 Pregnancy Decisions
	**** #8 Women's work related 
	**** #9 Gender Violence
************/



**** #1 Filepaths and global variables 
** Global roots

** DASIL folder root
* global foster "\\storage\groups\DASIL\Child Foster Dominican Republic"

** Mac Filepath
global foster "/Users/jennyli/Desktop/Child Foster"

global out "$foster/Outcomes"
set more off

** load the dataset
**Mac path
use "$foster/Raw Data/DRIR61DT/DRIR61FL.dta", clear

** Windows Filepath
* use "$foster\Raw Data\DRIR61FL.dta", clear


**** #2 General Variable Creation 
count
** 9,372 observations total

** age distribution 
tab v012
** ages between 15 - 49

** number of respondents ever given birth
count if v201 > 0
** 6,687/9372 = 71.35% have given birth

** generate a dummy for females that have sons that are not staying with them
gen son_not_together = .
replace son_not_together = 0 if v204 == 0
replace son_not_together = 1 if v204 > 0
replace son_not_together = . if v201 == 0
** keep this dummy as missing for females that have not given birth

** generate a similar dummy for whether the daughter is staying with the respondent
gen daut_not_together = . 
replace daut_not_together = 1 if v205 > 0 
replace daut_not_together = 0 if v205 == 0 
replace daut_not_together = . if v201 == 0


/*
				son_not_tog |
					  ether |      Freq.     Percent        Cum.
				------------+-----------------------------------
						  0 |      5,089       54.30       54.30
						  1 |      1,598       17.05       71.35
						  . |      2,685       28.65      100.00
				------------+-----------------------------------
					  Total |      9,372      100.00


				daut_not_to |
					 gether |      Freq.     Percent        Cum.
				------------+-----------------------------------
						  0 |      4,964       52.97       52.97
						  1 |      1,723       18.38       71.35
						  . |      2,685       28.65      100.00
				------------+-----------------------------------
					  Total |      9,372      100.00
									  
					
*/

** generate a dummy for if any of the respondent's children are not staying with her
** first generate the dummy to a temporary value of 10, this value is expected to be replaced during the process of variable generation 
gen not_together = 10
replace not_together = 1 if daut_not_together == 1 | son_not_together == 1
replace not_together = 0 if daut_not_together == 0 & son_not_together == 0 
replace not_together = . if v201 == 0 
** age of the child 
			/* 

			not_togethe |
					  r |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      4,179       44.59       44.59
					  1 |      2,508       26.76       71.35
					  . |      2,685       28.65      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00
				  
				daut_not_t |         son_not_together
				   ogether |         0          1          . |     Total
				-----------+---------------------------------+----------
						 0 |     4,179        785          0 |     4,964 
						 1 |       910        813          0 |     1,723 
						 . |         0          0      2,685 |     2,685 
				-----------+---------------------------------+----------
					 Total |     5,089      1,598      2,685 |     9,372 

								  
			*/ 
						  				  
** Generate dummy variables "Child_u16" with index such that it represents whether the corresponding children is under 16 or not, replace the input with missing if there's no input for the child's age
local childindex "01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20"	

foreach index in `childindex'{
		gen child_u16_`index' = 0 
		replace child_u16_`index' = 1 if b8_`index' <= 16
		replace child_u16_`index' = . if b8_`index' ==. 
		}
		
** check if the above loop generates what we want, might not need to do in the fulture
 * bro child_u16* b8*

** generate a dummy variable for whether the respondent have ever given birth
gen birth_ever = . 
replace birth_ever = 1 if v201 > 0 
replace birth_ever = 0 if v201 == 0
	/* 
			 tab birth_ever, miss

		 birth_ever |      Freq.     Percent        Cum.
		------------+-----------------------------------
				  0 |      2,685       28.65       28.65
				  1 |      6,687       71.35      100.00
		------------+-----------------------------------
			  Total |      9,372      100.00
*/


**** #3 Relationship Crisis

/*
		 tab v535 v502, miss

			   ever been |   currently/formerly/never in
		   married or in |              union
				   union | never in   currently  formerly  |     Total
		-----------------+---------------------------------+----------
					  no |     2,128          0          0 |     2,128 
		formerly married |         0          0        307 |       307 
		lived with a man |         0          0      1,718 |     1,718 
					   . |         0      5,219          0 |     5,219 
		-----------------+---------------------------------+----------
				   Total |     2,128      5,219      2,025 |     9,372 


JL: it seems like if the data does displays v535 as missing if the respondents answered that they are currently in union (v502 == 1 )
*/

** JL: all the following dummy should be used with the birth_ever dummy
** Relationship Crisis Hypothesis 1 
** generate a dummy called "rc1" for whether the respondent ever lived with someone that is not because of marriage or never lived with someone
generate rc1 = . 
** currently in marriage  
replace rc1 = 0 if v502 == 1
** formly in union and is because of marriage
replace rc1 = 0 if v502 == 2 & v535 == 1
** formly in union but is not formerly married
replace rc1 = 1 if v502 == 2 & v535 == 2
** never in a union
replace rc1 = 1 if v502 == 0 

			/* 
				rc1 |      Freq.     Percent        Cum.
		------------+-----------------------------------
				  0 |      5,526       58.96       58.96
				  1 |      3,846       41.04      100.00
		------------+-----------------------------------
			  Total |      9,372      100.00
			*/ 
			
** Relationship Crisis Hypothesis 2
** generate a dummy called "rc2" for whether the respondent ever lived with a man not because of marriage 
gen rc2 = . 
** formerly in a union because of marriage
replace rc2 = 0 if v502 == 2 & v535 == 1
** formerly in a union but not because of marriage
replace rc2 = 1 if v502 == 2 & v535 == 2
			
** JL: no info about the marital status of female who are currently in a union--check back			
			
** Relationship Crisis Hypothesis 3
** generate a dummy called "rc3" for whether the married respondent is living with a partner or not
			/* 
			tab v501,miss

						 current marital status |      Freq.     Percent        Cum.
			------------------------------------+-----------------------------------
								 never in union |      2,128       22.71       22.71
										married |      1,182       12.61       35.32
							living with partner |      4,037       43.08       78.39
										widowed |        115        1.23       79.62
									   divorced |        116        1.24       80.86
			no longer living together/separated |      1,794       19.14      100.00
			------------------------------------+-----------------------------------
										  Total |      9,372      100.00
			*/ 

gen rc3 = . 
** replace the dummy to 1 if v501 = 1-> married 
replace rc3 = 1 if v501 == 1 
** v502 = 2 -> living with partner 
replace rc3 = 0 if v501 == 2

			/*
			tab rc2, miss

					rc2 |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      4,037       43.08       43.08
					  1 |      1,182       12.61       55.69
					  . |      4,153       44.31      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00

			*/

** JL: we can't differentiate those who are never in union is married or not, check back 

** Relationship Crisis Hypothesis 4

/*
tab v501 v503, miss

      current marital |         number of unions
               status |      once  more than          . |     Total
----------------------+---------------------------------+----------
       never in union |         0          0      2,128 |     2,128 
              married |       915        264          3 |     1,182 
  living with partner |     2,273      1,757          7 |     4,037 
              widowed |        58         56          1 |       115 
             divorced |        75         41          0 |       116 
no longer living toge |     1,028        761          5 |     1,794 
----------------------+---------------------------------+----------
                Total |     4,349      2,879      2,144 |     9,372 

*/
** JL: check back about the missing value for v503 for those who used be married, etc. 

** generate a dummy variable for whether a respondent has lived/married for more than once
generate rc4 = . 
replace rc4 = 1 if v503 == 2
replace rc4 = 0 if v503 == 1
**JL: check back for missing values in v503


** Relationship Crisis Hypothesis 5
** generate a dummy variable for whether a respondent is married/in a relationship and is not staying with their husband/partner
generate rc5 = . 
** v501 = 1 (married); v504 = 2 (partner/husband somewhere else)
replace rc5 = 1 if v501 == 1 & v504 == 2
replace rc5 = 1 if v501 == 2 & v504 == 2 
** v501 = 2 (living with a partner) ; v504 = 1 (currently living with partner)
replace rc5 = 0 if v501 == 1 & v504 == 1
replace rc5 = 0 if v501 == 2 & v504 == 1

/* 
tab rc5, miss
				rc5 |      Freq.     Percent        Cum.
		------------+-----------------------------------
				  0 |      4,709       50.25       50.25
				  1 |        503        5.37       55.61
				  . |      4,160       44.39      100.00
		------------+-----------------------------------
			  Total |      9,372      100.00
*/ 


** Relationship Crisis Hypothesis 6
** current age range of females who have given birth: 15-49 
** age range for v511 "How old were you when you started living with him"
** 10-46, with 2,128 observations missing 

/* the following code are no longer needed since age will be treated as a continuous variable in the model 
** create group of local variables called ages
** notice that 46 was left out intentionally to avoid multicollinearity between age dummies 
local ages "10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 44"	

** generate a list of age dummies started with union_age
foreach age in `ages'{
		gen union_age_`age' = 0
		replace union_age_`age' = 1 if v511 == `age' 
		replace union_age_`age' = . if v511 == .  
		}
*/

** Relationship Crisis Hypothesis 7
** similar with hypothesis 6, we will treat age as a continuous varibale, no dummy variables need to be created

** Relationship Crisis Hypothesis 8
** similar to previous one, age wil lbe treat as a continuous variable, no dummy variables will be created here
** but be careful with 23.77% of the participants report age of first sex with " at first union 2,227", which is not specific age
** correspond v511(age at first cohabitation)

** Relationship Crisis Hypothesis 9
** make slight modification toward v767* such that it indicates greater level of instability when the level increases
local index_1 "a b c "	

foreach index in `index_1'{
		gen rc9`index' = v767`index'
		** casual acquaintance from 4 to 3
		replace rc9`index' = 3 if v767`index' == 4
		** commercial sex worker from 6 to 4
		replace rc9`index' = 4 if v767`index' == 6
		** live-in partner from 7 to 5
		replace rc9`index' = 5 if v767`index' == 7
		** convert other into missing -- JL: check back if this is okay
		replace rc9`index' = . if v767`index' == 96
		}

** generate a dummy variable if the last three sex partners are the same or not 
gen rc9_same = 10
replace rc9_same = 1 if v767a == v767b & v767b == v767c 
replace rc9_same = 0 if v767a != v767b | v767a != v767c | v767b != v767c
replace rc9_same = . if v767a ==. & v767b == . & v767c == . 

** Relationship Crisis Hypothesis 10 
** part 1: number of sexual partners in the last 12 months 
** use v766b --number of sexual partners including spouse
** no need to generate variables here, use v766b in the same way as age

**** #4 Partner/husband characteristics

** Partner/husband characteristics 1
** no new variables need, use v730 directly

** Partner/husband characteristics 2 
** generate a dummy phc2 such that it equals to 1 if the partner/husband have attended school to some extend
gen phc2 = 0 
replace phc2 = 1 if v701 == 1 | v701 == 2 | v701 == 3

/* 
				   phc2 |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      2,854       30.45       30.45
					  1 |      6,518       69.55      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00
*/ 

** Partner/husband characteristics 3
** phc3_p = 1 if the husband/partner attained at least secondary
gen phc3_s = 0 
replace phc3_s = 1 if v701 == 2 | v702 == 3

**phc3_u = 1 if the husband/partner attained at least university
gen phc3_u = 0 
replace phc3_u = 1 if v702 == 3

** exclude the missings and don't know  

/* 

				 phc3_s |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      6,483       69.17       69.17
					  1 |      2,889       30.83      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00

				  
				 phc3_u |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      8,601       91.77       91.77
					  1 |        771        8.23      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00
*/ 

** Partner/husband characteristics 4
** JL: just use v715 (partner's number of years of education) is good enough 

**** #5 Contextual factors
** Contextual factors 1
** JL: not entirely sure how to interpret the value
/*  
				 region |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      1,449       15.46       15.46
					  i |      1,044       11.14       26.60
					 ii |      1,145       12.22       38.82
					iii |        958       10.22       49.04
					 iv |        865        9.23       58.27
					  v |      1,136       12.12       70.39
					 vi |        918        9.80       80.19
					vii |        890        9.50       89.68
				   viii |        967       10.32      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00

				  
			  de jure region of |
					  residence |      Freq.     Percent        Cum.
			--------------------+-----------------------------------
							  0 |      1,392       14.85       14.85
							  i |      1,011       10.79       25.64
							 ii |      1,109       11.83       37.47
							iii |        926        9.88       47.35
							 iv |        820        8.75       56.10
							  v |      1,109       11.83       67.94
							 vi |        859        9.17       77.10
							vii |        852        9.09       86.19
						   viii |        950       10.14       96.33
			not dejure resident |        344        3.67      100.00
			--------------------+-----------------------------------
						  Total |      9,372      100.00
*/ 
** JL: is this just the i is urban and the rest is rural ? how about 0 

** Contextual factors 2
** generate cf2 = 1 if the respondent have gone to school 
gen cf2 = 0 
replace cf2 = 1 if v106 == 1 | v106 == 2| v106 == 3

/* 
				cf2 |      Freq.     Percent        Cum.
		------------+-----------------------------------
				  0 |        263        2.81        2.81
				  1 |      9,109       97.19      100.00
		------------+-----------------------------------
			  Total |      9,372      100.00

*/ 

** Contextual factors 3 
** generate cf3_s = 1 if respondent have at least attended secondary
gen cf3_s = 0 
replace cf3_s = 1 if v106 == 2 | v106 == 3
** generate cf3_u = 1 if respondnet have at least attended university
gen cf3_u = 0 
replace cf3_u = 1 if v106 == 3

** Context factors 4
** use v107 directly --highest years of education

** Contextual factors 5
** generate cf5 = 1 if respondent cannot read or can only partially read and cf5 = 0 if respondent call read the whole sentence
gen cf5 = . 
replace cf5 = 1 if v155 == 0 | v155 == 1
replace cf5 = 0 if v155 == 2

** Context factors 6
** v167 is sufficient here

** Context factors 7
** v168 == 1 if the respondent have away more than 1 month in the last 12 months
** careful with the missings here
		/* 
		   away for |
		  more than |
		  one month |
		 in last 12 |
			 months |      Freq.     Percent        Cum.
		------------+-----------------------------------
				  0 |      3,444       36.75       36.75
				  1 |        666        7.11       43.85
				  . |      5,262       56.15      100.00
		------------+-----------------------------------
			  Total |      9,372      100.00

		*/ 

** Context factors 8 
** s117 is sufficent here, it = 1 when the respondent have travelled outside the country in the last 12 months
		/* 
		   have you |
		  travel in |
		the last 12 |
			 months |      Freq.     Percent        Cum.
		------------+-----------------------------------
				 no |      9,058       96.65       96.65
				yes |        312        3.33       99.98
				  . |          2        0.02      100.00
		------------+-----------------------------------
			  Total |      9,372      100.00

		*/ 

**** #6 Characterisitics of the child		
** Child Characteristics 1 
** we can't constrain this to children under 16 -- no info about which child is not staying, only the total number

** Child Characteristics 2	
** same question: how do we know which child is not staying with the respondent

**** #7 Pregnancy
** pregnancy decision 1
** interpreting the result
/* 
     wanted |
  pregnancy |
when became |
   pregnant |      Freq.     Percent        Cum.
------------+-----------------------------------
       then |      1,545       16.49       16.49
      later |      1,029       10.98       27.46
    no more |        387        4.13       31.59
          . |      6,411       68.41      100.00
------------+-----------------------------------
      Total |      9,372      100.00
*/
** also, constrain to child under 5? since m10* is about child under 5

** preganancy decision 2 
** use all m10_1 to m10_5 ?? 

**** #8 Women's Work Related
** Women's work 1 
** gen ww1 = 1 if she has worked to some extent, = 0 if she has not worked at all
gen ww1 = .
replace ww1 = 1 if v731 == 1 | v731 == 2 | v731 == 3
replace ww1 = 0 if v731 == 0

** Women's work 2
** ww2 = 1 if she has been wokerd all year; = 0 otherwise
gen ww2 = . 
replace ww2 = 1 if v732 == 1 
replace ww2 = 0 if v732 == 2 | v732 == 3

** Women's work 3
** modify v739 such that the degree of freedom to spend salary decrease when the value increase
gen ww3 = v739
replace ww3 = 3 if v739 == 4
replace ww3 = 4 if v739 == 5

** Women's work 4
** modfity v743d such that the power of decision making decrease when the value increases
gen ww4 = v743d 
replace ww4 = 3 if v743d == 4
replace ww4 = 4 if v743d == 5
replace ww4 = 5 if v743d == 6

**** #9 Gender Violence
** Gender Violence 1
** a 
** modify d101a such that gv1a = 1 if the respondent says yes, 0 otherwise
** JL: should we leave missing as missing or convert them into zero
gen gv1a = d101a
replace gv1a = 0 if d101a == 8

** b - f 
** do the same thing for the rest sub questions        
local question_indx "b c d e f "	
foreach index in `question_indx'{
		gen gv1`index' = d101`index'
		** replace does not know from 8 to 0 
		replace  gv1`index' = 0 if d101`index' == 8
		}

** Gender Violence 2
** JL: there are 2 different lists of variables we could use here : d103a... or s1104a1... 
** I proceed with using s1104a... cause the answer is yes/no while d103a is about different degree/frequency... 

** a
** use s1104a1 directly, s1104a1 = 1 when the respondent answers "yes"
			/* 

			  ever been |
			 humiliated |
					 by |
			husband/par |
				   tner |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      4,822       51.45       51.45
					  1 |        978       10.44       61.89
					  . |      3,572       38.11      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00
			*/

** b 
** use s1104b1 directly, s1104b1 = 1 when the respondent answers "yes"
			/*
			. tab s1104b1, miss nol

			  ever been |
			 threatened |
			  with harm |
					 by |
			husband/par |
				   tner |      Freq.     Percent        Cum.
			------------+-----------------------------------
					  0 |      5,241       55.92       55.92
					  1 |        558        5.95       61.88
					  . |      3,573       38.12      100.00
			------------+-----------------------------------
				  Total |      9,372      100.00
			*/

** c
** use s1104c1 directly, s1104c1 = 1 when the respondent answers "yes"
/* 
  ever been |
insulted or |
    made to |
feel bad by |
husband/par |
       tner |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |      4,317       46.06       46.06
          1 |      1,483       15.82       61.89
          . |      3,572       38.11      100.00
------------+-----------------------------------
      Total |      9,372      100.00

*/

** Gender Violence 3 
** a -- use s1104a2 directly, but be careful with the number of observations (N=975 without any filter)
** b -- use s1104b2 directly, N = 555 without any filter
** c -- use s1104c2 directly, N = 1, 477 without any filter

** Gender Violence 4
** for all the subparts, use the corresponding variable in s1105
** for instance, for a use s1105a1 and use s1105b1 
** the corresponding variable equal to 1 when the respondent answers "yes" and 0 otherwise
** JL: do we need to do anything with missing? 

** Gender Violence 5 
** I use d130a and d130b here, but these variables only contain info about previous husband and it is not just yes/no
** I convert them into yes/no for the interest of this question

** a
gen gv5a = 10 
replace gv5a = 1 if d130a > 0 & d130a < 5
replace gv5a = 0 if d130a == 0 
replace gv5a = . if d130a == . 
		/*
		  previous |
		  husband: |
		 ever hit, |
		slap, kick |
				or |
		physically |
			  hurt |               gv5a
		respondent |         0          1          . |     Total
		-----------+---------------------------------+----------
				 0 |     1,752          0          0 |     1,752 
				 1 |         0         35          0 |        35 
				 2 |         0        439          0 |       439 
				 3 |         0        127          0 |       127 
				 4 |         0          6          0 |         6 
				 . |         0          0      7,013 |     7,013 
		-----------+---------------------------------+----------
			 Total |     1,752        607      7,013 |     9,372 
		*/

** b
gen gv5b = 10 
replace gv5b = 1 if d130b > 0 & d130b < 5
replace gv5b = 0 if d130b == 0 
replace gv5b = . if d130b == . 
		/* 
		  previous |
		  husband: |
		physically |
		 forced to |
		  have sex |
			 or to |
		   perform |
			sexual |               gv5b
			  acts |         0          1          . |     Total
		-----------+---------------------------------+----------
				 0 |     2,100          0          0 |     2,100 
				 1 |         0         21          0 |        21 
				 2 |         0        178          0 |       178 
				 3 |         0         59          0 |        59 
				 4 |         0          2          0 |         2 
				 . |         0          0      7,012 |     7,012 
		-----------+---------------------------------+----------
			 Total |     2,100        260      7,012 |     9,372 
		*/

graph bar (sum)birth_ever (sum)not_together if birth_ever == 1, over(rc1) stack

		
		
***************
** Export the current dataset 
* save "$out/full_data_cleaned", replace
