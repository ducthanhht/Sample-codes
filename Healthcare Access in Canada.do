
**============================================================================**
***** Do-file, Healthcare Access Disparities in Canada *****
**============================================================================**


/* 

- This do-file is to give a preliminary assessment about the disparities in 
healthcare access in Canada.
- You need to change the directories to your own specifications in order to run
- The Stata dataset "CCHS_Annual_2017_2018.dta" is available at 
https://abacus.library.ubc.ca/file.xhtml?persistentId=hdl:11272.1/AB2/SEB16A/8YQECD&version=1.0

*/

/* 
You may need to install the following packages for use. 

ssc install coefplot, replace

ssc install treemap, replace
ssc install palettes, replace
ssc install colrspace, replace

ssc install texdoc, replace
ssc install estout, replace
ssc install mediation, replace
ssc install rmpw
ssc install moremata
ssc install asdoc

ssc install treemap, replace
ssc install palettes, replace
ssc install colrspace, replace

*/

**============================================================================**
***** GENERAL SETTINGS *****
**============================================================================**

clear all
macro drop _all
set more off
mata: mata clear

*cd ~/Desktop/Zohre/Dataanalysis/
cd "~/Desktop/Github/Sample codes"

capture log close
sjlog using logfile, replace 

**============================================================================**
***** DATA PREPARATION: CLEANING UP, VARIABLE CREATION *****
**============================================================================**

use CCHS_Annual_2017_2018, clear 

keep PHC_020 UCN_005 DHH_SEX dhhgage sdcdgcgt SDC_015 sdcdvimm EHG2DVH3 sdcdglhm ///
incdgper incdghh GEO_PRV UCN_010* WTS_M INCG015 INCG035 

rename PHC_020 regdoctor
rename UCN_005 unmet
rename DHH_SEX gender
rename dhhgage age
rename sdcdgcgt race
rename SDC_015 aborident // obmited since correlated with sdcdgcgt, see data dictionary& CCHS Derived Variables
rename sdcdvimm immig
rename EHG2DVH3 edulevels
rename sdcdglhm homelang
rename incdgper pincome
rename incdghh hincome
rename GEO_PRV residence

order regdoctor unmet gender age race aborident immig homelang residence ///
edulevels  pincome hincome INCG015 INCG035

******************** recode variables ******************************************

recode regdoctor (2=0) 
recode unmet (2=0) 

recode gender (1=0) (2=1) 
recode age (1/2=1) (3/12=2) (13/16=3)
recode race (2=0)
recode aborident (2=0)
recode immig (2=0)
recode homelang (1/3=1) (4=0)

***** Residence: Quebec = 1, rest of Canada = 0 *****

recode residence (24=1)
replace residence = 0 if residence~=1&residence~=.


recode residence (10=1) (11=2) (12=3) (13=4) (24=5) (35=6) (46=7) (47=8) (48=9) ///
(59=10) (60=11) (61=12) (62=13)

sum residence
tab residence
recode residence (5=1) 
replace residence = 0 if residence~=1&residence~=.
sum residence
tab residence

recode UCN_010* (2=0)

********** Create variables: Reasons for self-perceived unmet need *************

gen Availability = (UCN_010A==1|UCN_010B==1|UCN_010D==1)
replace Availability =. if UCN_010A==.&UCN_010B==.&UCN_010D==.

gen Accessibility = (UCN_010G==1|UCN_010J==1)
replace Accessibility =. if UCN_010G==.&UCN_010J==.

gen Acceptability = (UCN_010C==1|UCN_010E==1|UCN_010F==1|UCN_010H==1|UCN_010I==1|UCN_010K==1)
replace Acceptability =. if UCN_010C==.&UCN_010E==.&UCN_010F==.&UCN_010H==.&UCN_010I==.&UCN_010K==.

***************

order regdoctor unmet Availability Acceptability Accessibility gender age race aborident immig homelang residence ///
edulevels  pincome hincome INCG015 INCG035 

label var unmet "Unmet needs" 
label var regdoctor "Medical Doctor"
label var gender "Gender (Female = 1)"
label var age "Age"
label var race "Race"
label var aborident "Aboriginal identity"
label var immig "Immigrant status"
label var edulevels "Educational level"
label var homelang "Home language"
label var pincome "Persional income"
label var hincome "Household income"
label var residence "Residence (Quebec = 0, Rest of Canada = 1)"
label var Availability "Availability"
label var Acceptability "Acceptability"
label var Accessibility "Accessibility"

label define regdoctor 0 "No" 1 "Yes"
label values regdoctor regdoctor

label define unmet 0 "No" 1 "Yes"
label values unmet unmet

label define gender 0 "Male" 1 "Female"
label values gender gender

label define edulevels 1 "Less than Secondary" 2 "Secondary" 3 "Post-secondary"
label values edulevels edulevels

label define age 1 "12-17" 2 "18-64" 3 "65 and over"
label values age age

label define race 0 "Non-white" 1 "White"
label values race race

label define aborident 0 "No aboriginal identity" 1 "Aboriginal identity"
label values aborident aborident

label define immig 0 "Canadian born" 1 "Immigrant"
label values immig immig 

label define residence 0 "Rest of Canada" 1 "Quebec"
label values residence residence

label define homelang 0 "Neither English nor French (Other)" 1 "English or/and French"
label values homelang homelang


**============================================================================**
*****DATA SUMMARY, DESCRIPTIVE STATISTICS*****
**============================================================================**

* Chrasteristics of the sample
sum regdoctor unmet Availability Acceptability Accessibility ///
gender age race aborident immig edulevels homelang hincome residence [aweight = WTS_M]


****************** Separate tables - Tables 1-9 ********************

***** Table 1 - By gender ***** 

estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store gender0

estpost sum regdoctor unmet Availability Acceptability Accessibility  if gender==1 [aweight = WTS_M]
est store gender1

estpost sum regdoctor unmet Availability Acceptability Accessibility if gender==0 [aweight = WTS_M]
est store gender2

eststo ttest:  estpost ttest regdoctor unmet Availability Acceptability Accessibility, by(gender) 

esttab gender0 gender1 gender2 ttest using table1.rtf, ///
	mtitles( "All" "Female" "Male" "Diff." ) title("Descriptive Statistics by Gender")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab gender0 gender1 gender2 ttest using table1.tex, ///
	mtitles( "All" "Female" "Male" "Diff." ) title("Descriptive Statistics by Gender")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs fragment	
* nonumbers

***** Table 2 - By age ***** 
estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store age0
estpost sum regdoctor unmet Availability Acceptability Accessibility if age == 1 [aweight = WTS_M]
est store age1
estpost sum regdoctor unmet Availability Acceptability Accessibility if age == 2 [aweight = WTS_M]
est store age2
estpost sum regdoctor unmet Availability Acceptability Accessibility if age == 3 [aweight = WTS_M]
est store age3

gen age21=1 if age==1 
replace age21=0 if age==2 
gen age23=1 if age==3 
replace age23=0 if age==2 

order regdoctor unmet Availability Acceptability Accessibility

foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(age21) 
	est store tage1
	}
	
foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(age23) 
	est store tage2
	}

esttab age0 age1 age2 age3 tage1 tage2 using table2.rtf, ///
	mtitles( "All" "12 - 17" "18 - 64" "65 and over" "2 v 1" "2 v 3" ) title("Descriptive Statistics by Age")  ///	
	cell("mean(pattern(1 1 1 1 0 0) fmt(3)) & sd(pattern(1 1 1 1 0 0) par fmt(3)) b(star pattern(0 0 0 0 1 1) fmt(3))  &se(pattern(0 0 0 0 1 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab age0 age1 age2 age3 tage1 tage2 using table2.tex, ///
	mtitles( "All" "Female" "Male" "Diff." ) title("Descriptive Statistics by Age")  ///	
	cell("mean(pattern(1 1 1 1 0 0) fmt(3)) & sd(pattern(1 1 1 1 0 0) par fmt(3)) b(star pattern(0 0 0 0 1 1) fmt(3))  &se(pattern(0 0 0 0 1 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs fragment	
	
***** Table 3 - By race ***** 

estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store race0

estpost sum regdoctor unmet Availability Acceptability Accessibility  if race==1 [aweight = WTS_M]
est store race1

estpost sum regdoctor unmet Availability Acceptability Accessibility if race==0 [aweight = WTS_M]
est store race2

eststo ttest:  estpost ttest regdoctor unmet Availability Acceptability Accessibility, by(race) 

esttab race0 race1 race2 ttest using table3.rtf, ///
	mtitles( "All" "While" "Non-while" "Diff." ) title("Descriptive Statistics by race")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab race0 race1 race2 ttest using table3.tex, ///
	mtitles( "All" "While" "Non-while" "Diff." ) title("Descriptive Statistics by Race")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs fragment
	
/* aborident is correlated with race, so drop this variable 
(see Data Dictionary and CCHS Derived Variables)
*/	

***** Table 4 - By Immigrant status ***** 

estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store imm0

estpost sum regdoctor unmet Availability Acceptability Accessibility  if immig==1 [aweight = WTS_M]
est store imm1

estpost sum regdoctor unmet Availability Acceptability Accessibility if immig==0 [aweight = WTS_M]
est store imm2

eststo ttest: estpost ttest regdoctor unmet Availability Acceptability Accessibility, by(immig) 

esttab imm0 imm1 imm2 ttest using table4.rtf, ///
	mtitles( "All" "Immigrant" "Canadian born" "Diff." ) title("Descriptive Statistics by imm")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab imm0 imm1 imm2 ttest using table4.tex, ///
	mtitles( "All" "Immigrant" "Canadian born" "Diff." ) title("Descriptive Statistics by ai")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs fragment
	
***** Table 5 - By Home Languages ***** 

estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store hl0

estpost sum regdoctor unmet Availability Acceptability Accessibility  if homelang==1 [aweight = WTS_M]
est store hl1

estpost sum regdoctor unmet Availability Acceptability Accessibility if homelang==0 [aweight = WTS_M]
est store hl2

eststo ttest: estpost ttest regdoctor unmet Availability Acceptability Accessibility, by(homelang) 

esttab hl0 hl1 hl2 ttest using table5.rtf, ///
	mtitles( "All" " English or/and French" "No English or/and French" "Diff." ) title("Descriptive Statistics by Home Languages")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab hl0 hl1 hl2 ttest using table5.tex, ///
	mtitles( "All" "hligrant" "Canadian born" "Diff." ) title("Descriptive Statistics by Home Languages")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs fragment
	
***** Table 6 - By Place of Residence ***** 
estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store res0

estpost sum regdoctor unmet Availability Acceptability Accessibility  if residence==1 [aweight = WTS_M]
est store res1

estpost sum regdoctor unmet Availability Acceptability Accessibility if residence==0 [aweight = WTS_M]
est store res2

eststo ttest: estpost ttest regdoctor, by(residence) 

esttab res0 res1 res2 ttest using table6.rtf, ///
	mtitles( "All" "Quebec" "Rest of Canada" "Diff." ) title("Descriptive Statistics by Residence")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab res0 res1 res2 ttest using table6.tex, ///
	mtitles( "All" "Quebec" "Rest of Canada" "Diff." ) title("Descriptive Statistics by Residence")  ///	
	cell("mean(pattern(1 1 1 0) fmt(3)) & sd(pattern(1 1 1 0) par fmt(3)) b(star pattern(0 0 0 1) fmt(3))  &se(pattern(0 0 0 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs fragment	
		
***** Table 7 - By Educational levels ***** 

estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store edu0
estpost sum regdoctor unmet Availability Acceptability Accessibility if edulevels == 1 [aweight = WTS_M]
est store edu1
estpost sum regdoctor unmet Availability Acceptability Accessibility if edulevels == 2 [aweight = WTS_M]
est store edu2
estpost sum regdoctor unmet Availability Acceptability Accessibility if edulevels == 3 [aweight = WTS_M]
est store edu3


gen edu21=1 if edulevels==1 
replace edu21=0 if edulevels==2 
gen edu23=1 if edulevels==3 
replace edu23=0 if edulevels==2 

order regdoctor unmet Availability Acceptability Accessibility

foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(edu21) 
	est store tedu1
	}
	
foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(edu23) 
	est store tedu2
	}

esttab edu0 edu1 edu2 edu3 tedu1 tedu2 using table7.rtf, ///
	mtitles( "All" "Less than Secondary" "Secondary" "Post-secondary" "2 v 1" "2 v 3" ) title("Descriptive Statistics by edu")  ///	
	cell("mean(pattern(1 1 1 1 0 0) fmt(3)) & sd(pattern(1 1 1 1 0 0) par fmt(3)) b(star pattern(0 0 0 0 1 1) fmt(3))  &se(pattern(0 0 0 0 1 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab edu0 edu1 edu2 edu3 tedu1 tedu2 using table7.tex, ///
	mtitles( "All" "Less than Secondary" "Secondary" "Post-secondary" "2 v 1" "2 v 3" ) title("Descriptive Statistics by Educational Levels")  ///	
	cell("mean(pattern(1 1 1 1 0 0) fmt(3)) & sd(pattern(1 1 1 1 0 0) par fmt(3)) b(star pattern(0 0 0 0 1 1) fmt(3))  &se(pattern(0 0 0 0 1 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs fragment		
		
***** Table 8 - By Income ***** 
estpost sum regdoctor unmet Availability Acceptability Accessibility [aweight = WTS_M]
est store sumincome0
estpost sum regdoctor unmet Availability Acceptability Accessibility if hincome == 1 [aweight = WTS_M]
est store sumincome1
estpost sum regdoctor unmet Availability Acceptability Accessibility if hincome == 2 [aweight = WTS_M]
est store sumincome2
estpost sum regdoctor unmet Availability Acceptability Accessibility if hincome == 3 [aweight = WTS_M]
est store sumincome3
estpost sum regdoctor unmet Availability Acceptability Accessibility if hincome == 4 [aweight = WTS_M]
est store sumincome4
estpost sum regdoctor unmet Availability Acceptability Accessibility if hincome == 5 [aweight = WTS_M]
est store sumincome5

gen in31=1 if hincome==1 
replace in31=0 if hincome==3 
gen in32=1 if hincome==2 
replace in32=0 if hincome==3 
gen in34=1 if hincome==4 
replace in34=0 if hincome==3 
gen in35=1 if hincome==5 
replace in35=0 if hincome==3 

order regdoctor unmet Availability Acceptability Accessibility

foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(in31) 
	est store tincome1
	}
	
foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(in32) 
	est store tincome2
	}

foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(in34) 
	est store tincome3
	}	

foreach x in regdoctor-Accessibility {
	estpost ttest `x', by(in35) 
	est store tincome4
	}	
	

esttab sumincome0 sumincome1 sumincome2 sumincome3 sumincome4 sumincome5 tincome1 ///
	tincome2 tincome3 tincome4 using table8.rtf, mtitles( "Full sample" "1" "2" "3" "4" "5" ///
	"3 v 1" "3 v 2" "3 v 4" "3 v 5" ) title("Outcomes by Income")  ///
	cells("mean(pattern(1 1 1 1 1 1 0 0 0 0) fmt(3)) & sd(pattern(1 1 1 1 1 1 0 0 0 0) par) b(star pattern(0 0 0 0 0 0 1 1 1 1) fmt(3)) & se(pattern(0 0 0 0 0 0 1 1 1 1) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") onecell ///
	mgroups("Summary Statistics" "Difference In Means", pattern(1 0 0 0 0 0 1 0 0 0)) ///
	nogaps replace compress label noobs
	

esttab sumincome0 sumincome1 sumincome2 sumincome3 sumincome4 sumincome5 tincome1 ///
	tincome2 tincome3 tincome4 using table8.tex, mtitles( "Full sample" "1" "2" "3" "4" "5" ///
	"3 v 1" "3 v 2" "3 v 4" "3 v 5" ) title("Outcomes by Income")  ///
	cells("mean(pattern(1 1 1 1 1 1 0 0 0 0) fmt(3)) & sd(pattern(1 1 1 1 1 1 0 0 0 0) par) b(star pattern(0 0 0 0 0 0 1 1 1 1) fmt(3)) & se(pattern(0 0 0 0 0 0 1 1 1 1) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") onecell ///
	mgroups("Summary Statistics" "Difference In Means", pattern(1 0 0 0 0 0 1 0 0 0)) ///
	nogaps replace compress label noobs fragment	
	
**============================================================================**
***** LOGIT REGRESSION *****
**============================================================================**

est clear
logit regdoctor i.gender ib2.age i.race i.immig i.homelang i.residence ib2.edulevels ib3.hincome [pw=WTS_M], level(90)
	margins
	eststo: margins, dydx(*) level(90) post 

logit unmet i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ib3.hincome [pw=WTS_M], level(90)
	margins
	eststo: margins, dydx(*) level(90) post

logit Availability i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ib3.hincome [pw=WTS_M], level(90)
	margins
	eststo: margins, dydx(*) level(90) post

logit Acceptability i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ib3.hincome [pw=WTS_M], level(90)
	margins
	eststo: margins, dydx(*) level(90) post

logit Accessibility i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ib3.hincome [pw=WTS_M], level(90)
	margins
	eststo: margins, dydx(*) level(90) post
	
/*
 Remember to drop starl( * 0.10 ** 0.05 *** 0.010) here for the final table  
 and therefore "star" in the final report is dropped.
 It is here now just for convenience for interpreting the results.
*/ 
	
esttab using table9.rtf, replace ///
	mtitles( "Medical Doctor" "Unmet Needs" ///
	"Availability" "Acceptability" "Accessibility" ) /// 
	title("Marginal Effects") cells(b(star fmt(3)) ci(fmt(2) par)) starl( * 0.10 ** 0.05 *** 0.010) ///
	nogaps nonumbers compress label

esttab using table9.tex, replace ///
	mtitles( "Medical Doctor" "Unmet Needs" ///
	"Availability" "Acceptability" "Accessibility" ) /// 
	title("Marginal Effects") cells(b(star fmt(3)) ci(fmt(2) par)) starl( * 0.10 ** 0.05 *** 0.010) ///
	nogaps nonumbers compress label substitute($ \$) fragment

**============================================================================**
***** THIS PART CREATE DESCRIPTIVE STATISTICS TABLE AND REGRESSION SEPARATELY FOR 
***** QUEBEC AND THE REST OF CANADA, TO COMPARE BETWEEN QUEBEC AND THE REST BY SEPARATE MODELS
 
***** Summary Statistics *****
***** Appendix Table B.1 ***** 

***** unmet Availability Acceptability Accessibility are missing for Quebec, then they are dropped


estpost sum regdoctor gender age race aborident immig edulevels homelang hincome residence if residence==1 [aweight = WTS_M]
est store asumsta1

estpost sum regdoctor gender age race aborident immig edulevels homelang hincome residence if residence==0 [aweight = WTS_M]
est store asumsta2

eststo ttest: estpost ttest regdoctor gender age race aborident ///
immig edulevels homelang hincome residence, by(residence) 

esttab asumsta1 asumsta2 ttest using tableB1.rtf, ///
	mtitles( "All" "Quebec" "Rest of Canada" "Diff." ) title("Descriptive Statistics by Residence")  ///	
	cell("mean(pattern(1 1 0) fmt(3)) & sd(pattern(1 1 0) par fmt(3)) b(star pattern(0 0 1) fmt(3))  &se(pattern(0 0 1) par fmt(3))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label noobs
	
esttab asumsta1 asumsta2 ttest using tableB1.tex, ///
	mtitles( "All" "Quebec" "Rest of Canada" "Diff." ) title("Descriptive Statistics by Residence")  ///	
	cell("mean(pattern(1 1 0) fmt(3)) & sd(pattern(1 1 0) par fmt(3)) b(star pattern(0 0 1) fmt(3))  &se(pattern(0 0 1) fmt(3) par([ ]))") ///
	addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001.") ///
	nogaps compress replace label fragment	

	
***** Logit *****
***** Appendix Table B.2 ***** 
est clear
logit regdoctor i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ///
ib3.hincome [pw=WTS_M] if residence ==1, level(90) 

predict pq if residence ==1
sum pq 
margins 
eststo pq: margins, dydx(*) level(90) post 

logit regdoctor i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ///
 ib3.hincome [pw=WTS_M] if residence ==0, level(90) 
predict pc if residence ==0
sum pc 
margins

eststo pc: margins, dydx(*) level(90) post


esttab using tableB2.rtf, replace ///
	mtitles( "Quebec" "Rest of Canada" ) /// 
	title("Marginal Effects") cells(b(star fmt(3)) ci(fmt(2) par)) starl( * 0.10 ** 0.05 *** 0.010) ///
	nogaps nonumbers compress label

esttab using tableB2.tex, replace ///
	mtitles( "Quebec" "Rest of Canada" ///
	"Availability" "Acceptability" "Accessibility" ) /// 
	title("Marginal Effects") cells(b(star fmt(3)) ci(fmt(2) par)) starl( * 0.10 ** 0.05 *** 0.010) ///
	nogaps nonumbers compress label substitute($ \$) fragment 

***** Appendix Table B.3 - calculate treatment effects ***** 

teffects ra (regdoctor i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ib3.hincome, logit) (residence)
predict y0hat

predict y1hat
gen ATE = y1hat - y0hat
sum ATE

* teffects nnmatch (regdoctor i.gender ib2.age i.race i.immig i.homelang ib2.edulevels ib3.hincome, logit) (residence)

* regdoctor i.gender ib2.age i.race i.immig i.homelang i.residence ib2.edulevels ib3.hincome
**============================================================================**
***** coefplot *****
**============================================================================**
/*
This plot displays the point estimates and their confidence intervals 
(coefficients and confidence intervals) to show the healthcare access disparities 
between Quebec and the rest of Canada 
*/

global infile="~/Desktop/Job application/Github/Sample codes"
coefplot pq pc, drop(_cons) xline(0) 
gr export "$infile/coefplot.eps", as(eps) preview(off) replace
gr export "$infile/coefplot.pdf", as(pdf) replace
*!epstopdf coefplot.eps

	
sjlog close, replace
	

