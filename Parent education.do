
**============================================================================**
/*
This do-file is to correct mom's and dad's education across 5 rounds of YLS and 
create mom's and dad's education profile:

* Step 1: If educational data is missing, copy nonmissing value from the succeeding round.
* Step 2: If it is still missing, copy nonmissing value from the preceeding round.
* Step 3: if the value is greater than the value of succeeding round, replace ///
and use the value of the succeeding round.

*/
**============================================================================**

CORRECTIONS FOR MOTHER EDUCATION

clear all
macro drop _all
set more off
mata: mata clear
capture log close

sjlog using "Parental education", replace

cd "~/Desktop/Github/Sample codes"

			
********************************************************************************
***** MOM'S EDUCATION *****
********************************************************************************
			
			
use CHILDID ID SEX RELATE YRSCHOOL using round1/vnsubsec2householdroster8.dta, clear

gen mother=1 if RELATE==1 & SEX==2
keep if mother==1                                   
recode RELATE YRSCHOOL (88 99=.)
recode YRSCHOOL (45=13) (46=14) (42 43 44=.)
rename CHILDID childid
rename ID momid
rename YRSCHOOL momedu
keep childid mom*
g round=1
tempfile  mom1
save     `mom1'	

use CHILDID ID MEMSEX LIVHSE GRADE RELATE CHGRADE using Round2/vnsubhouseholdmember12.dta, clear
gen mother=1 if RELATE==1 & MEMSEX==2
keep if mother==1 
recode LIVHSE RELATE GRADE CHGRADE (77 79 88 99=.)
rename CHILDID childid
rename ID momid
rename LIVHSE momlive
rename GRADE momedu
replace momedu=CHGRADE if missing(momedu)
keep childid mom*
g round=2
tempfile  mom2
save     `mom2'


use CHILDID ID MEMSEX RELATE LIVHSE GRADE using Round3/vn_oc_householdmemberlevel.dta, clear
gen mother=1 if RELATE==1 & MEMSEX==2
keep if mother==1 
recode LIVHSE (4=2) (77 79 88 99 5=.)
recode GRADE (16=28) (17=29)
rename CHILDID childid
rename ID momid
rename LIVHSE momlive
rename GRADE momedu			
recode momedu (18=17) (19=16) (77 79 88 99=.)
keep childid mom*		
g round=3
tempfile  mom3
save     `mom3'	

use CHILDCODE MEMIDR4 MEMSEXR4 MEMAGER4 RELATER4 GRDE18R4 LIVHSER4 YRDIEDR4 using Round4/VN_R4_OCHH_HouseholdRosterR4.dta, clear
recode GRDE18R4 (77 79 88 99=.) 
recode MEMAGER4 (-88 -77=.)
gen mother=1 if RELATER4==1 & MEMSEXR4==2
gen childid="VN"+string(CHILDCODE, "%06.0f")
keep if mother==1	
recode LIVHSER4 (4=2) (77 79 88 99 5=.)			
rename MEMIDR4 momid
rename MEMAGER4 momage
rename LIVHSER4 momlive
rename GRDE18R4 momedu
rename YRDIEDR4 momyrdied
drop if childid=="VN110072" & momid==6
drop if childid=="VN170039" & momid==7
keep childid mom*
g round=4
tempfile  mom4
save     `mom4'	

use CHILDCODE MEMIDR5 MEMSEXR5 MEMAGER5 RELATER5 GRDE18R5 LIVHSER5 YRDIEDR5 using Round5/vn_r5_ochh_householdrosterr5, clear

recode GRDE18R5 (77 79 88 99=.) 
recode MEMAGER5 (-88 -77 1 2=.)
gen mother=1 if RELATER5==1 & MEMSEXR5==2
gen childid="VN"+string(CHILDCODE, "%06.0f")
keep if mother==1	
recode LIVHSER5 (4=2) (77 79 88 99 5=.)			
rename MEMIDR5 momid
rename MEMAGER5 momage
rename LIVHSER5 momlive
rename GRDE18R5 momedu
rename YRDIEDR5 momyrdied
drop if childid=="VN041002" & momid==7
drop if childid=="VN110072" & momid==6
drop if childid=="VN170039" & momid==7
keep childid mom*
g round=5
tempfile  mom5
save     `mom5'					


	* MERGE
			use `mom1', clear
			forvalues i=2/5 {
				qui append using `mom`i''
				}					
			sort childid round
			tempfile    mom
			save       `mom'		
		
			use childid round momid using `mom', clear
			egen momid2=mode(momid), by(childid) 
			drop momid
			merge 1:1 childid round using `mom', nogen
			recode momage momedu momlive momyrdied (*=.) if momid!=momid2
			drop momid
			rename momid2 momid
			sort childid round
			tempfile    mom
			save       `mom'			
						
			sort childid round
			keep childid momid momedu round
			g inround=1
			reshape wide momedu inround, i(childid momid) j(round)
		
			* Step 1:
			local r=4
			forvalues i=1/4 {
				local j=`r'+1
				replace momedu`r'=momedu`j' if missing(momedu`r')
				local r=`r'-1
				}
		
			* Step 2:
			forvalues i=1/4 {
				local j=`i'+1
				replace momedu`j'=momedu`i' if missing(momedu`j')
				}
				
			* Step 3:	
			local r=4
			forvalues i=1/4 {
				local j=`r'+1
				replace momedu`r'=momedu`j' if momedu`r'>momedu`j' 
				local r=`r'-1
				}
			reshape long
			keep if inround==1
			drop inround
			rename momedu momeducorr
			
			merge 1:1 childid round using `mom', nogen
			drop momedu
			rename momeducorr momedu
			
			replace momlive=3 if momlive[_n-1]==3 & childid==childid[_n-1]
			recode momage (*=.) if momlive==3
			bys childid: egen diedround=min(round) if momlive==3
			bys childid: egen yrdied=min(momyrdied) if momlive==3
			replace momyrdied=yrdied
			recode momage momedu momyrdied momlive (*=.) if diedround!=round & diedround!=.
			recode momage momedu (*=.) if momlive==3			
			keep childid round mom*

label var momedu		"Mother's level of education"
			label define educ1  0 "None" ///
							   1 "Grade 1" ///
							   2 "Grade 2" ///
							   3 "Grade 3" ///
							   4 "Grade 4" ///
							   5 "Grade 5" ///
							   6 "Grade 6" ///
							   7 "Grade 7" ///
							   8 "Grade 8" ///
							   9 "Grade 9" ///
							  10 "Grade 10" ///
							  11 "Grade 11" ///
							  12 "Grade 12" ///
							  13 "Post-secondary, vocational" ///
							  14 "University" ///
							  15 "Masters, doctorate" ///
							  28 "Adult literacy" ///
							  29 "Religious education" ///
							  30 "Other" 
label values momedu educ1			
			
drop if round!=3	
keep childid momedu 		
save momedu, replace	

			
********************************************************************************
***** DAD'S EDUCATION *****
********************************************************************************
			
			
			
use CHILDID ID SEX RELATE YRSCHOOL using Round1/vnsubsec2householdroster8.dta, clear

gen father=1 if RELATE==1 & SEX==1
keep if father==1                                   
recode RELATE YRSCHOOL (88 99=.)
recode YRSCHOOL (45=13) (46=14) (42 43 44=.)
rename CHILDID childid
rename ID dadid
rename YRSCHOOL dadedu
keep childid dad*
g round=1
tempfile  dad1
save     `dad1'	

use CHILDID ID MEMSEX LIVHSE GRADE RELATE CHGRADE using Round2/vnsubhouseholdmember12.dta, clear
gen father=1 if RELATE==1 & MEMSEX==1
keep if father==1 
recode LIVHSE RELATE GRADE CHGRADE (77 79 88 99=.)
rename CHILDID childid
rename ID dadid
rename LIVHSE dadlive
rename GRADE dadedu
replace dadedu=CHGRADE if missing(dadedu)
keep childid dad*
g round=2
tempfile  dad2
save     `dad2'


use CHILDID ID MEMSEX RELATE LIVHSE GRADE using Round3/vn_oc_householdmemberlevel.dta, clear
gen father=1 if RELATE==1 & MEMSEX==1
keep if father==1 
recode LIVHSE (4=2) (77 79 88 99 5=.)
recode GRADE (16=28) (17=29)
rename CHILDID childid
rename ID dadid
rename LIVHSE dadlive
rename GRADE dadedu			
recode dadedu (18=17) (19=16) (77 79 88 99=.)
keep childid dad*		
g round=3
tempfile  dad3
save     `dad3'	

use CHILDCODE MEMIDR4 MEMSEXR4 MEMAGER4 RELATER4 GRDE18R4 LIVHSER4 YRDIEDR4 using Round4/VN_R4_OCHH_HouseholdRosterR4.dta, clear
recode GRDE18R4 (77 79 88 99=.) 
recode MEMAGER4 (-88 -77=.)
gen father=1 if RELATER4==1 & MEMSEXR4==1
gen childid="VN"+string(CHILDCODE, "%06.0f")
keep if father==1	
recode LIVHSER4 (4=2) (77 79 88 99 5=.)			
rename MEMIDR4 dadid
rename MEMAGER4 dadage
rename LIVHSER4 dadlive
rename GRDE18R4 dadedu
rename YRDIEDR4 dadyrdied
keep childid dad*
g round=4
tempfile  dad4
save     `dad4'	

use CHILDCODE MEMIDR5 MEMSEXR5 MEMAGER5 RELATER5 GRDE18R5 LIVHSER5 YRDIEDR5 using Round5/vn_r5_ochh_householdrosterr5, clear

recode GRDE18R5 (77 79 88 99=.) 
recode MEMAGER5 (-88 -77 1 2=.)
gen father=1 if RELATER5==1 & MEMSEXR5==1
gen childid="VN"+string(CHILDCODE, "%06.0f")
keep if father==1	
recode LIVHSER5 (4=2) (77 79 88 99 5=.)			
rename MEMIDR5 dadid
rename MEMAGER5 dadage
rename LIVHSER5 dadlive
rename GRDE18R5 dadedu
rename YRDIEDR5 dadyrdied
keep childid dad*
g round=5
tempfile  dad5
save     `dad5'					


	* MERGE
			use `dad1', clear
			forvalues i=2/5 {
				qui append using `dad`i''
				}					
			sort childid round
			tempfile    dad
			save       `dad'			

			use childid round dadid using `dad', clear
			egen dadid2=mode(dadid), by(childid) 
			replace dadid2=4 if childid=="VN180011"
			drop dadid
			merge 1:1 childid round using `dad', nogen
			recode dadage dadedu dadlive dadyrdied (*=.) if dadid!=dadid2
			drop dadid
			rename dadid2 dadid
			sort childid round
			tempfile    dad
			save       `dad'		
			
			sort childid round
			keep childid dadid dadedu round
			g inround=1
			reshape wide dadedu inround, i(childid dadid) j(round)
		
			* Step 1:
			local r=4
			forvalues i=1/4 {
				local j=`r'+1
				replace dadedu`r'=dadedu`j' if missing(dadedu`r')
				local r=`r'-1
				}
		
			* Step 2:
			forvalues i=1/4 {
				local j=`i'+1
				replace dadedu`j'=dadedu`i' if missing(dadedu`j')
				}
				
			* Step 3:	
			local r=4
			forvalues i=1/4 {
				local j=`r'+1
				replace dadedu`r'=dadedu`j' if dadedu`r'>dadedu`j' 
				local r=`r'-1
				}
			reshape long
			keep if inround==1
			drop inround
			rename dadedu dadeducorr
			
			merge 1:1 childid round using `dad', nogen
			drop dadedu
			rename dadeducorr dadedu
			
			
			replace dadlive=3 if dadlive[_n-1]==3 & childid==childid[_n-1]
			recode dadage (*=.) if dadlive==3
			bys childid: egen diedround=min(round) if dadlive==3
			bys childid: egen yrdied=min(dadyrdied) if dadlive==3
			replace dadyrdied=yrdied
			recode dadage dadedu dadyrdied dadlive (*=.) if diedround!=round & diedround!=.
			recode dadage dadedu (*=.) if dadlive==3			
			keep childid round dad*
			
label var dadedu		"Father's level of education"
				label define educ2  0 "None" ///
							   1 "Grade 1" ///
							   2 "Grade 2" ///
							   3 "Grade 3" ///
							   4 "Grade 4" ///
							   5 "Grade 5" ///
							   6 "Grade 6" ///
							   7 "Grade 7" ///
							   8 "Grade 8" ///
							   9 "Grade 9" ///
							  10 "Grade 10" ///
							  11 "Grade 11" ///
							  12 "Grade 12" ///
							  13 "Post-secondary, vocational" ///
							  14 "University" ///
							  15 "Masters, doctorate" ///
							  28 "Adult literacy" ///
							  29 "Religious education" ///
							  30 "Other" 
			label values dadedu educ2
			
drop if round!=3
keep childid dadedu			
save dadedu, replace	

merge 1:1 childid using momedu 
drop _merge

save /momdadedu, replace

use vietnam_constructed.dta, clear
drop if round!=3
drop if yc==1
keep childid round momlive dadlive caredu caresex carerel headedu headsex headrel

merge 1:1 childid using momdadedu
drop _merge

recode momedu dadedu caredu headedu (28=0)

replace dadedu=caredu if caresex==1 & carerel==1&dadedu==.
replace momedu=caredu if caresex==2 & carerel==1&momedu==.

replace dadedu=headedu if headsex==1 & headrel==1&dadedu==.
replace momedu=headedu if headsex==2 & headrel==1&momedu==.			
			
gen paredu=max(momedu, dadedu)
replace paredu=caredu if momedu==.&dadedu==.
replace paredu=caredu if paredu==.

replace dadedu=0 if dadedu==.
replace momedu=0 if momedu==.


gen parlev=1 if paredu<5
replace parlev=2 if paredu>=5&paredu<9
replace parlev=3 if paredu>=9&paredu<12
replace parlev=4 if paredu==12
replace parlev=5 if paredu>12&paredu!=.

gen dadlev=1 if dadedu<5
replace dadlev=2 if dadedu>=5&dadedu<9
replace dadlev=3 if dadedu>=9&dadedu<12
replace dadlev=4 if dadedu==12
replace dadlev=5 if dadedu>12&dadedu!=.

gen momlev=1 if momedu<5
replace momlev=2 if momedu>=5&momedu<9
replace momlev=3 if momedu>=9&momedu<12
replace momlev=4 if momedu==12
replace momlev=5 if momedu>12&momedu!=.

replace dadedu=14 if dadedu==13
replace dadedu=16 if dadedu==14

replace momedu=14 if momedu==13
replace momedu=16 if momedu==14

save paredu, replace

sjlog close, replace




	
