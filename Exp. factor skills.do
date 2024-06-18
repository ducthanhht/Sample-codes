

**============================================================================**
***** Do-file Transition from School to Work: The Role of Cognitive and Noncognitive Abilities *****
***** Statistics summary and exploratory factor analysis of skills *****
**============================================================================**

 
/*
* This do-file is to create the summary statistics and run exploratory factor analysis 
of skills to find whether there are factors that represent cognitive skills and 
noncognitive skills. The factor analysis based on both Kaiser's eigenvalue rule 
and scree tests. The main analysis for the paper is done in MATLAB.
* You need to change the directories to your own specifications in order to run.
* Data: The raw data was provided by Young Lives. I selected variables used in the paper 
and cleaned the data. The dataanalysis.dta is the final cleaned data.
* You need to change the directories to your own specifications in order to run 
*/

* You may need to install the following packages for use. 
* findit sjlatex
* net search sjlatex
* ssc install texdoc, replace
* ssc install estout, replace
* ssc install mediation, replace
* ssc install rmpw
* ssc install moremata
* which rmpw

********************************************************************************
**Data Preparation - Round 3 
********************************************************************************

clear all
macro drop _all
set more off
mata: mata clear
capture log close
sjlog using "Exploratory factor analysis", replace

cd "~/Desktop/Github/Sample codes"

use dataanalysis, clear

/*
*******************************************************************************
***** CLEAN AND MERGE DATA *****
********************************************************************************
do Household.do
do Experience.do
do Edu5.do
do Income.do
do MR3.do
do Paredu.do


merge 1:1 childid using household3
keep if _merge==3
drop _merge

merge 1:1 childid using household5
keep if _merge==3
drop _merge
merge 1:1 childid using Experience
keep if _merge==3
drop _merge

merge 1:1 childid using paredu
keep if _merge==3
drop _merge
*/

********************************************************************************
***** CHECK CORRELATION *****
********************************************************************************

estpost correlate zmath3 zcloze3 zppvt3 zSEF3 zSER3 zSES3, matrix listwise
esttab, unstack not noobs compress

egen double acog = rowmean(zrmath3 zrcloze3 zrppvt3)

egen double ancog = rowmean(zSEF3 zSER3 zSES3)

pwcorr acog zrmath3 zrcloze3 zrppvt3 ancog zSEF3 zSER3 zSES3, star(.01)

estpost correlate acog zrmath3 zrcloze3 zrppvt3 ancog zSEF3 zSER3 zSES3, matrix listwise
esttab, unstack not noobs compress

********************************************************************************
***** DESCRIPTIVE STATISTICS *****
********************************************************************************
estpost sum zrmath3 zrcloze3 zrppvt3 zSEF3 zSER3 zSES3 chsex age siblings wi_new ///
parlev urban aspiration COL USEC mincome hincome rexp
est store sumsta1

estpost sum zrmath3 zrcloze3 zrppvt3 zSEF3 zSER3 zSES3 chsex age siblings wi_new ///
parlev urban  aspiration COL USEC mincome hincome rexp if chsex==1
est store sumsta2

estpost sum zrmath3 zrcloze3 zrppvt3 zSEF3 zSER3 zSES3 chsex age siblings wi_new ///
parlev urban aspiration COL USEC mincome hincome rexp if chsex==0
est store sumsta3

esttab sumsta1 sumsta2 sumsta3 using table2.rtf, ///
	mtitles( "Full" "Female" "Male" ) title("Descriptive Statistics")  ///
	cells(mean(fmt(3))  sd(par fmt(3)))  ///
    nogaps nonumbers  replace compress label	 	
	
esttab sumsta1 sumsta2 sumsta3 using table2.tex, ///
	mtitles( "Full" "Female" "Male" ) title("Descriptive Statistics")  ///
	cells(mean(fmt(3))  sd(par fmt(3)))  ///
    nogaps nonumbers  replace compress label	

********************************************************************************
***** EXPLORATORY FACTOR ANALYSIS *****
********************************************************************************

****************** Cognitive skills ********************

factor zrppvt3 zrmath3 zrcloze3, pcf

****************** Scree plot - scree tests - cognitive skills ************

global infile="~/Documents/Thesis/T1/Latex"
screeplot 
gr export "$infile/screecog.eps", as(eps) preview(off) replace
*!epstopdf "$infile/screecog.eps", replace
gr export "$infile/screecog.pdf", as(pdf) replace

****************** Kaiser's eigenvalue - cognitive skills*******************

matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / 3
matrix colnames p = "Proportion"
matrix c = J(3,1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/3 {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
matrix list t

estadd matrix table = t

esttab, cells("table[Eigenvalue](t fmt(5)) table[Difference](t fmt(5))table[Proportion](t fmt(5)) table[Cumulative](t fmt(5))") ///
nogap noobs nonumber title(Factor analysis/correlation) ///
addnote("LR test: independent vs. saturated: chi2(3)  =  588.67 Prob>chi2 = 0.0000, Retained factors = 1, 593 observations" "Source: Authors' calculations")
	
esttab using pcfcog1.tex, tex replace ///
cells("table[Eigenvalue](t fmt(5)) table[Difference](t fmt(5)) table[Proportion](t fmt(5)) table[Cumulative](t fmt(5))") ///
nogap noobs nonumber title(Factor analysis/correlation) ///
addnote("LR test: independent vs. saturated:  chi2(3)  =  588.67 Prob>chi2 = 0.0000, Retained factors = 1, 593 observations" "Source: Authors' calculations")

esttab, cells("L[Factor1](t) Psi[Uniqueness]") nogap noobs nonumber ///
title(Factor loadings (pattern matrix) and unique variances) addnote("Source: auto.dta")

esttab using pcfcog2.tex, tex replace cells("L[Factor1](t) Psi[Uniqueness]") ///
nogap noobs nonumber title(Factor loadings (pattern matrix) and unique variances) ///
addnote("Source: Authors' calculations")


****************** Noncognitive skills ********************

factor zSES3 zSER3 zSEF3, pcf

****************** Scree plot - scree tests - noncognitive skills ************

screeplot 
gr export "$infile/screencog.eps", as(eps) preview(off) replace
*!epstopdf "$infile/screecog.eps", replace
gr export "$infile/screencog.pdf", as(pdf) replace

****************** Kaiser's eigenvalue - noncognitive skills*******************

matrix ev = e(Ev)'
matrix roweq ev = ""
matrix colnames ev = "Eigenvalue"

matrix d = ev - ( ev[2...,1] \ . )
matrix colnames d = "Difference"

matrix p = ev[1...,1] / 3
matrix colnames p = "Proportion"
matrix c = J(3,1,0)
matrix c[1,1] = p[1,1]
forvalues i=2/3 {
    matrix c[`i',1] = c[`=`i'-1',1] + p[`i',1]
    }
matrix colnames c = "Cumulative"

matrix t = ( ev , d , p , c )
matrix list t

estadd matrix table = t

esttab, cells("table[Eigenvalue](t fmt(5)) table[Difference](t fmt(5)) table[Proportion](t fmt(5)) table[Cumulative](t fmt(5))") /// 
nogap noobs nonumber title(Factor analysis/correlation) ///
addnote("LR test: independent vs. saturated:  chi2(3)  =  588.67 Prob>chi2 = 0.0000, Retained factors = 1, 593 observations" "Source: Authors' calculations")

esttab using pcfncog1.tex, tex replace ///
cells("table[Eigenvalue] (t fmt(5)) table[Difference](t fmt(5)) table[Proportion](t fmt(5)) table[Cumulative](t fmt(5))") ///
nogap noobs nonumber title(Factor analysis/correlation) ///
addnote("LR test: independent vs. saturated: chi2(3)  =  337.35 Prob>chi2 = 0.0000. Retained factors = 1, 593 observations" "Source: Authors' calculations")

esttab, cells("L[Factor1](t) Psi[Uniqueness]") nogap noobs nonumber ///
title(Factor loadings (pattern matrix) and unique variances) addnote("Source: Authors' calculations")

esttab using pcfncog2.tex, tex replace cells("L[Factor1](t) Psi[Uniqueness]") ///
nogap noobs nonumber title(Factor loadings (pattern matrix) and unique variances) ///
addnote("Source: Authors' calculations")

factor ppvt_co cloze_co math_co, fa(1) 
rotate 
predict cogR3
egen zcogR3= std(cogR3)


factor ppvt_co cloze_co math_co
rotate 


factor rppvt3 rmath3 rcloze3 
rotate 
predict rcogR3
egen zrcogR3= std(rcogR3)

factor zrppvt3 zrmath3 zrcloze3 
rotate 
factor zSER3 zSEF3 zSES3, pcf
predict ncogR3
egen zncogR3= std(ncogR3)

factor zSES3 zSEF3 zSER3
rotate 

factor zhfa chweight YRHLTHR3
rotate 

factor zzhfa zchweight zYRHLTHR3
rotate 
predict healthR3

keep childid hincome lnhincome mincome lnmincome chsex exp exp2 rexp rexp2 urban ///
 Uni USEC wi_new wi_new5 paredu momedu dadedu caredu headedu parlev dadlev momlev ///
 urban5 zmath3 zcloze3 zppvt3 zrmath3 zrcloze3 zrppvt3 zSEF3 zSER3 zSES3 rSEF3 ///
 rSER3 rSES3 siblings aspiration zaspiration ageyear5 ageyear52 zzhfa zchweight ///
 zYRHLTHR3 ENRSCHR5 STLL18R5 COL COL1 GRDE18R5 HGHQULR5 phys visn resp NMTMINR3 ///
 CPRVSNR3 CRSPPRR3 CEYGLSR3 LVLEDCR3     

********************************************************************************
***** EXPORT DATA FOR MAIN ANALYSIS IN MATLAB *****
********************************************************************************

*************************
use dataanalysis, clear
replace urban5 = urban if urban5==.
keep childid chsex urban5
export delimited using Pers.csv, replace nolabel 

*************************
use dataanalysis, clear
keep childid zrppvt3 zrmath3 zrcloze3  zSES3 zSEF3 zSER3
order childid zrppvt3 zrmath3 zrcloze3 zSES3 zSEF3 zSER3  
export delimited using Measures.csv, replace nolabel 

*************************
use dataanalysis, clear
keep childid hincome COL chsex urban5 rexp rexp2 siblings wi_new5 parlev aspiration
order childid hincome COL chsex urban5 rexp rexp2 siblings wi_new5 parlev aspiration 
export delimited using Outcomes.csv, replace nolabel 

********************************************************************************
sjlog close, replace
