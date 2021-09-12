cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_a10_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure A10. Behavior of the News Sentiment Index - Good vs. Bad Times
********************************************************************************

use "..\data\regsample.dta", clear
xtset weo DATE

* Bull/bear market dummy
gen month = month(DATE)
gen quarter = quarter(DATE)
gen year = year(DATE)
merge m:1 year month using "..\data\recessions.dta", keep(1 3) nogen
merge m:1 weo year month using "..\data\recessionsIndex.dta" ///
	, keepusing(D1_index D0_index) keep(1 3) nogen

* Negative real GDP growth (YoY) 
merge m:1 weo year quarter using "..\data\IFS_09-25-2020 21-57-15-95.dta" ///
	, keepusing(neg_g4_rgdp) keep(1 3) nogen

* Raw sentiment index (no rescaling)
cap noi drop sent1_best
merge 1:1 weo DATE using "..\data\news.dta" ///
	, nogen keep(1 3) ///
	keepusing(positive negative sent1_best positiveTonal negativeTonal)
ren sent1_best raw_sent1_best

* Days with Negative stock returns
xtset weo DATE
gen byte negReturns = (returns < 0)
	tab negReturns, m

* Various definitions of Good vs. Bad Times
foreach dummy in neg_g4_rgdp D1_index negReturns {
	
	qui su raw_sent1_best if `dummy'==0, d
	global mean_`dummy'0 = r(mean)
	global p50_`dummy'0 = r(p50)
	
	qui su raw_sent1_best if `dummy'==1, d
	global mean_`dummy'1 = r(mean)
	global p50_`dummy'1 = r(p50)
}

qui su raw_sent1_best, d
global mean_all = r(mean)
global p50_all = r(p50)

preserve

clear
set obs 7

gen Mean = .
replace Mean = ${mean_neg_g4_rgdp0} if _n==1
replace Mean = ${mean_neg_g4_rgdp1} if _n==2
replace Mean = ${mean_D1_index0} if _n==3
replace Mean = ${mean_D1_index1} if _n==4
replace Mean = ${mean_negReturns0} if _n==5
replace Mean = ${mean_negReturns1} if _n==6
replace Mean = ${mean_all} if _n==7

gen Median = .
replace Median = ${p50_neg_g4_rgdp0} if _n==1
replace Median = ${p50_neg_g4_rgdp1} if _n==2
replace Median = ${p50_D1_index0} if _n==3
replace Median = ${p50_D1_index1} if _n==4
replace Median = ${p50_negReturns0} if _n==5
replace Median = ${p50_negReturns1} if _n==6
replace Median = ${p50_all} if _n==7

export excel using "..\results\results.xlsx", /// 
	sheet("figure_a10_data") cell(B2) sheetmodify firstrow(variables) // 

restore

cap log close
