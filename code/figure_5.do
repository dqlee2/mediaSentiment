cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_5_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 5. Global News Sentiment vs. VIX ***
********************************************************************************

use "..\data\regsample.dta", clear

* Shorthand for row vectors
ds returns vlm_ma* vol_ma* ///
	equity_flowpct ///
	equity_loc_flowpct equity_fgn_flowpct ///
	equity_etf_flowpct equity_act_flowpct ///
	vix vix_w commo R_DJ_global ///
	R_MSCI_EM /// 
	epu ln_epu ///
	cesi_usd cesi_eur cesi_cny cesi_g10 ///
	art0 art1 art2 sent1 sent2 ///
	art1_best art2_best sent1_best sent2_best ///
	t_sent1_best_local t_art1_best_local ///
	t_sent2_best_local t_art2_best_local ///
	factor_sent1_best_filter /// 
	negative factor_negative_filter ///
	negativeTonal factor_negativeTonal ///
	artTonal1 ///
	sentu1 artu1 sentu1_best artu1_best ///
	t_sentu1_best_local t_artu1_best_local ///
	factor_sentu1_best_filter //
foreach vv in `r(varlist)' {
	global L_`vv'
	forval i = 1(1)$Nlags {
		global L_`vv' ${L_`vv'} L`i'_`vv'
	}
}

* Shorthand for dummy controls
ds dow2-dow5 
global other_dummies = "`r(varlist)'"

********************************************************************************
* Figure 5.A - Correlation

preserve

* produced by dfm.do
use "..\data\factor_sent1_best.dta", clear

merge 1:1 DATE using "..\data\VIXCLS.dta" ///
	, nogen keepusing(vix vix_w)
merge 1:1 DATE using "..\data\EPU_All_Daily_Policy_Data.dta" ///
	, nogen keepusing(epu)
tsset DATE

lab var factor_sent1_best_filter "Global News Sentiment Index"
lab var vix "VIX"

keep if year(DATE)<2017
keep if year(DATE)>1990
tsset DATE

ds vix factor_sent1_best_filter epu
foreach vv in `r(varlist)' {
	gen _raw = `vv'
	cap drop _mean _sd _raw
	egen _mean = mean(_raw)
	egen _sd = sd(_raw)
	di "gen `vv' = (_raw - _mean) / _sd"
	gen std_`vv' = (_raw - _mean) / _sd
	cap drop _mean _sd _raw
}

* Correlation between global news sentiment and the VIX
corr vix factor_sent1_best_filter 

cap drop month
gen month = month(DATE)
cap drop year
gen year = year(DATE)

* Recession dummies from various definitions
merge m:1 year month using "..\data\recessions.dta" ///
	, keep(1 3) nogen keepusing(D1_DJ_GL)

* Correlation between global news sentiment and the VIX during bull/bear markets
corr vix factor_sent1_best_filter if D1_DJ_GL==1 // bear
corr vix factor_sent1_best_filter if D1_DJ_GL==0 // bull
	
* Figure 5.A - Correlation

graph twoway (scatter factor_sent1_best_filter vix ///
		if tin(01jan1991, 01jan2016), color(gs6) msize(0.9)) ///
	(lfit factor_sent1_best_filter vix ///
		if tin(01jan1991, 01jan2016), lcolor(black)) ///
	, ytitle("Global News Sentiment Index", size(medlarge)) ///
	ylabel(, labsize(medlarge)) ///
	xtitle("VIX", size(medlarge)) /// 
	xlabel(, labsize(medlarge)) ///
	graphregion(color(white)) legend(off)
graph export ///
	"..\results\figure_5a.pdf" ///
	, as(pdf) replace 
cap graph close

restore

********************************************************************************
* Figure 5.B - Variance Decompositions

preserve

foreach DV in returns {
xtset weo DATE
tsfill
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

	if `impulse'==t_sent`s'_best_local loc art_cntrl t_art`s'_best_local
	
	loc filepath equity
	loc prices returns
	loc glb_cntrl R_DJ_global
	loc vlm vlm_ma60_adj
	loc vol vol_ma60
	
	keep weo DATE F*_index ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'} F*_d_*_h F*_d_*_l ///
				${L_`vol'} ${L_`vlm'} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}
				
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP* r2_LP*
	cap drop res*
	gen b_LP = 0
	gen se_LP = 0
	gen b_LP2 = 0
	gen se_LP2 = 0
	gen b_LP3 = 0
	gen se_LP3 = 0
	gen b_LP4 = 0
	gen se_LP4 = 0
	gen r2_LP1 = 0
	gen r2_LP2 = 0
	gen r2_LP3 = 0
	gen r2_LP4 = 0
	gen r2_LP5 = 0
	
	eststo clear
	
	* No sentiment, no VIX
	cap drop infhat 
	loc r2
	cap drop res0-res$horizon
	forvalues i=0/$horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			reghdfe F`i'_index ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} /// 
				${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s1_LP`i'
		}
		else if `i'==0 {
			reghdfe F`i'_index ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ///
				${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s1_LP`i'
		}
		loc r2 = `e(r2_a)'
		cap predict infhat
			loc error = _rc
			cap gen res`i' = F`i'_index - infhat
			cap drop infhat 
		
		* Store coefficient and se on first lag
		di "replace r2_LP1 = `e(r2_a)' if _n==`i'+2"
		replace r2_LP1 = `r2' if _n==`i'+2
	}
	
	* local sentiment
	cap drop infhat 
	loc r2
	cap drop res0-res$horizon
	forvalues i=0/$horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			reghdfe F`i'_index ${L_`impulse'} ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s2_LP`i'
		}
		else if `i'==0 {
			reghdfe F`i'_index ${L_`impulse'} ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s2_LP`i'
		}
		loc r2 = `e(r2_a)'
		cap predict infhat
			loc error = _rc
			cap gen res`i' = F`i'_index - infhat
			cap drop infhat 
		
		* Store coefficient and se on first lag
		di "replace r2_LP2 = `e(r2_a)' if _n==`i'+2"
		replace r2_LP2 = `r2' if _n==`i'+2
	} 
	
	* glb sentiment, loc sentiment, minus VIX
	cap drop infhat 
	loc r2
	cap drop res0-res$horizon
	forvalues i=0/$horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			reghdfe F`i'_index ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s3_LP`i'
		}
		else if `i'==0 {
			reghdfe F`i'_index ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s3_LP`i'
		}
		loc r2 = `e(r2_a)'
		cap predict infhat
			loc error = _rc
			cap gen res`i' = F`i'_index - infhat
			cap drop infhat 
		
		* Store coefficient and se on first lag
		di "replace r2_LP3 = `e(r2_a)' if _n==`i'+2"
		replace r2_LP3 = `r2' if _n==`i'+2
	}
	
	* glb sentiment, loc sentiment, vix
	cap drop infhat 
	loc r2
	cap drop res0-res$horizon
	forvalues i=0/$horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			reghdfe F`i'_index ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s4_LP`i'
		}
		else if `i'==0 {
			reghdfe F`i'_index ${L_`impulse'} ///
				${L_`DV'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}, absorb(weo) vce(robust)
			cap eststo ols_s4_LP`i'
		}
		loc r2 = `e(r2_a)'
		cap predict infhat
			loc error = _rc
			cap gen res`i' = F`i'_index - infhat
			cap drop infhat 
		
		* Store coefficient and se on first lag
		di "replace r2_LP4 = `e(r2_a)' if _n==`i'+2"
		replace r2_LP4 = `r2' if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_5b.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $horizon +1

	keep t zero r2_LP*
	drop if mi(t)
	save "..\results\figure_5b.dta", replace
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop

use "..\results\figure_5b.dta", clear

gen r2_LP_1_2 = (r2_LP2 - r2_LP1) * 100
gen r2_LP_2_3 = (r2_LP3 - r2_LP2) * 100
gen r2_LP_3_4 = (r2_LP4 - r2_LP3) * 100

gen blank = 0
tsset t
drop if t==0

graph bar r2_LP_1_2 r2_LP_2_3 r2_LP_3_4 blank ///
		, over(t, label(labsize(medlarge))) stack /// 
	bar(1, color(black)) bar(2, color(gs5)) bar(3, color(gs10)) ///
	ytitle("Marginal Contribution to Adj. R2 (%)", size(medlarge)) ylabel(#3) ///
	graphregion(color(white) margin(medlarge)) title("") /// 
	ylabel(0.0(.2)0.8, labsize(medlarge) format(%9.1f)) ///
	b1title("Horizon (h Days)", size(medlarge)) /// 
	legend(order(1 "Local Sentiment" 2 "Global Sentiment" 3 "VIX") ///
	symxsize(*.3) symysize(*.5) size(medlarge) row(1))
graph export "..\results\figure_5b.pdf", as(pdf) replace 
graph close

restore


cap log close
