cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_2_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 2. Benchmark Results - Equity Returns ***
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
*** Figure 2.A. US only

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ///
			${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
			${other_dummies}
	
	keep if iso=="US"
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			newey2 F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		else if `i'==0 {
			newey2 F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using "..\results\figure_2a.csv" ///
		, se csv nocon ar2 nogaps replace ///
		b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
		stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
			fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
			fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
			lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)) ///
		, title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) 
	graph export "..\results\figure_2a.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2a.dta", replace
restore
} // end impulse loop
} // end sent type loop


* Figure 2.B - Panel Full Sample

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo* iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
			${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
			${other_dummies}
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat  
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_2b.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
		fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
		fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
		lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)), ///
		title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///
		title("")
	graph export "..\results\figure_2b.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2b.dta", replace
restore
	
} // end impulse loop
} // end sent type loop

********************************************************************************
* Robustness test: Figure 2.B - Panel Full Sample excluding US

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo* iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
			${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
			${other_dummies}
	
	drop if iso=="US"
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat  
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_2b_exUS.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
		fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
		fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
		lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)), ///
		title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///
		title("")
	graph export "..\results\figure_2b_exUS.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2b_exUS.dta", replace
restore
	
} // end impulse loop
} // end sent type loop


********************************************************************************
* Figure 2.A and 2.B without outlier dummies

*** Figure 2.A. US only without outlier dummies

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
		
	keep weo iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ///
			${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
			${other_dummies}
	
	keep if iso=="US"
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			newey2 F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} /// F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		else if `i'==0 {
			newey2 F`i'_index ${L_`impulse'} ///
				${L_returns} /// F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using "..\results\figure_2a_noDummies.csv" ///
		, se csv nocon ar2 nogaps replace ///
		b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
		stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
			fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
			fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
			lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)) ///
		, title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) 
	graph export "..\results\figure_2a_noDummies.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2a_noDummies.dta", replace
restore
} // end impulse loop
} // end sent type loop


* Figure 2.B - Panel Full Sample without dummies

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo* iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
			${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
			${other_dummies}
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat  
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} /// F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ///
				${L_returns} /// F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_2b_noDummies.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
		fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
		fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
		lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)), ///
		title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///
		title("")
	graph export "..\results\figure_2b_noDummies.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2b_noDummies.dta", replace
restore
	
} // end impulse loop
} // end sent type loop


********************************************************************************
* Figure 2.A. and 2.B. without global controls

*** Figure 2.A. US only without global controls

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
		
	keep weo iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ///
			${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
			${other_dummies}
	
	keep if iso=="US"
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			newey2 F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} /// ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		else if `i'==0 {
			newey2 F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} /// ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using "..\results\figure_2a_noGlbCtrls.csv" ///
		, se csv nocon ar2 nogaps replace ///
		b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
		stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
			fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
			fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
			lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)) ///
		, title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) 
	graph export "..\results\figure_2a_noGlbCtrls.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2a_noGlbCtrls.dta", replace
restore
} // end impulse loop
} // end sent type loop



* Figure 2.B - Panel Full Sample without global controls

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo* iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
			${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
			${other_dummies}
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat  
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} /// ${L_vix} ///
				${L_art1_best} /// ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} /// ${L_vix} ///
				${L_art1_best} /// ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_2b_noGlbCtrls.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
		fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
		fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
		lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)), ///
		title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///
		title("")
	graph export "..\results\figure_2b_noGlbCtrls.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2b_noGlbCtrls.dta", replace
restore
	
} // end impulse loop
} // end sent type loop


********************************************************************************
*** Robustness test: Not controlling for volume

*** Figure 2.A. US only

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ///
			${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
			${other_dummies}
	
	keep if iso=="US"
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			newey2 F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} /// ${L_vlm_ma60_adj} 
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		else if `i'==0 {
			newey2 F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} /// ${L_vlm_ma60_adj} 
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using "..\results\figure_2a_noVlm.csv" ///
		, se csv nocon ar2 nogaps replace ///
		b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
		stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
			fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
			fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
			lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)) ///
		, title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) 
	graph export "..\results\figure_2a_noVlm.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2a_noVlm.dta", replace
restore
} // end impulse loop
} // end sent type loop


* Figure 2.B - Panel Full Sample

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo* iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
			${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
			${other_dummies}
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat  
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_2b_noVlm.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
		fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
		fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
		lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)), ///
		title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///
		title("")
	graph export "..\results\figure_2b_noVlm.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2b_noVlm.dta", replace
restore
	
} // end impulse loop
} // end sent type loop


********************************************************************************
*** Restrict to balanced sample post 1992

*** Figure 2.A. US only

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ///
			${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
			${other_dummies}
	
	keep if iso=="US"
	keep if tin(1jan1992,)
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			newey2 F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		else if `i'==0 {
			newey2 F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ///
				${L_art1_best} ${L_vix} ${L_commo} ${L_R_DJ_global} /// 
				${other_dummies}, lag(`i') force
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using "..\results\figure_2a_balanced.csv" ///
		, se csv nocon ar2 nogaps replace ///
		b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
		stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
			fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
			fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
			lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)) ///
		, title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) 
	graph export "..\results\figure_2a_balanced.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2a_balanced.dta", replace
restore
} // end impulse loop
} // end sent type loop


* Figure 2.B - Panel Balanced Sample

xtset weo DATE
foreach s in 1 {
foreach impulse in sent`s'_best { 

preserve
	
	keep weo* iso DATE `impulse' ///
		F*_index ${L_`impulse'} ///
			${L_returns} F*_d_returns_h F*_d_returns_l ///
			${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
			${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
			${other_dummies}
	
	* Restrict to balanced sample
	keep if tin(1jan1992,)
	
	* Variables to store impulse response and SE
	cap drop b_LP se_LP
	gen b_LP = 0
	gen se_LP = 0

	* One regression for each horizon of the response
	eststo clear
	cap drop infhat  
	cap drop res0-res$US_horizon
	forvalues i=0/$US_horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_art1_best} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_index - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_2b_balanced.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $US_horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $US_horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP dn*_LP
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($US_horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($US_horizon + 1)
	
	twoway (rarea up_LP dn_LP t, ///
		fcolor(gs12) lcolor(white) lpattern(solid)) ///
		(rarea up2_LP dn2_LP t, ///
		fcolor(gs10) lcolor(white) lpattern(solid)) ///
		(line b_LP t, lcolor(blue) ///
		lpattern(solid) lwidth(thick)) /// 
		(line zero t, lcolor(black)), ///
		title(, color(black) size(medium)) ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.1(0.05)0.15, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) ///
		legend(off) ///
		title("")
	graph export "..\results\figure_2b_balanced.pdf", as(pdf) replace 
	cap graph close
	cap drop res0-res$US_horizon
	
	keep t zero up*_LP* dn*_LP* b_LP* se_LP*
	ds up*_LP* dn*_LP* b_LP* se_LP*
	drop if mi(t)
	save "..\results\figure_2b_balanced.dta", replace
restore
	
} // end impulse loop
} // end sent type loop


cap log close
