cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_6_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 6. Fundamentals vs. Sentiment Hypothesis ***
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
*** Figure 6.A. Global News Sentiment Impact (Equity Prices) ***

cap drop month
gen month = month(DATE)
cap drop year
gen year = year(DATE)

* Recession dummies from various definitions
merge m:1 year month using "..\data\recessions.dta" ///
	, keep(1 3) nogen keepusing(D1_DJ_GL)

preserve

* Recessions with split
foreach DV in returns { 
xtset weo DATE
tsfill
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach dd in D1_DJ_GL {
foreach DFM in factor_sent`s'_best_filter {

	xtset weo DATE
	
	if `impulse'==t_sent`s'_best_local loc art_cntrl t_art`s'_best_local
	
	if `"`DV'"'=="returns" {
		loc filepath equity
		loc prices returns
		loc glb_cntrl R_DJ_global
		loc vlm vlm_ma60_adj
		loc vol vol_ma60
	}
	
	keep weo* iso DATE AE F*_index ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'} F*_d_`DV'_h F*_d_`DV'_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} `dd'	
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	gen b_LP = 0
	gen se_LP = 0
	gen b_LP2 = 0
	gen se_LP2 = 0
	gen b_LP3 = 0
	gen se_LP3 = 0
	gen b_LP4 = 0
	gen se_LP4 = 0
	
	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_`DV'} F`i'_d_`DV'_h F`i'_d_`DV'_l ///
				${L_`vol'} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} if `dd'==1 ///
					, lag(`i') fe pooled 
			cap eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'} F`i'_d_`DV'_h F`i'_d_`DV'_l ///
				${L_`vol'} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} if `dd'==1 ///
					, lag(`i') fe pooled 
			cap eststo ols_LP`i'
			tab iso if e(sample), m
		}
		cap predict infhat
			loc error = _rc
			cap gen res`i' = F`i'_index - infhat
			cap drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_6a_bear.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 
	
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_index ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_`DV'} F`i'_d_`DV'_h F`i'_d_`DV'_l ///
				${L_`vol'} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} if `dd'==0 ///
					, lag(`i') fe pooled 
			cap eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_index ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'} F`i'_d_`DV'_h F`i'_d_`DV'_l ///
				${L_`vol'} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} if `dd'==0 ///
					, lag(`i') fe pooled 
			cap eststo ols_LP`i'
			tab iso if e(sample), m
		}
		cap predict infhat
			loc error = _rc
			cap gen res`i' = F`i'_index - infhat
			cap drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP3 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP3 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP4 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP4 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_6a_bull.csv" ///
		, se csv nocon ar2 nogaps replace ///
	b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
	stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $horizon +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $horizon +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP* dn*_LP*
	
	gen up_LP = b_LP + invnormal(1-sig1/2)*se_LP if _n <= ($horizon + 1)
	gen dn_LP = b_LP - invnormal(1-sig1/2)*se_LP if _n <= ($horizon + 1)

	gen up2_LP = b_LP + invnormal(1-sig2/2)*se_LP if _n <= ($horizon + 1)
	gen dn2_LP = b_LP - invnormal(1-sig2/2)*se_LP if _n <= ($horizon + 1)
	
	gen up_LP2 = b_LP2 + invnormal(1-sig1/2)*se_LP2 if _n <= ($horizon + 1)
	gen dn_LP2 = b_LP2 - invnormal(1-sig1/2)*se_LP2 if _n <= ($horizon + 1)

	gen up2_LP2 = b_LP2 + invnormal(1-sig2/2)*se_LP2 if _n <= ($horizon + 1)
	gen dn2_LP2 = b_LP2 - invnormal(1-sig2/2)*se_LP2 if _n <= ($horizon + 1)
	
	gen up_LP3 = b_LP3 + invnormal(1-sig1/2)*se_LP3 if _n <= ($horizon + 1)
	gen dn_LP3 = b_LP3 - invnormal(1-sig1/2)*se_LP3 if _n <= ($horizon + 1)

	gen up2_LP3 = b_LP3 + invnormal(1-sig2/2)*se_LP3 if _n <= ($horizon + 1)
	gen dn2_LP3 = b_LP3 - invnormal(1-sig2/2)*se_LP3 if _n <= ($horizon + 1)
	
	gen up_LP4 = b_LP4 + invnormal(1-sig1/2)*se_LP4 if _n <= ($horizon + 1)
	gen dn_LP4 = b_LP4 - invnormal(1-sig1/2)*se_LP4 if _n <= ($horizon + 1)

	gen up2_LP4 = b_LP4 + invnormal(1-sig2/2)*se_LP4 if _n <= ($horizon + 1)
	gen dn2_LP4 = b_LP4 - invnormal(1-sig2/2)*se_LP4 if _n <= ($horizon + 1)
	
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)
	save "..\results\figure_6a.dta", replace

	twoway (line up_LP t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_6a_bear.pdf" ///
		, as(pdf) replace 
	cap graph close

	twoway (line up_LP3 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP3 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP3 t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP4 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP4 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP4 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_6a_bull.pdf" ///
		, as(pdf) replace 
	cap graph close
	
} // end DFM loop
} // end recession loop
} // end varname loop
} // end sent type loop
} // end DV loop

use "..\results\figure_6a.dta", clear

foreach i in 2 4 {
	foreach t in b se {
		qui su `t'_LP`i' if t==20, d
		global `t'_LP`i' = r(max)
	}
	global h_LP`i' = ${b_LP`i'} + (invnormal(1-sig1/2) * ${se_LP`i'})
	global l_LP`i' = ${b_LP`i'} - (invnormal(1-sig1/2) * ${se_LP`i'})
}

clear
set obs 3

gen global_bear = .
replace global_bear = ${h_LP2} if _n==1
replace global_bear = ${b_LP2} if _n==2
replace global_bear = ${l_LP2} if _n==3

gen global_bull = .
replace global_bull = ${h_LP4} if _n==1
replace global_bull = ${b_LP4} if _n==2
replace global_bull = ${l_LP4} if _n==3

export excel using "..\results\results.xlsx", /// 
	sheet("figure_6a_data") cell(C3) sheetmodify firstrow(variables) // 

clear
set obs 1

gen global_bear = ""
replace global_bear = "Global Bear Market" if _n==1

gen global_bull = ""
replace global_bull = "Global Bull Market" if _n==1

export excel using "..\results\results.xlsx", /// 
	sheet("figure_6a_data") cell(C2) sheetmodify firstrow(variables) // 

restore


********************************************************************************
* Figure 6.B. Global News Sentiment Impact (Funds' Allocations) 

xtset weo DATE
foreach tt in FULL {
foreach cc in EM {
foreach tp in equity_etf equity_act {
foreach DV in `tp'_flow {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter { // 

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	
	if `"`cc'"'=="AE_EM" {
		loc cnd_cc = ""
	}
	if `"`cc'"'=="AE" {
		loc cnd_cc = "& AE==1"
	}
	if `"`cc'"'=="EM" {
		loc cnd_cc = "& AE==0"
	}
	loc prices returns
	loc glb_cntrl R_DJ_global
	loc rgn_cntrl R_MSCI_EM
	loc vlm vlm_ma60_adj
	loc vol vol_ma60

preserve
	
	keep weo* iso DATE AE F*_cum_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'pct} d_F*_cum_`DV'_h d_F*_cum_`DV'_l ///
				${L_`prices'} F0_d_returns_h F0_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* LP: One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizonFlows
	forvalues i=0/$horizonFlows {	
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_cum_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_`DV'pct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F0_d_returns_h F0_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled 
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_cum_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'pct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F0_d_returns_h F0_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled 
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_cum_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_6b_`DV'.csv" ///
		, se csv nocon ar2 nogaps replace ///
		b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) drop() order() /// 
		stats(N r2, fmt(%9.0f %9.3fc) labels("Observations" "R-squared")) 

	* Time horizon
	cap drop t
	gen t = _n-1 if _n <= $horizonFlows +1

	* Zero line
	cap drop zero
	gen zero = 0 if _n <= $horizonFlows +1

***** create confidence bands (in this case 90 and 95%) *****
	
	scalar sig1 = 0.05	 // specify significance level
	scalar sig2 = 0.3	 // specify significance level

	cap drop up*_LP* dn*_LP*
	forval pp = 1(1)2 {
		forval ss = 1(1)2 {
			cap noi gen up`ss'_LP`pp' = ///
				b_LP`pp' + invnormal(1-sig`ss'/2)*se_LP`pp' ///
				if _n <= ($horizonFlows + 1)
			cap noi gen dn`ss'_LP`pp' = ///
				b_LP`pp' - invnormal(1-sig`ss'/2)*se_LP`pp' ///
				if _n <= ($horizonFlows + 1)
		}
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)

	twoway (line up1_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line dn1_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
			(line up1_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line dn1_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
			ylabel(, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_6b_`DV'.pdf" ///
		, as(pdf) replace 
	cap graph close
	
	compress
	save "..\results\figure_6b_`DV'.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end equity bond loop
} // end cc loop
} // end tt loop

use "..\results\figure_6b_equity_etf_flow.dta", clear

foreach i in 2 {
	foreach t in b se {
		qui su `t'_LP`i' if t==20, d
		global p1_`t'_LP`i' = r(max)
	}
	global p1_h_LP`i' = ${p1_b_LP`i'} + (invnormal(1-sig1/2) * ${p1_se_LP`i'})
	global p1_l_LP`i' = ${p1_b_LP`i'} - (invnormal(1-sig1/2) * ${p1_se_LP`i'})
}


use "..\results\figure_6b_equity_act_flow.dta", clear

foreach i in 2 {
	foreach t in b se {
		qui su `t'_LP`i' if t==20, d
		global p2_`t'_LP`i' = r(max)
	}
	global p2_h_LP`i' = ${p2_b_LP`i'} + (invnormal(1-sig1/2) * ${p2_se_LP`i'})
	global p2_l_LP`i' = ${p2_b_LP`i'} - (invnormal(1-sig1/2) * ${p2_se_LP`i'})
}

clear
set obs 3

gen etf = .
replace etf = ${p1_h_LP2} if _n==1
replace etf = ${p1_b_LP2} if _n==2
replace etf = ${p1_l_LP2} if _n==3

gen act = .
replace act = ${p2_h_LP2} if _n==1
replace act = ${p2_b_LP2} if _n==2
replace act = ${p2_l_LP2} if _n==3

export excel using "..\results\results.xlsx", /// 
	sheet("figure_6b_data") cell(C3) sheetmodify firstrow(variables) // 

clear
set obs 1

gen etf = ""
replace etf = "ETFs" if _n==1

gen act = ""
replace act = "Active Funds" if _n==1

export excel using "..\results\results.xlsx", /// 
	sheet("figure_6b_data") cell(C2) sheetmodify firstrow(variables) // 

	
cap log close
