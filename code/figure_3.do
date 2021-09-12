cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_3_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 3. Global vs. Local Sentiment Shocks - Equity Returns ***
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

* Median absolute deviation of returns
* on the regression sample at first horizon

reghdfe F0_index ${L_t_sent1_best_local} ${L_factor_sent1_best_filter} ///
	${L_returns} F0_d_returns_h F0_d_returns_l ///
	${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
	${L_t_art1_best_local} ${L_commo} ${L_R_DJ_global} ///
	${other_dummies} ///
	, absorb(weo) 
su F0_index if e(sample), d
di "Median absolute deviation of returns: `r(p50)'"

********************************************************************************
* Figure 3.A – Panel Full Sample
* Figure 3.B – Panel – excl. GFC

xtset weo DATE
foreach tt in FULL exGFC {
foreach cc in AE_EM {
foreach DV in index {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

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

preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	
	if `"`tt'"'=="exGFC" {
		drop if tin(1sep2008, 1sep2009)
		qui xtset weo DATE
		qui cap drop notmiss
		qui gen notmiss = 1 
		qui cap drop SDATE // ignore gaps 
		qui bys weo: gen SDATE = cond(notmiss, sum(notmiss), .)
		xtset weo SDATE
	}
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// weo_* 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// weo_* 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_3_`tt'_`cc'.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = ///
			b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = ///
			b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = ///
			b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = ///
			b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)

	twoway (line up_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(-0.1(0.1)0.6, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_3_`tt'_`cc'.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_3_`tt'_`cc'.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop

* Figure 3.C – Advanced Economies
* Figure 3.D – Emerging Markets

xtset weo DATE
foreach tt in FULL {
foreach cc in AE EM {
foreach DV in index {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

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
	
preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	
	if `"`tt'"'=="exGFC" {
		drop if tin(1sep2008, 1sep2009)
		qui xtset weo DATE
		qui cap drop notmiss
		qui gen notmiss = 1 
		qui cap drop SDATE // ignore gaps 
		qui bys weo: gen SDATE = cond(notmiss, sum(notmiss), .)
		xtset weo SDATE
	}
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_3_`tt'_`cc'.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = ///
			b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = ///
			b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = ///
			b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = ///
			b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)

	twoway (line up_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(-0.1(0.1)0.6, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_3_`tt'_`cc'.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_3_`tt'_`cc'.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop

********************************************************************************
*** Robustness test: Not controlling for volume

* Figure 3.A – Panel Full Sample
* Figure 3.B – Panel – excl. GFC

xtset weo DATE
foreach tt in FULL exGFC {
foreach cc in AE_EM {
foreach DV in index {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

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

preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	
	if `"`tt'"'=="exGFC" {
		drop if tin(1sep2008, 1sep2009)
		qui xtset weo DATE
		qui cap drop notmiss
		qui gen notmiss = 1 
		qui cap drop SDATE // ignore gaps 
		qui bys weo: gen SDATE = cond(notmiss, sum(notmiss), .)
		xtset weo SDATE
	}
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// weo_* 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// weo_* 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_3_`tt'_`cc'_noVlm.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = ///
			b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = ///
			b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = ///
			b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = ///
			b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)

	twoway (line up_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(-0.1(0.1)0.6, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_3_`tt'_`cc'_noVlm.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_3_`tt'_`cc'_noVlm.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop

* Figure 3.C – Advanced Economies
* Figure 3.D – Emerging Markets

xtset weo DATE
foreach tt in FULL {
foreach cc in AE EM {
foreach DV in index {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

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
	
preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	
	if `"`tt'"'=="exGFC" {
		drop if tin(1sep2008, 1sep2009)
		qui xtset weo DATE
		qui cap drop notmiss
		qui gen notmiss = 1 
		qui cap drop SDATE // ignore gaps 
		qui bys weo: gen SDATE = cond(notmiss, sum(notmiss), .)
		xtset weo SDATE
	}
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vol_ma60} ${L_vix} /// ${L_vlm_ma60_adj} 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_3_`tt'_`cc'_noVlm.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = ///
			b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = ///
			b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = ///
			b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = ///
			b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)

	twoway (line up_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(-0.1(0.1)0.6, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_3_`tt'_`cc'_noVlm.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_3_`tt'_`cc'_noVlm.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop


********************************************************************************
* Restrict to balanced sample post 1992

* Figure 3.A – Panel Full Sample
* Figure 3.B – Panel – excl. GFC

xtset weo DATE
foreach tt in FULL exGFC {
foreach cc in AE_EM {
foreach DV in index {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

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

preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	
	* Restrict to balanced sample
	keep if tin(1jan1992,)
	
	if `"`tt'"'=="exGFC" {
		drop if tin(1sep2008, 1sep2009)
		qui xtset weo DATE
		qui cap drop notmiss
		qui gen notmiss = 1 
		qui cap drop SDATE // ignore gaps 
		qui bys weo: gen SDATE = cond(notmiss, sum(notmiss), .)
		xtset weo SDATE
	}
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// weo_* 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// weo_* 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_3_`tt'_`cc'_balanced.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = ///
			b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = ///
			b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = ///
			b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = ///
			b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)

	twoway (line up_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(-0.1(0.1)0.6, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_3_`tt'_`cc'_balanced.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_3_`tt'_`cc'_balanced.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop

* Figure 3.C – Advanced Economies
* Figure 3.D – Emerging Markets

xtset weo DATE
foreach tt in FULL {
foreach cc in AE EM {
foreach DV in index {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

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
	
preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	
	* Restrict to balanced sample
	keep if tin(1jan1992,)
	
	if `"`tt'"'=="exGFC" {
		drop if tin(1sep2008, 1sep2009)
		qui xtset weo DATE
		qui cap drop notmiss
		qui gen notmiss = 1 
		qui cap drop SDATE // ignore gaps 
		qui bys weo: gen SDATE = cond(notmiss, sum(notmiss), .)
		xtset weo SDATE
	}
	
	* Variables to store impulse response and SE
	cap drop b_LP* se_LP*
	cap drop res*
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizon
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_3_`tt'_`cc'_balanced.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = ///
			b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = ///
			b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = ///
			b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = ///
			b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)

	twoway (line up_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(blue) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(blue) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity returns (%)", size(large)) ///
			ylabel(-0.1(0.1)0.6, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_3_`tt'_`cc'_balanced.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_3_`tt'_`cc'_balanced.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop


cap log close
