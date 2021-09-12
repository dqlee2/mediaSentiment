cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_4_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 4. Global vs. Local Sentiment Shocks - Equity Flows
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

reghdfe F0_cum_equity_flow ///
	${L_equity_flowpct} d_F0_cum_equity_flow_h d_F0_cum_equity_flow_l ///
	${L_t_sent1_best_local} ${L_factor_sent1_best_filter} ///
	${L_returns} F0_d_returns_h F0_d_returns_l ///
	${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
	${L_t_art1_best_local} ${L_commo} ${L_R_DJ_global} ///
	${other_dummies} ///
	if AE==0 & tin(1dec2008,), absorb(weo) 
su F0_cum_equity_flow if e(sample), d
di "Mean absolute deviation of equity flows: `r(mean)'"

********************************************************************************

* Figure 4.A – All Funds
* Figure 4.B – All Funds – Post 2010

xtset weo DATE
foreach tt in dec2008 jan2010 {
foreach cc in EM {
foreach tp in equity {
foreach DV in `tp'_flow {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="jan2010" {
		loc cnd_tt = "& tin(1jan2010, )"
	}
	if `"`tt'"'=="dec2008" {
		loc cnd_tt = "& tin(1dec2008, )"
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
				${L_`tp'_flowpct} d_F*_cum_`DV'_h d_F*_cum_`DV'_l ///
				${L_`prices'} F*_d_returns_h F*_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	xtset weo DATE 
	
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
	
	* LP: One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizonFlows
	forvalues i=0/$horizonFlows {
		loc k = `i' + 1
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_cum_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_`tp'_flowpct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies} ///
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled 
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_cum_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_`tp'_flowpct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
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
		"..\results\figure_4_`DV'_`tt'.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen dn_LP`i' = b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen up2_LP`i' = b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen dn2_LP`i' = b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizonFlows + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)
	
	twoway (line up_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
			ylabel(, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_4_`DV'_`tt'.pdf" ///
		, as(pdf) replace 
	cap graph close 

	save "..\results\figure_4_`DV'_`tt'.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end equity bond loop
} // end cc loop
} // end tt loop

* Figure 4.C – Foreign Investors
* Figure 4.D – Local Investors

xtset weo DATE
foreach tt in dec2008 jan2010 { // 
foreach cc in EM {
foreach tp in equity_fgn equity_loc { // 
foreach DV in `tp'_flow {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="jan2010" {
		loc cnd_tt = "& tin(1jan2010, )"
	}
	if `"`tt'"'=="dec2008" {
		loc cnd_tt = "& tin(1dec2008, )"
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
				${L_`prices'} F*_d_returns_h F*_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
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
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
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
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
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
		"..\results\figure_4_`DV'_`tt'.csv" ///
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
		"..\results\figure_4_`DV'_`tt'.pdf" ///
		, as(pdf) replace 
	cap graph close 

	save "..\results\figure_4_`DV'_`tt'.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end equity bond loop
} // end cc loop
} // end tt loop

preserve

* Graph formatting
use "..\results\figure_4_equity_flow_dec2008.dta", clear
twoway (line up_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line dn_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
		(line up_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line dn_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(0.00(0.05)0.25, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_4_equity_flow_dec2008.pdf", as(pdf) replace 
cap graph close

use "..\results\figure_4_equity_flow_jan2010.dta", clear
twoway (line up_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line dn_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
		(line up_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line dn_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(0.00(0.05)0.25, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_4_equity_flow_jan2010.pdf", as(pdf) replace 
cap graph close

use "..\results\figure_4_equity_fgn_flow_dec2008.dta", clear
twoway (line up1_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line dn1_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
		(line up1_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line dn1_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(0.00(0.05)0.25, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_4_equity_fgn_flow_dec2008.pdf", as(pdf) replace 
cap graph close

use "..\results\figure_4_equity_loc_flow_dec2008.dta", clear
twoway (line up1_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line dn1_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
		(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
		(line up1_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line dn1_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
		ylabel(-0.4(0.2)0.5, labsize(large)) ///
		xtitle("Horizon (h Days)", size(large)) ///
		xlabel(, labsize(large)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_4_equity_loc_flow_dec2008.pdf", as(pdf) replace 
cap graph close

restore

********************************************************************************
*** Robustness test: Not controlling for volume

* Figure 4.A – All Funds
* Figure 4.B – All Funds – Post 2010

xtset weo DATE
foreach tt in dec2008 jan2010 {
foreach cc in EM {
foreach tp in equity {
foreach DV in `tp'_flow {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="jan2010" {
		loc cnd_tt = "& tin(1jan2010, )"
	}
	if `"`tt'"'=="dec2008" {
		loc cnd_tt = "& tin(1dec2008, )"
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
				${L_`tp'_flowpct} d_F*_cum_`DV'_h d_F*_cum_`DV'_l ///
				${L_`prices'} F*_d_returns_h F*_d_returns_l ///
				${L_`vol'} ${L_`art_cntrl'} /// ${L_`vlm'} 
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	xtset weo DATE 
	
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
	
	* LP: One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizonFlows
	forvalues i=0/$horizonFlows {
		loc k = `i' + 1
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_cum_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_`tp'_flowpct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vol'} ${L_`art_cntrl'} /// ${L_`vlm'} 
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies} ///
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled 
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_cum_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_`tp'_flowpct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vol'} ${L_`art_cntrl'} /// ${L_`vlm'} 
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
		"..\results\figure_4_`DV'_`tt'_noVlm.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen dn_LP`i' = b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen up2_LP`i' = b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen dn2_LP`i' = b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizonFlows + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)
	
	twoway (line up_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
			ylabel(, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_4_`DV'_`tt'_noVlm.pdf" ///
		, as(pdf) replace 
	cap graph close 

	save "..\results\figure_4_`DV'_`tt'_noVlm.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end equity bond loop
} // end cc loop
} // end tt loop

* Figure 4.C – Foreign Investors
* Figure 4.D – Local Investors

xtset weo DATE
foreach tt in dec2008 jan2010 { // 
foreach cc in EM {
foreach tp in equity_fgn equity_loc { // 
foreach DV in `tp'_flow {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="jan2010" {
		loc cnd_tt = "& tin(1jan2010, )"
	}
	if `"`tt'"'=="dec2008" {
		loc cnd_tt = "& tin(1dec2008, )"
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
				${L_`prices'} F*_d_returns_h F*_d_returns_l ///
				${L_`vol'} ${L_`art_cntrl'} /// ${L_`vlm'} 
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
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
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vol'} ${L_`art_cntrl'} /// ${L_`vlm'} 
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled 
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_cum_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_`DV'pct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vol'} ${L_`art_cntrl'} /// ${L_`vlm'} 
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
		"..\results\figure_4_`DV'_`tt'_noVlm.csv" ///
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
		"..\results\figure_4_`DV'_`tt'_noVlm.pdf" ///
		, as(pdf) replace 
	cap graph close 

	save "..\results\figure_4_`DV'_`tt'_noVlm.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end equity bond loop
} // end cc loop
} // end tt loop


********************************************************************************
* Restrict to balanced sample post 1992

* Figure 4.A – All Funds
* Figure 4.B – All Funds – Post 2010

xtset weo DATE
foreach tt in dec2008 jan2010 {
foreach cc in EM {
foreach tp in equity {
foreach DV in `tp'_flow {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="jan2010" {
		loc cnd_tt = "& tin(1jan2010, )"
	}
	if `"`tt'"'=="dec2008" {
		loc cnd_tt = "& tin(1dec2008, )"
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
				${L_`tp'_flowpct} d_F*_cum_`DV'_h d_F*_cum_`DV'_l ///
				${L_`prices'} F*_d_returns_h F*_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies}
	keep if !mi(weo) `cnd_cc' `cnd_tt'
	xtset weo DATE 
	
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
	
	* LP: One regression for each horizon of the response
	eststo clear
	cap drop infhat 
	cap drop res0-res$horizonFlows
	forvalues i=0/$horizonFlows {
		loc k = `i' + 1
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_cum_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_`tp'_flowpct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
				${other_dummies} ///
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled 
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_cum_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_`tp'_flowpct} d_F`i'_cum_`DV'_h d_F`i'_cum_`DV'_l ///
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
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
		"..\results\figure_4_`DV'_`tt'_balanced.csv" ///
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
	foreach i in 1 2 { // 
		gen up_LP`i' = b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen dn_LP`i' = b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen up2_LP`i' = b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizonFlows + 1)
		gen dn2_LP`i' = b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizonFlows + 1)
	}
	keep t zero up*_LP* dn*_LP* b*_LP* se*_LP*
	ds up*_LP* dn*_LP* b*_LP* se*_LP*
	drop if mi(t)
	
	twoway (line up_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line dn_LP1 t, lcolor(navy) lpattern(dash) lwidth(medium)) /// 
			(line b_LP1 t, lcolor(navy) lpattern(dash) lwidth(thick)) ///
			(line up_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line dn_LP2 t, lcolor(maroon) lpattern(solid) lwidth(medium)) /// 
			(line b_LP2 t, lcolor(maroon) lpattern(solid) lwidth(thick)) ///
			(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
			, legend(off) title("") ///
			ytitle("Cumulative equity (fund) flows (%)", size(large)) ///
			ylabel(, labsize(large)) ///
			xtitle("Horizon (h Days)", size(large)) ///
			xlabel(, labsize(large)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_4_`DV'_`tt'_balanced.pdf" ///
		, as(pdf) replace 
	cap graph close 

	save "..\results\figure_4_`DV'_`tt'_balanced.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end equity bond loop
} // end cc loop
} // end tt loop

* Figure 4.C – Foreign Investors
* Figure 4.D – Local Investors

xtset weo DATE
foreach tt in dec2008 jan2010 { // 
foreach cc in EM {
foreach tp in equity_fgn equity_loc { // 
foreach DV in `tp'_flow {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter {

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="jan2010" {
		loc cnd_tt = "& tin(1jan2010, )"
	}
	if `"`tt'"'=="dec2008" {
		loc cnd_tt = "& tin(1dec2008, )"
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
				${L_`prices'} F*_d_returns_h F*_d_returns_l ///
				${L_`vlm'} ${L_`vol'} ${L_`art_cntrl'} ///
				${L_vix} ${L_commo} ${L_`glb_cntrl'} ${L_`rgn_cntrl'} ///
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
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
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
				${L_`prices'} F`i'_d_returns_h F`i'_d_returns_l ///
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
		"..\results\figure_4_`DV'_`tt'_balanced.csv" ///
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
		"..\results\figure_4_`DV'_`tt'_balanced.pdf" ///
		, as(pdf) replace 
	cap graph close 

	save "..\results\figure_4_`DV'_`tt'_balanced.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end equity bond loop
} // end cc loop
} // end tt loop


cap log close
