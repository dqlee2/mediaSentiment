cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_a11_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure A11. Global Sentiment Shocks and Currency Returns ***
********************************************************************************

use "..\data\regsample.dta", clear

merge 1:1 weo DATE using "..\data\fx_bis.dta", nogen keep(1 3)

******** Dummy for outliers **************************

cap drop mean_R_fx
bysort weo: egen mean_R_fx = mean(R_fx)
cap drop sd_R_fx
bysort weo: egen sd_R_fx = sd(R_fx)

gen R_fx_norm = (R_fx - mean_R_fx) / sd_R_fx

gen band_ub_R_fx = mean_R_fx + 6 * sd_R_fx
gen band_lb_R_fx = mean_R_fx - 6 * sd_R_fx

cap drop d_R_fx
gen d_R_fx_h = 0
gen d_R_fx_l = 0
replace d_R_fx_h = 1 if R_fx > band_ub_R_fx & !mi(R_fx)
replace d_R_fx_l = 1 if R_fx < band_lb_R_fx & !mi(R_fx)
replace d_R_fx_h = . if mi(R_fx)
replace d_R_fx_l = . if mi(R_fx)
	tab d_R_fx_h d_R_fx_l, m

* Cumulative subsequent returns to go on LHS

cap drop F*_fx*
qui xtset weo DATE
qui cap drop notmiss
qui gen notmiss = fx < .
qui cap drop SDATE // ignore weekends when calculating returns
qui gen SDATE = cond(notmiss, sum(notmiss), .)
xtset weo SDATE

forval h = 0(1)$horizon {
	
	gen F`h'_fx = ((F`h'.fx / L.fx) - 1) * 100
	replace F`h'_fx = . if F`h'_fx==0 // these are holidays during the week
	lab var F`h'_fx "`h'-day subsequent cumulative returns of fx (%)"
	
	bysort weo: egen _mean = mean(F`h'_fx)
	bysort weo: egen _sd = sd(F`h'_fx)
	
	gen F`h'_fx_norm = (F`h'_fx - _mean) / _sd
	cap drop _mean _sd
}

qui xtset weo DATE
qui cap drop notmiss SDATE

* Forwards to go on LHS, marginal version

ds d_R_fx_h d_R_fx_l 
loc LP_LHS = "`r(varlist)'"

foreach vv in `LP_LHS' {

preserve

	cap drop F*_*
	keep weo DATE `vv'
	xtset weo DATE
	keep if !mi(`vv')
	
	* To be used for each horizon of LP regression
	forval h = 0(1)$horizon {
		gen F`h'_`vv' = `vv'[_n+`h']
	}
	order F*_`vv', after(`vv')
	
	keep weo DATE F*_*
	compress
	tempfile tmp_F_`vv'
	save `tmp_F_`vv''
	
restore

}

foreach vv in `LP_LHS' {
	
	di "merge 1:1 weo DATE using `tmp_F_`vv'', nogen"
	merge 1:1 weo DATE using `tmp_F_`vv'', nogen
	
	gen L0_`vv' = `vv'
}

xtset weo DATE

* Shorthand for row vectors

ds fx R_fx ///
	returns vlm_ma* vol_ma* ///
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

* Dummy for reserve currencies

gen byte reserve = 0
replace reserve = 1 if ccy=="USD"
replace reserve = 1 if ccy=="EUR"
replace reserve = 1 if ccy=="JPY"
replace reserve = 1 if ccy=="GBP"
	tab ccy reserve, m
	
gen byte reserve0 = (reserve==0)
gen byte reserve1 = (reserve==1)

********************************************************************************

* Median absolute deviation of currency returns
* on the regression sample at first horizon

reghdfe F0_fx ${L_t_sent1_best_local} ${L_factor_sent1_best_filter} ///
	${L_R_fx} F0_d_R_fx_h F0_d_R_fx_l ///
	${L_returns} F0_d_returns_h F0_d_returns_l ///
	${L_vlm_ma60_adj} ${L_vol_ma60} ${L_vix} ///
	${L_t_art1_best_local} ${L_commo} ${L_R_DJ_global} ///
	${other_dummies} ///
	, absorb(weo) 
su F0_fx if e(sample), d
di "Mean absolute deviation of currency returns: `r(mean)'"

********************************************************************************

* Figure A11. Global Sentiment Shocks and Currency Returns

* Figure A11.A – Panel Full Sample
* Figure A11.B – Panel – excl. GFC

xtset weo DATE
foreach tt in FULL exGFC {
foreach cc in AE_EM { 
foreach DV in fx {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {
foreach DFM in factor_sent`s'_best_filter { // 

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="exGFC" {
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
	
	if `"`cc'"'=="reserve0" {
		loc cnd_cc = "& reserve0==1"
	}
	if `"`cc'"'=="reserve1" {
		loc cnd_cc = "& reserve1==1"
	}

preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_R_fx} F*_d_R_fx_h F*_d_R_fx_l ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vol_ma60_R_fx} ${L_vix} ${L_vlm_ma60_adj} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} reserve0 reserve1
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
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_R_fx} F`i'_d_R_fx_h F`i'_d_R_fx_l ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60_R_fx} ${L_vix} /// 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_R_fx} F`i'_d_R_fx_h F`i'_d_R_fx_l ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60_R_fx} ${L_vix} /// 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			cap drop res`i'
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_a11_`tt'_`cc'.csv" ///
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
		gen up_LP`i' = b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
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
			ytitle("Cumulative currency returns (%)", size(medsmall)) ///
			xtitle("Horizon (h Days)", size(medsmall)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_a11_`tt'_`cc'.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_a11_`tt'_`cc'.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop

* Figure A11.C – Reserve currencies
* Figure A11.D – Emerging markets

xtset weo DATE
foreach tt in FULL {
foreach cc in reserve1 reserve0 { 
foreach DV in fx {
foreach s in 1 {
foreach impulse in t_sent`s'_best_local {

	xtset weo DATE
	
	if "`impulse'"=="t_sent`s'_best_local" loc art_cntrl t_art`s'_best_local
	
	if `"`tt'"'=="FULL" {
		loc cnd_tt = ""
	}
	if `"`tt'"'=="exGFC" {
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
	
	if `"`cc'"'=="reserve0" {
		loc cnd_cc = "& reserve0==1"
	}
	if `"`cc'"'=="reserve1" {
		loc cnd_cc = "& reserve1==1"
	}

*** WITH GLOBAL SENTIMENT
foreach DFM in factor_sent`s'_best_filter { // 

preserve
	
	keep weo* iso DATE AE F*_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_R_fx} F*_d_R_fx_h F*_d_R_fx_l ///
				${L_returns} F*_d_returns_h F*_d_returns_l ///
				${L_vol_ma60_R_fx} ${L_vix} ${L_vlm_ma60_adj} ///
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} reserve0 reserve1
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
	forval i = 1(1)4 {
		gen b_LP`i' = 0
		gen se_LP`i' = 0
	}
	
	* One regression for each horizon of the response
	eststo clear
	forvalues i=0/$horizon {
		
		* LP regression
		if `i' > 0 {
			loc j = `i' - 1
			xtscc F`i'_`DV' ${L_`impulse'} res`j' ${L_`DFM'} ///
				${L_R_fx} F`i'_d_R_fx_h F`i'_d_R_fx_l ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60_R_fx} ${L_vix} /// 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		else if `i'==0 {
			xtscc F`i'_`DV' ${L_`impulse'} ${L_`DFM'} ///
				${L_R_fx} F`i'_d_R_fx_h F`i'_d_R_fx_l ///
				${L_returns} F`i'_d_returns_h F`i'_d_returns_l ///
				${L_vlm_ma60_adj} ${L_vol_ma60_R_fx} ${L_vix} /// 
				${L_`art_cntrl'} ${L_commo} ${L_R_DJ_global} ///
				${other_dummies} /// 
				if !mi(weo) `cnd_cc' `cnd_tt' ///
					, lag(`i') fe pooled
			eststo ols_LP`i'
		}
		predict infhat
			cap drop res`i'
			gen res`i' = F`i'_`DV' - infhat
			drop infhat 
		
		* Store coefficient and se on first lag
		replace b_LP1 = _b[L1_`impulse'] if _n==`i'+2
		replace se_LP1 = _se[L1_`impulse'] if _n==`i'+2
		replace b_LP2 = _b[L1_`DFM'] if _n==`i'+2
		replace se_LP2 = _se[L1_`DFM'] if _n==`i'+2
	}
	esttab * using ///
		"..\results\figure_a11_`tt'_`cc'.csv" ///
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
		gen up_LP`i' = b_LP`i' + invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn_LP`i' = b_LP`i' - invnormal(1-sig1/2)*se_LP`i' if _n <= ($horizon + 1)
		gen up2_LP`i' = b_LP`i' + invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
		gen dn2_LP`i' = b_LP`i' - invnormal(1-sig2/2)*se_LP`i' if _n <= ($horizon + 1)
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
			ytitle("Cumulative currency returns (%)", size(medsmall)) ///
			xtitle("Horizon (h Days)", size(medsmall)) ///
			graphregion(color(white)) plotregion(color(white)) 
	graph export ///
		"..\results\figure_a11_`tt'_`cc'.pdf" ///
		, as(pdf) replace 
	cap graph close

	save "..\results\figure_a11_`tt'_`cc'.dta", replace
restore
	
} // end DFM loop
} // end varname loop
} // end sent type loop
} // end DV loop
} // end cc loop
} // end tt loop

preserve

* Graph formatting
use "..\results\figure_a11_FULL_AE_EM.dta", clear
twoway (line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative currency returns (%)", size(medium)) ///
		xtitle("Horizon (h Days)", size(medium)) ///
		ylabel(-0.3(0.05)0, labsize(medium)) xlabel(, labsize(medium)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_a11_FULL_AE_EM.pdf", as(pdf) replace 
cap graph close

use "..\results\figure_a11_exGFC_AE_EM.dta", clear
twoway (line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative currency returns (%)", size(medium)) ///
		xtitle("Horizon (h Days)", size(medium)) ///
		ylabel(-0.3(0.05)0, labsize(medium)) xlabel(, labsize(medium)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_a11_exGFC_AE_EM.pdf", as(pdf) replace 
cap graph close

use "..\results\figure_a11_FULL_reserve1.dta", clear
twoway (line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative currency returns (%)", size(medium)) ///
		xtitle("Horizon (h Days)", size(medium)) ///
		ylabel(-0.3(0.05)0, labsize(medium)) xlabel(, labsize(medium)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_a11_FULL_reserve1.pdf", as(pdf) replace 
cap graph close

use "..\results\figure_a11_FULL_reserve0.dta", clear
twoway (line up_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line dn_LP2 t, lcolor(green) lpattern(solid) lwidth(medium)) /// 
		(line b_LP2 t, lcolor(green) lpattern(solid) lwidth(thick)) ///
		(line zero t, lcolor(black) lpattern(solid) lwidth(medium)) ///
		, legend(off) title("") ///
		ytitle("Cumulative currency returns (%)", size(medium)) ///
		xtitle("Horizon (h Days)", size(medium)) ///
		ylabel(-0.3(0.05)0, labsize(medium)) xlabel(, labsize(medium)) ///
		graphregion(color(white)) plotregion(color(white)) 
graph export "..\results\figure_a11_FULL_reserve0.pdf", as(pdf) replace 
cap graph close

restore


cap log close
