cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\table_2_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Table 2 – Global News Sentiment and Economic Surprises ***
********************************************************************************

* produced by dfm.do
use "..\data\factor_sent1_best.dta", clear

merge 1:1 DATE using "..\data\VIXCLS.dta", nogen
tsset DATE

lab var factor_sent1_best_filter "Global News Sentiment Index"
lab var vix "VIX"

merge m:1 DATE using "..\data\EPU_All_Daily_Policy_Data.dta" ///
	, nogen keep(1 3) keepusing(epu ln_epu)
merge m:1 DATE using "..\data\CESI.dta" ///
	, nogen keep(1 3) keepusing(cesi_usd cesi_eur cesi_cny cesi_g10)	

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

* Table 2 – Global News Sentiment and Economic Surprises
* regress global news sentiment on the three CESI, 
* both in level and first difference (CESI)

label var cesi_usd "$ CESI\\_USD$"
label var cesi_eur "$ CESI\\_EUR$"
label var cesi_cny "$ CESI\\_CNY$"
label var cesi_g10 "$ CESI\\_G10$"

tsset DATE
eststo clear

eststo: reg factor_sent1_best_filter cesi_usd, vce(robust)
	estimates store est1
eststo: reg factor_sent1_best_filter cesi_eur, vce(robust)
	estimates store est2
eststo: reg factor_sent1_best_filter cesi_cny, vce(robust)
	estimates store est3
eststo: reg factor_sent1_best_filter cesi_g10, vce(robust)
	estimates store est4
eststo: reg factor_sent1_best_filter cesi_usd ///
		cesi_eur cesi_cny cesi_g10, vce(robust)
	estimates store est5

esttab est1 est2 est3 est4 est5 using ///
	"..\results\table_2.tex", booktabs replace fragment ///
	nocon nonotes label nomtitles b(%9.3f) se(%9.3f) /// nostar nonum 
	stats(N r2, fmt(%9.0fc %9.2fc) ///
	labels("$ N$" "$ R^2$")) substitute(\_ _)

cap log close
