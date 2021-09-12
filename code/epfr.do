cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\epfr_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** EPFR EQUITY FLOWS ***
********************************************************************************

*** COUNTRY FLOWS DECOMPOSED INTO SUBGROUPS

global iso2em "AR BR CL CN ID IN KR MX MY PE PH PL RU TH TR ZA"
global iso2ae "DE ES FR GR IE IT JP PT US"

foreach tp in equity { 
foreach tt in etf active local foreign {
	
	loc tn = subinstr("`tt'", "currency", "cur", .)
	loc tn = subinstr("`tn'", "institutional", "inst", .)

foreach cc in $iso2em $iso2ae {
	
	clear
	cap noi import delimited using ///
		"..\data\epfr\EPFR_`tp'_`tt'_`cc'.txt" ///
		, delimiter(tab) clear
	
	qui count
	if r(N) > 0 {
		
		split reportdate, gen(date) parse(" ")
		
		gen DATE = date(date1, "MDY", 2000)
		format DATE %td
		
		keep DATE flow flowtotal flowpct assetsend flowtotalpct
		order DATE flow flowtotal flowpct assetsend flowtotalpct
		
		ds DATE, not
		foreach vv in `r(varlist)' {
			ren `vv' `tp'_`tn'_`vv'
		}
		
		gen iso2 = "`cc'"
		compress
		save "..\data\epfr\EPFR_`tp'_`tn'_`cc'.dta", replace
	}
}

loc cnt = 1
foreach cc in $iso2em $iso2ae {
	
	if `cnt++'==1 cap use "..\data\epfr\EPFR_`tp'_`tn'_`cc'.dta", clear
	else if `cnt++' > 1 cap append using "..\data\epfr\EPFR_`tp'_`tn'_`cc'.dta"
}

qui count
if r(N) > 0 {
	
	kountry iso2, from(iso2c) to(imfn)
		ren _IMFN_ weo
	
	order weo iso2 DATE
	duplicates drop
	
	xtset weo DATE
	compress
	save "..\data\epfr\EPFR_`tp'_`tn'.dta", replace
}
}
}

foreach tp in equity {
loc cnt = 1
foreach tt in etf active local foreign {
	
	loc tn = subinstr("`tt'", "currency", "cur", .)
	loc tn = subinstr("`tn'", "institutional", "inst", .)
	
	if `cnt'==1 use "..\data\epfr\EPFR_`tp'_`tn'.dta", clear
	else cap noi merge 1:1 weo DATE using "..\data\epfr\EPFR_`tp'_`tn'.dta", nogen
	loc `cnt++'
}
compress
save "..\data\epfr\EPFR_`tp'_subgroups.dta", replace
}

foreach tp in equity {

use "..\data\epfr\EPFR_`tp'_subgroups.dta", clear

foreach tt in etf active local foreign {

preserve

cap noi keep weo DATE *`tt'*

if _rc==0 {
	
	cap drop iso2
	xtset weo DATE
	
	loc outlier_dummies
	ds weo DATE, not
	
	foreach vv in `r(varlist)' {
		
		bysort weo: egen m_`vv' = mean(`vv')
		bysort weo: egen sd_`vv' = sd(`vv')
		
		gen `vv'_n = (`vv' - m_`vv') / sd_`vv'
		
		gen ub_`vv' = m_`vv' + 6 * sd_`vv'
		gen lb_`vv' = m_`vv' - 6 * sd_`vv'
		
		gen d_`vv'_h = 0
		gen d_`vv'_l = 0
		replace d_`vv'_h = 1 if `vv' > ub_`vv' & !mi(`vv')
		replace d_`vv'_l = 1 if `vv' < lb_`vv' & !mi(`vv')
		replace d_`vv'_h = . if mi(`vv')
		replace d_`vv'_l = . if mi(`vv')
			tab d_`vv'_h d_`vv'_l, m
		
		loc outlier_dummies `outlier_dummies' d_`vv'_h d_`vv'_l
		cap drop m_`vv' sd_`vv' `vv'_n ub_`vv' lb_`vv'
	}

	* Cumulative subsequent returns to go on LHS
	cap drop F*_`tp'_flow*
	qui xtset weo DATE
	qui cap drop notmiss
	qui gen notmiss = `tp'_`tt'_flow < .
	qui cap drop SDATE // ignore weekends when calculating returns
	qui gen SDATE = cond(notmiss, sum(notmiss), .)
	xtset weo SDATE
	
	foreach vv in `tp'_`tt'_flow {
	forval h = 0(1)$horizonFlows {
		
		xtset weo SDATE
		loc cumsum
		forval f = 0(1)`h' {
			if `f'==0 loc cumsum `vv'
			else loc cumsum `cumsum' + F`f'.`vv'
		}
		
		gen F`h'_cum_`vv' = ((`cumsum') / L.`tp'_`tt'_assetsend) * 100
		lab var F`h'_cum_`vv' "`h'-day subsequent cumulative returns of flow (%)"
		
		bysort weo: egen m_`vv' = mean(F`h'_cum_`vv')
		bysort weo: egen sd_`vv' = sd(F`h'_cum_`vv')
		
		gen `vv'_n = (`vv' - m_`vv') / sd_`vv'
		
		gen ub_F`h'_cum_`vv' = m_`vv' + 6 * sd_`vv'
		gen lb_F`h'_cum_`vv' = m_`vv' - 6 * sd_`vv'
		
		gen byte d_F`h'_cum_`vv'_h = 0
		gen byte d_F`h'_cum_`vv'_l = 0
		
		replace d_F`h'_cum_`vv'_h = 1 ///
			if `vv' > ub_F`h'_cum_`vv' & !mi(F`h'_cum_`vv')
		replace d_F`h'_cum_`vv'_l = 1 ///
			if `vv' < lb_F`h'_cum_`vv' & !mi(F`h'_cum_`vv')
		replace d_F`h'_cum_`vv'_h = . if mi(F`h'_cum_`vv')
		replace d_F`h'_cum_`vv'_l = . if mi(F`h'_cum_`vv')
		
		cap drop m_`vv' sd_`vv' `vv'_n ub_F`h'_cum_`vv' lb_F`h'_cum_`vv'
		compress
	}
	}
	foreach vv in `tp'_`tt'_flow `tp'_`tt'_flowtotal `tp'_`tt'_flowpct ///
		`tp'_`tt'_assetsend `tp'_`tt'_flowtotalpct {
		
		xtset weo SDATE
		di "CREATING LAGS FOR: `vv'"
		
		keep if !mi(`vv')
		forval l=1(1)$Nlags {
			gen L`l'_`vv' = `vv'[_n-`l']
		}
		order L*_`vv', after(`vv')
		
		forval l=1(1)$Nlags {
			cap lab var L`l'_`vv' ///
			"`l'-day lag of `vv', latest value available ignoring missing values"
		}
		compress
	}

	qui xtset weo DATE
	qui cap drop notmiss SDATE

	tsfill
	d
	compress
	save "..\data\epfr\EPFR_`tp'_`tt'_cum.dta", replace
}
restore

}
}

*** EPFR EQUITY COUNTRY FLOWS

global iso2 "AR BR CL CN DE ES FR GR ID IE IN IT JP KR MX MY PE PH PL PT RU TH TR US ZA"
foreach tp in equity { 
foreach cc in $iso2 {
	
	import delimited using ///
		"..\data\epfr\EPFR_`tp'_`cc'.txt" ///
		, delimiter(tab) clear
	
	split reportdate, gen(date) parse(" ")
	gen DATE = date(date1, "MDY", 2000)
		format DATE %td
	
	keep DATE flow flowtotal flowpct assetsend flowtotalpct
	order DATE flow flowtotal flowpct assetsend flowtotalpct
	
	ds DATE, not
	foreach vv in `r(varlist)' {
		ren `vv' `tp'_`vv'
	}
	
	gen iso2 = "`cc'"
	compress
	save "..\data\epfr\EPFR_`tp'_`cc'.dta", replace
}

loc cnt = 1
foreach cc in $iso2 {
	if `cnt++'==1 use "..\data\epfr\EPFR_`tp'_`cc'.dta", clear
	else if `cnt++' > 1 append using "..\data\epfr\EPFR_`tp'_`cc'.dta"
}

kountry iso2, from(iso2c) to(imfn)
	ren _IMFN_ weo

order weo iso2 DATE
xtset weo DATE
compress
save "..\data\epfr\EPFR_`tp'.dta", replace

use "..\data\epfr\EPFR_`tp'.dta", clear

ds weo iso2 DATE, not 
global epfr_`tp' = "`r(varlist)'"

cap drop iso2
xtset weo DATE

loc outlier_dummies
ds weo DATE, not 
foreach vv in `r(varlist)' {
	
	bysort weo: egen mean_`vv'=mean(`vv')
	bysort weo: egen sd_`vv'=sd(`vv')
	
	gen `vv'_norm=(`vv'-mean_`vv')/sd_`vv'
	gen ub_`vv' = mean_`vv' + 6*sd_`vv'
	gen lb_`vv' = mean_`vv' - 6*sd_`vv'
	
	gen d_`vv'_h = 0
	gen d_`vv'_l = 0
	
	replace d_`vv'_h = 1 if `vv' > ub_`vv' & !mi(`vv')
	replace d_`vv'_l = 1 if `vv' < lb_`vv' & !mi(`vv')
	replace d_`vv'_h = . if mi(`vv')
	replace d_`vv'_l = . if mi(`vv')
		tab d_`vv'_h d_`vv'_l, m
	
	loc outlier_dummies `outlier_dummies' d_`vv'_h d_`vv'_l
	cap drop mean_`vv' sd_`vv' `vv'_norm ub_`vv' lb_`vv'
}

* Cumulative subsequent returns to go on LHS
cap drop F*_`tp'_flow* 
qui xtset weo DATE
qui cap drop notmiss
qui gen notmiss = `tp'_flow < . 
qui cap drop SDATE // ignore weekends when calculating returns
qui gen SDATE = cond(notmiss, sum(notmiss), .)
xtset weo SDATE

foreach vv in `tp'_flow { 
forval h = 0(1)$horizonFlows {
	
	xtset weo SDATE
	
	loc cumsum
	forval f = 0(1)`h' {
		if `f'==0 loc cumsum `vv'
		else loc cumsum `cumsum' + F`f'.`vv'
	}
	
	di "gen F`h'_cum_`vv' = ((`cumsum') / L.`tp'_assetsend) * 100"
	gen F`h'_cum_`vv' = ((`cumsum') / L.`tp'_assetsend) * 100
	lab var F`h'_cum_`vv' "`h'-day subsequent cumulative returns of flow (%)"
	
	bysort weo: egen mean_`vv' = mean(F`h'_cum_`vv')
	bysort weo: egen sd_`vv' = sd(F`h'_cum_`vv')
	
	gen `vv'_norm=(`vv' - mean_`vv')/sd_`vv'
	
	gen ub_F`h'_cum_`vv' = mean_`vv' + 6 * sd_`vv'
	gen lb_F`h'_cum_`vv' = mean_`vv' - 6 * sd_`vv'
	
	gen byte d_F`h'_cum_`vv'_h = 0
	gen byte d_F`h'_cum_`vv'_l = 0
	
	replace d_F`h'_cum_`vv'_h = 1 ///
		if `vv' > ub_F`h'_cum_`vv' & !mi(F`h'_cum_`vv')
	replace d_F`h'_cum_`vv'_l = 1 ///
		if `vv' < lb_F`h'_cum_`vv' & !mi(F`h'_cum_`vv')
	replace d_F`h'_cum_`vv'_h = . if mi(F`h'_cum_`vv')
	replace d_F`h'_cum_`vv'_l = . if mi(F`h'_cum_`vv')
	
	cap drop mean_`vv' sd_`vv' `vv'_norm ub_F`h'_cum_`vv' lb_F`h'_cum_`vv'
	compress
}
}
foreach vv in `tp'_flow `tp'_flowtotal `tp'_flowpct ///
		`tp'_assetsend `tp'_flowtotalpct {
	
	xtset weo SDATE
	di "CREATING LAGS FOR: `vv'"
	
	keep if !mi(`vv')
	forval l=1(1)$Nlags {
		gen L`l'_`vv' = `vv'[_n-`l']
	}
	order L*_`vv', after(`vv')
	
	forval l=1(1)$Nlags {
		cap lab var L`l'_`vv' ///
		"`l'-day lag of `vv', latest value available ignoring missing values"
	}
	compress
}

xtset weo DATE
cap drop notmiss SDATE
tsfill
d
compress
save "..\data\epfr\EPFR_`tp'_cum.dta", replace
}

cap log close
