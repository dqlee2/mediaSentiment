cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\clean_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** MERGE ***
********************************************************************************

* Stock market index/returns and trading volume
use DATE weo iso index returns volume using "..\data\INDEX.dta", clear

* List of countries to keep (has news sentiment data)
merge 1:1 weo DATE using "..\data\news.dta", nogen keepusing(weo DATE) keep(1 3)
	lab var iso "ISO Alpha-2 country code (string)"
	tab iso, m
	encode iso, gen(iso2)

xtset weo DATE
tsfill

********************************************************************************
*** DATA CHECKS AND STYLIZED FACTS ***
********************************************************************************

******** Returns **************************

bysort weo: egen mean_returns = mean(returns)
bysort weo: egen sd_returns = sd(returns)
gen returns_norm = (returns - mean_returns) / sd_returns

******** Dummy for outliers in Returns **************************

gen band_ub_returns = mean_returns + 6 * sd_returns
gen band_lb_returns = mean_returns - 6 * sd_returns

gen d_returns_h = 0
gen d_returns_l = 0
replace d_returns_h = 1 if returns > band_ub_returns & !mi(returns)
replace d_returns_l = 1 if returns < band_lb_returns & !mi(returns)
replace d_returns_h = . if mi(returns)
replace d_returns_l = . if mi(returns)
	tab d_returns_h d_returns_l, m

******** Volatility **************************

cap drop residual2*
gen residual2 = (returns - mean_returns)^2
lab var residual2 "Squared residual of returns"

preserve 

keep weo iso DATE residual2
xtset weo DATE

tempfile tmp
save `tmp'

foreach mm in 60 {
	
	use `tmp', clear
	gen residual2_ma`mm' = .
	lab var residual2_ma`mm' "`mm' day moving average of squared residual"
	tempfile tmp`mm'
	save `tmp`mm''
	
	levelsof iso, local(clist)
	foreach cc in `clist' {
		
		use `tmp`mm'', clear
		keep if iso=="`cc'"
		
		di "tssmooth ma residual2_`cc' = residual2 if iso==`cc', window(`mm' 0)"
		tssmooth ma residual2_`cc' = residual2 if iso=="`cc'", window(`mm' 0)
		replace residual2_ma`mm' = residual2_`cc' if iso=="`cc'"
		cap drop residual2_`cc'
		
		tempfile tmp`cc'
		save `tmp`cc''
	}
	
	loc cnt = 0
	foreach cc in `clist' {
		
		if `cnt'==0 use `tmp`cc'', clear
		else if `cnt' > 0 append using `tmp`cc''
		loc ++cnt
	}
	
	gen vol_ma`mm' = residual2 - residual2_ma`mm'
	lab var vol_ma`mm' "Volatility, `mm' day window"
	
	compress
	tempfile tmp_vol_ma`mm'
	save `tmp_vol_ma`mm''
}
restore
foreach mm in 60 {
	merge 1:1 iso DATE using `tmp_vol_ma`mm'', nogen
}

******** Volumes **************************

gen ln_volume = ln(volume), after(volume)

bysort weo: egen mean_ln_volume = mean(ln_volume)
bysort weo: egen sd_ln_volume = sd(ln_volume)

gen ln_volume_norm = (ln_volume - mean_ln_volume) / sd_ln_volume

******** Dummy for breaks in Volumes **************************

cap drop volume_break
gen volume_break = 0 if !mi(volume)

replace volume_break = 1 if iso=="TH" & DATE==td(02dec1996)
replace volume_break = 1 if iso=="BR" & DATE==td(09jul1992)
replace volume_break = 1 if iso=="BR" & DATE==td(30dec1998)
lab var volume_break "Last trading day before break in log volume"

******** Detrended Log Volumes **************************

* Moving averages without adjusting for breaks

preserve

keep iso weo DATE ln_volume
xtset weo DATE

tempfile tmp
save `tmp'

foreach mm in 60 {
	
	use `tmp', clear
	cap drop ln_volume_ma`mm'*
	gen ln_volume_ma`mm' = ., after(ln_volume)
	
	qui levelsof iso, local(clist)
	tempfile tmp`mm'
	save `tmp`mm''
	
	foreach cc in `clist' {
		
		di "MA`mm' FOR COUNTRY: `cc'"
		use `tmp`mm'', clear
		keep if iso=="`cc'"
		
		qui levelsof DATE if !mi(ln_volume) & iso=="`cc'", local(date_)
		foreach ll in `date_' {
			
			qui su ln_volume ///
				if DATE<=`ll' - 1 & DATE>=`ll' - `mm' & iso=="`cc'", meanonly
			
			cap replace ln_volume_ma`mm' = `r(mean)' ///
				if DATE==`ll' & iso=="`cc'"
		}
		
		qui su DATE
		qui cap replace ln_volume_ma`mm' = . if DATE < r(min) + `mm'
		tempfile tmp`cc'
		save `tmp`cc''
	}
	
	loc cnt = 0
	foreach cc in `clist' {
		if `cnt'==0 use `tmp`cc'', clear
		else if `cnt' > 0 append using `tmp`cc''
		loc ++cnt
	}
	
	lab var ln_volume_ma`mm' "`mm' day rolling average of ln_volume"
	gen vlm_ma`mm' = ln_volume - ln_volume_ma`mm'
	lab var vlm_ma`mm' "Detended log volume, `mm' day window (unadjusted)"
	
	compress
	tempfile tmp_vlm_ma`mm'
	save `tmp_vlm_ma`mm''
}
restore
foreach mm in 60 {
	merge 1:1 iso DATE using `tmp_vlm_ma`mm'', nogen
}

* If there is a break at t: 
* use 30/60 day rolling forward looking moving average over [t,t+30]
xtset weo DATE
foreach mm in 60 {
	
	cap drop ln_volume_ma`mm'_adj*
	gen ln_volume_ma`mm'_adj = ., after(ln_volume)
	
	qui levelsof iso if volume_break==1, local(clist)
	foreach cc in `clist' {
		
		di "MA`mm' FOR COUNTRY: `cc'"
		qui levelsof DATE ///
			if !mi(ln_volume) & iso=="`cc'" & volume_break==1, local(date_)
		foreach ll in `date_' {
			
			loc tl = `ll' + 1
			loc tu = `ll' + `mm'
			
			forval i = `tl'(1)`tu' {
				qui su ln_volume ///
					if DATE>=`i' + 1 & DATE<=`i' + `mm' & iso=="`cc'", meanonly
				cap replace ln_volume_ma`mm'_adj = `r(mean)' ///
					if DATE==`i' & iso=="`cc'"
			}
		}

		qui su DATE
		qui cap replace ln_volume_ma`mm'_adj = . if DATE < r(min) + `mm'
	}
	
	lab var ln_volume_ma`mm'_adj ///
		"`mm' day rolling average of ln_volume (break adjustment)"
	replace ln_volume_ma`mm'_adj = ///
		ln_volume_ma`mm' if mi(ln_volume_ma`mm'_adj)
	
	gen vlm_ma`mm'_adj = ///
		ln_volume - ln_volume_ma`mm'_adj
	lab var vlm_ma`mm' ///
		"Detended log volume, `mm' day window (adjusted for breaks)"
}

xtset weo DATE
gen month = month(DATE)
gen year = year(DATE)

merge 1:1 weo DATE using "..\data\news.dta", nogen keep(1 3)
merge m:1 DATE using "..\data\factor_sent1_best.dta", nogen keep(1 3)
merge m:1 DATE using "..\data\factor_sent2_best.dta", nogen keep(1 3)
merge m:1 DATE using "..\data\factor_negativeTonal.dta", nogen keep(1 3)
merge m:1 DATE using "..\data\factor_negative.dta", nogen keep(1 3)
merge m:1 DATE using "..\data\factor_sentu1_best.dta", nogen keep(1 3)

bys weo (DATE): egen firstdate = min(cond(!mi(art0), DATE, .))
	format firstdate %tdnn/dd/CCYY
gen byte first = (DATE==firstdate)

merge m:1 DATE using "..\data\VIXCLS.dta" ///
	, nogen keep(1 3) keepusing(vix vix_w)
merge m:1 DATE using "..\data\Commodity_indexes.dta" ///
	, nogen keep(1 3) keepusing(commo)
merge m:1 DATE using "..\data\global_proxies.dta" ///
	, nogen keep(1 3) keepusing(R_MSCI_EM R_DJ_global)
merge m:1 DATE using "..\data\glb_bond_index.dta" ///
	, nogen keep(1 3) keepusing(R_glb_bond_index)
merge m:1 DATE using "..\data\EPU_All_Daily_Policy_Data.dta" ///
	, nogen keep(1 3) keepusing(epu ln_epu)
merge m:1 DATE using "..\data\CESI.dta" ///
	, nogen keep(1 3) keepusing(cesi_usd cesi_eur cesi_cny cesi_g10)	
	
* Merge epfr equity/bond flows
merge 1:1 weo DATE using "..\data\epfr\EPFR_equity_cum.dta" , keep(1 3) nogen

* Merge epfr flows sub groups
foreach tt in etf active local foreign {
	
	cap noi merge 1:1 weo DATE using ///
		"..\data\epfr\EPFR_equity_`tt'_cum.dta" ///
		, keep(1 3) nogen
}
foreach tp in equity {
	foreach tt in local foreign {
		
		cap drop start_L1_`tp'_`tt'_flowpct
		bys weo: egen start_L1_`tp'_`tt'_flowpct = ///
			min(cond(!mi(L1_`tp'_`tt'_flowpct), DATE, .))
		
		cap drop end_L1_`tp'_`tt'_flowpct
		bys weo: egen end_L1_`tp'_`tt'_flowpct = ///
			max(cond(!mi(L1_`tp'_`tt'_flowpct), DATE, .))
	}
}

cap ren *equity_local* *equity_loc*
cap ren *equity_foreign* *equity_fgn*
cap ren *equity_etf* *equity_etf*
cap ren *equity_active* *equity_act*

format start_L1_* end_L1_* %td
	tab start_L1_equity_loc_flowpct start_L1_equity_fgn_flowpct, m
	tab weo if mi(start_L1_equity_loc_flowpct)

keep if year > 1990 & year < 2018

xtset weo DATE
compress

* Standardize sentiment index
ds *sent*
foreach vv in `r(varlist)' {
	cap ren `vv' _raw
	cap drop _mean _sd _raw
	bys weo: egen _mean = mean(_raw)
	bys weo: egen _sd = sd(_raw)
	di "bys weo: gen `vv' = (_raw - _mean) / _sd"
	bys weo: gen `vv' = (_raw - _mean) / _sd
	cap drop _mean _sd _raw
}

* Winsorized versions
ds vlm_ma*_adj vol_ma* 
foreach vv in `r(varlist)' {
	winsor2 `vv', by(weo) suffix(_w) cuts(0.5 99.5)
}

* Add variables to the list to merge in additional lags
ds factor_sent1_best_filter factor_sent2_best_filter ///
	factor_sentu1_best_filter /// 
	factor_negative_filter factor_negativeTonal  ///
	vix vix_w commo R_MSCI_EM R_DJ_global R_glb_bond_index /// 
	epu ln_epu cesi_usd cesi_eur cesi_cny cesi_g10
foreach vv in `r(varlist)' {
	
	preserve
	
	keep if weo==111
	
	cap drop L*_*
	keep DATE `vv'
	tsset DATE
	di "CREATING LAGS FOR: `vv'"
	
	keep if !mi(`vv')
	forval l=1(1)$Nlags {
		gen L`l'_`vv' = `vv'[_n-`l']
	}
	order L*_`vv', after(`vv')
	
	keep DATE L*_`vv'
	forval l=1(1)$Nlags {
		cap lab var L`l'_`vv' ///
		"`l'-day lag of `vv', latest value available ignoring missing values"
	}
	compress
	tempfile lags_`vv'
	save `lags_`vv''
	
	restore
	
	merge m:1 DATE using `lags_`vv'' /// 
		, keep(1 3) nogen keepusing(*`vv')
	order L*_`vv', after(`vv')
}

* Add variables to the list to merge in additional lags
ds returns ///
	vlm_ma60_adj vol_ma60 vlm_ma60_adj_w vol_ma60_w /// 
	art0 sent1 art1 sent2 art2 ///
	sent1_best art1_best sent2_best art2_best ///
	t_sent1_best_local t_art1_best_local ///
	t_sent2_best_local t_art2_best_local ///
	sentu1 artu1 sentu1_best artu1_best ///
	t_sentu1_best_local t_artu1_best_local ///
	negative negativeTonal artTonal1
foreach vv in `r(varlist)' {
	
	preserve
	
	cap drop L*_*
	keep weo DATE `vv'
	xtset weo DATE
	di "CREATING LAGS FOR: `vv'"
	
	keep if !mi(`vv')
	forval l=1(1)$Nlags {
		gen L`l'_`vv' = `vv'[_n-`l']
	}
	order L*_`vv', after(`vv')
	
	keep weo DATE L*_`vv'
	forval l=1(1)$Nlags {
		cap lab var L`l'_`vv' ///
		"`l'-day lag of `vv', latest value available ignoring missing values"
	}
	compress
	tempfile lags_`vv'
	save `lags_`vv''
	
	restore
	
	merge 1:1 weo DATE using `lags_`vv'' /// 
		, keep(1 3) nogen keepusing(*`vv')
	order L*_`vv', after(`vv')
}

* Dummies for day of the week
cap drop day_of_week 
cap drop dow*
gen day_of_week = dow(DATE)
	tab day_of_week, gen(dow)

* Shorthand for dummy controls
ds dow2-dow5 
global other_dummies = "`r(varlist)'"

********************************************************************************
*** LOCAL PROJECTION ***
********************************************************************************

* Cumulative subsequent returns to go on LHS
cap drop F*_index*
qui xtset weo DATE
qui cap drop notmiss
qui gen notmiss = index < .
qui cap drop SDATE // ignore weekends when calculating returns
qui gen SDATE = cond(notmiss, sum(notmiss), .)

xtset weo SDATE
forval h = 0(1)$horizonFlows {

	gen F`h'_index = ((F`h'.index / L.index) - 1) * 100
	replace F`h'_index = . if F`h'_index==0 // holidays during the week
	lab var F`h'_index "`h'-day subsequent cumulative returns of index (%)"
	
	bysort weo: egen _mean = mean(F`h'_index)
	bysort weo: egen _sd = sd(F`h'_index)
	
	gen F`h'_index_norm = (F`h'_index - _mean) / _sd
	cap drop _mean _sd
}
qui xtset weo DATE
qui cap drop notmiss SDATE

* Forwards to go on LHS, marginal version
ds d_returns_h d_returns_l 
loc LP_LHS = "`r(varlist)'"
foreach vv in `LP_LHS' {
	preserve
		cap drop F*_*
		keep weo DATE `vv'
		xtset weo DATE
		keep if !mi(`vv')
		
		* To be used for each horizon of LP regression
		forval h = 0(1)$horizonFlows {
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
	merge 1:1 weo DATE using `tmp_F_`vv'', nogen
	gen L0_`vv' = `vv'
}
xtset weo DATE

* Map countries to AE/EM
do "cmap.do"

* Shorthand for lags row vectors
ds returns ///
	vlm_ma60_adj vol_ma60 vlm_ma60_adj_w vol_ma60_w /// 
	equity_flowpct ///
	equity_loc_flowpct equity_fgn_flowpct ///
	equity_etf_flowpct equity_act_flowpct ///
	vix vix_w commo R_DJ_global R_glb_bond_index ///
	R_MSCI_EM /// 
	epu ln_epu ///
	cesi_usd cesi_eur cesi_cny cesi_g10 ///
	art0 art1 art2 sent1 sent2 ///
	art1_best art2_best sent1_best sent2_best ///
	t_sent1_best_local t_art1_best_local ///
	t_sent2_best_local t_art2_best_local ///
	factor_sent1_best_filter factor_sent2_best_filter /// 
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

********************************************************************************

keep iso weo DATE AE ///
	F*_index F*_d_returns_h F*_d_returns_l ///
	returns ${L_returns} ///
	F*_cum_*equity_*flow* d_F*_cum_*_h d_F*_cum_*_l ///
	equity_flowpct ${L_equity_flowpct} /// 
	equity_loc_flowpct ${L_equity_loc_flowpct} /// 
	equity_fgn_flowpct ${L_equity_fgn_flowpct} /// 
	equity_etf_flowpct ${L_equity_etf_flowpct} /// 
	equity_act_flowpct ${L_equity_act_flowpct} /// 
	art0 ${L_art0} ///
	sent1 ${L_sent1} art1 ${L_art1} ///
	sent2 ${L_sent2} art2 ${L_art2} ///
	sent1_best ${L_sent1_best} art1_best ${L_art1_best} ///
	t_sent1_best_local ${L_t_sent1_best_local} ///
	t_art1_best_local ${L_t_art1_best_local} ///
	factor_sent1_best_filter ${L_factor_sent1_best_filter} ///
	sent2_best ${L_sent2_best} art2_best ${L_art2_best} ///
	t_sent2_best_local ${L_t_sent2_best_local} ///
	t_art2_best_local ${L_t_art2_best_local} ///
	factor_sent2_best_filter ${L_factor_sent2_best_filter} ///
	negative ${L_negative} ///
	factor_negative_filter ${L_factor_negative_filter} ///
	negativeTonal ${L_negativeTonal} ///
	artTonal1 ${L_artTonal1} ///
	factor_negativeTonal ${L_factor_negativeTonal} ///
	sentu1 ${L_sentu1} artu1 ${L_artu1} ///
	sentu1_best ${L_sentu1_best} artu1_best ${L_artu1_best} ///
	t_sentu1_best_local ${L_t_sentu1_best_local} ///
	t_artu1_best_local ${L_t_artu1_best_local} ///
	factor_sentu1_best_filter ${L_factor_sentu1_best_filter} ///
	epu ${L_epu} ln_epu ${L_ln_epu} ///
	cesi_usd ${L_cesi_usd} cesi_eur ${L_cesi_eur} ///
	cesi_cny ${L_cesi_cny} cesi_g10 ${L_cesi_g10} ///
	vlm_ma60_adj_w ${L_vlm_ma60_adj_w} vlm_ma60_adj ${L_vlm_ma60_adj} ///
	vol_ma60_w ${L_vol_ma60_w} vol_ma60 ${L_vol_ma60} /// 
	vix_w ${L_vix_w} vix ${L_vix} ///
	commo ${L_commo} ///
	R_DJ_global ${L_R_DJ_global} ///
	R_MSCI_EM ${L_R_MSCI_EM} ///
	${other_dummies} 
	
compress
save "..\data\regsample.dta", replace


cap log close
