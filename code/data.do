cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\data_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** GLOBAL BOND INDEXES ***
********************************************************************************

import excel using "..\data\Bond_Indexes.xlsx", sheet("Sheet3") clear 
ren A date
ren B glb_bond_index
drop in 1/6
gen DATE = date(date, "MDY"), before(date)
format DATE %td
cap drop date
destring glb_bond_index, replace ignore("NA")
cap drop R_*
tsset DATE
ds glb_bond_index
foreach vv of varlist `r(varlist)' { 
	qui tsset DATE
	qui cap drop notmiss
	qui gen notmiss = `vv' < .
	qui cap drop SDATE // ignore weekends when calculating returns
	qui gen SDATE = cond(notmiss, sum(notmiss), .)
	tsset SDATE
	*gen R_`vv' = (`vv' - L.`vv') * 100
	gen R_`vv' = ((`vv' / L.`vv') - 1) * 100
	replace R_`vv' = . if R_`vv'==0
	lab var R_`vv' "Daily returns of `vv' (%)"
	cap drop notmiss SDATE
}
drop if year(DATE) < 1993
drop if year(DATE)==1993
tsset DATE
compress
d
save "..\data\glb_bond_index.dta", replace

********************************************************************************
*** FX ***
********************************************************************************

* BIS, US dollar exchange rates. Daily data. CSV vertical format. 
* https://www.bis.org/statistics/full_webstats_xru_current_d_dataflow_csv_row.zip

insheet using "..\data\WEBSTATS_XRU_CURRENT_D_DATAFLOW_csv_row.csv" ///
		, comma clear nonames

drop in 1/4
ren v1 str_date

ds str_date, not
foreach vv in `r(varlist)' {
	replace `vv' = subinstr(`vv', ":", "_", .) if _n==1
	loc _tmp = `vv'[1]
	ren `vv' fx`_tmp'
}
drop in 1

ren *_A *
ren fxD_* fx*

destring, replace ignore("NaN")

gen DATE = date(str_date, "YMD"), after(str_date)
	format DATE %td
	drop str_date

reshape long fx, i(DATE) j(ccy) string

gen iso = substr(ccy, 1, 2)
replace ccy = substr(ccy, 4, 3)
	tab iso, m
	
preserve
	keep iso
	duplicates drop
	kountry iso, from(iso2c) to(imfn)
		ren _IMFN_ weo
	tempfile _tmp
	save `_tmp'
restore

merge m:1 iso using `_tmp', nogen keepusing(weo)

tab iso if mi(weo)
drop if mi(weo)
xtset weo DATE

* Lags

ds fx 
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
	
	keep weo DATE L*_`vv' `vv'
	
	forval l=1(1)$Nlags {
		cap lab var L`l'_`vv' ///
		"`l'-day lag of `vv', latest value available ignoring missing values"
	}
	
	compress
	tempfile _tmp
	save `_tmp'
	
restore

merge m:1 weo DATE using `_tmp', nogen keepusing(L*_`vv')

}
gen R_fx = (fx - L1_fx) / L1_fx * 100

ds R_fx 
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
	
	keep weo DATE L*_`vv' `vv'
	
	forval l=1(1)$Nlags {
		cap lab var L`l'_`vv' ///
		"`l'-day lag of `vv', latest value available ignoring missing values"
	}
	
	compress
	tempfile _tmp
	save `_tmp'
	
restore

merge m:1 weo DATE using `_tmp', nogen

}

* Volatility

cap drop vol*
foreach vv in fx R_fx {
	
cap drop mean_`vv'
bysort weo: egen mean_`vv'=mean(`vv')

cap drop residual2*
gen residual2 = (`vv'-mean_`vv')^2
lab var residual2 "Squared residual of `vv'"

preserve 

keep weo iso DATE residual2 // sent1

tempfile tmp
save `tmp'

foreach mm in 60 { // 30 
	
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
	
	gen vol_ma`mm'_`vv' = residual2 - residual2_ma`mm'
	lab var vol_ma`mm'_`vv' "vol_`vv', `mm' day window"
	
	di "winsor2 vol_ma`mm'_`vv', by(weo) suffix(_w) cuts(0.5 99.5)"
	qui winsor2 vol_ma`mm'_`vv', by(weo) suffix(_w) cuts(0.5 99.5)
	
	compress
	tempfile _tmp`mm'
	save `_tmp`mm''
}

restore

foreach mm in 60 {
	merge m:1 weo DATE using `_tmp`mm'', nogen
}
}
ds fx R_fx
foreach vv in `r(varlist)' {

preserve

	cap drop L*_*
	keep weo DATE `vv' vol_ma*_`vv'* 
	xtset weo DATE
	
	di "CREATING LAGS FOR: `vv'"
	keep if !mi(`vv')
	
	forval l=1(1)$Nlags {
		
		gen L`l'_`vv' = `vv'[_n-`l']
		
		foreach mm in 60 {
			gen L`l'_vol_ma`mm'_`vv'_w = vol_ma`mm'_`vv'_w[_n-`l']
		}
	}
	order L*_`vv'*, after(`vv')
	
	keep weo DATE `vv' *vol_ma* 
	
	forval l=1(1)$Nlags {
		
		cap lab var L`l'_`vv' ///
		"`l'-day lag of R_`vv', latest value available ignoring missing values"
		
		foreach mm in 60 {
			
			cap lab var L`l'_vol_ma`mm'_`vv'_w ///
			"`l'-day lag of vol_ma`mm'_`vv'_w, latest value available ignoring missing values"
		}
	}
	
	compress
	tempfile _tmp
	save `_tmp'
	
restore

merge m:1 weo DATE using `_tmp', nogen

}

compress
save "..\data\fx_bis.dta", replace

********************************************************************************
*** STOCK INDEX AND VOLMES ***
********************************************************************************

* Price and volume data from datastream
insheet using "..\data\Trading volumes Data.csv", comma clear case
qui d
loc k = round(`r(k)' / 5)
forval i = 1(1)`k' {
	loc j1 = 5 * `i' - 4
	loc j2 = 5 * `i' - 3
	loc j3 = 5 * `i' - 2
	loc j4 = 5 * `i' - 1
	di "COLUMNS: `j1', `j2', `j3', `j4'"
	preserve
		keep v`j1' v`j2' v`j3' v`j4'
		loc nm = v`j2'[1]
		loc nm = trim("`nm'")
		loc nm = lower("`nm'")
		di "COUNTRY: `nm'"
		gen country = "`nm'"
		drop in 1/3
		drop if trim(v`j2')==""
		destring v`j3', replace ignore("#N/A") force
		destring v`j4', replace ignore("#N/A") force
		ren v`j2' date
		ren v`j3' index
		ren v`j4' volume
		drop if trim(date)==""
		gen DATE = date(date, "MDY", 2000), after(date)
		format DATE %tdnn/dd/CCYY
		lab var DATE "Date (MM/DD/YYYY)"
		cap keep country DATE index volume
		duplicates drop
		duplicates report DATE
		tempfile tmp`i'
		save `tmp`i''
	restore
}
use `tmp1', clear
forval i = 2(1)`k' {
	append using `tmp`i''
}
kountry country, from(other) stuck
cap ren _ISO3N_ iso3n
kountry iso3n, from(iso3n) to(imfn)
cap ren _IMFN_ weo
cap drop iso3n 
drop if country=="italy"
drop if country=="south africa"
drop if country=="russia"
drop if country=="russia 2"
tab country, m
compress
save "..\data\Trading_volumes_Data.dta", replace

* Extra volume information
insheet using "..\data\Trading volumes Yellow.csv", comma clear case
qui d
loc k = round(`r(k)' / 3)
forval i = 1(1)`k' {
	loc j1 = 3 * `i' - 2
	loc j2 = 3 * `i' - 1
	di "COLUMNS: `j1', `j2'"
	preserve
		keep v`j1' v`j2'
		loc nm = v`j1'[1]
		loc id = v`j1'[2]
		loc nm = trim("`nm'")
		loc nm = lower("`nm'")
		di "COUNTRY: `nm', INDEX: `id'"
		gen country = "`nm'"
		gen index_title = "`id'"
		drop in 1/4
		drop if trim(v`j2')==""
		destring v`j2', replace ignore("#N/A") force
		ren v`j1' date
		ren v`j2' volume
		drop if trim(date)==""
		gen DATE = date(date, "MDY", 2000), after(date)
		format DATE %tdnn/dd/CCYY
		lab var DATE "Date (MM/DD/YYYY)"
		cap keep country index_title DATE volume
		duplicates drop
		duplicates report DATE
		tempfile tmp`i'
		save `tmp`i''
	restore
}
use `tmp1', clear
forval i = 2(1)`k' {
	append using `tmp`i''
}
kountry country, from(other) stuck
cap ren _ISO3N_ iso3n
kountry iso3n, from(iso3n) to(imfn)
cap ren _IMFN_ weo
cap drop iso3n 
keep if inlist(index_title, ///
	"FTSEMIB Index", "MXRU Index", "JALSH Index", "MIB30 Index")
tab country index_title, m
compress
save "..\data\Trading_volumes_Yellow.dta", replace

* Extra price information
insheet using ///
	"..\data\Trading volumes CorrespondingPrices.csv" ///
	, comma clear case
qui d
loc k = round(`r(k)' / 3)
forval i = 1(1)`k' {
	loc j1 = 3 * `i' - 2
	loc j2 = 3 * `i' - 1
	di "COLUMNS: `j1', `j2'"
	preserve
		keep v`j1' v`j2'
		loc nm = v`j1'[1]
		loc id = v`j1'[2]
		loc nm = trim("`nm'")
		loc nm = lower("`nm'")
		di "COUNTRY: `nm', INDEX: `id'"
		gen country = "`nm'"
		gen index_title = "`id'"
		drop in 1/4
		drop if trim(v`j2')==""
		destring v`j2', replace ignore("#N/A") force
		ren v`j1' date
		ren v`j2' index
		drop if trim(date)==""
		gen DATE = date(date, "MDY", 2000), after(date)
		format DATE %tdnn/dd/CCYY
		lab var DATE "Date (MM/DD/YYYY)"
		cap keep country index_title DATE index
		duplicates drop
		duplicates report DATE
		tempfile tmp`i'
		save `tmp`i''
	restore
}
use `tmp1', clear
forval i = 2(1)`k' {
	append using `tmp`i''
}
kountry country, from(other) stuck
cap ren _ISO3N_ iso3n
kountry iso3n, from(iso3n) to(imfn)
cap ren _IMFN_ weo
cap drop iso3n 
keep if inlist(index_title, ///
	"FTSEMIB Index", "MXRU Index", "JALSH Index", "MIB30 Index")
tab country index_title, m
compress
save "..\data\Trading_volumes_CorrespondingPrices.dta", replace

* Volumes from Bloomberg
insheet using ///
	"..\data\Trading volumes MissingfromReuters.csv" ///
	, comma clear case
qui d
loc k = round(`r(k)' / 3)
forval i = 1(1)`k' {
	loc j1 = 3 * `i' - 2
	loc j2 = 3 * `i' - 1
	di "COLUMNS: `j1', `j2'"
	preserve
		keep v`j1' v`j2'
		loc nm = v`j1'[1]
		loc id = v`j1'[2]
		loc nm = trim("`nm'")
		loc nm = lower("`nm'")
		di "COUNTRY: `nm', INDEX: `id'"
		gen country = "`nm'"
		gen index_title = "`id'"
		drop in 1/4
		drop if trim(v`j2')==""
		destring v`j2', replace ignore("#N/A") force
		ren v`j1' date
		ren v`j2' volume
		drop if trim(date)==""
		gen DATE = date(date, "MDY", 2000), after(date)
		format DATE %tdnn/dd/CCYY
		lab var DATE "Date (MM/DD/YYYY)"
		cap keep country index_title DATE volume
		duplicates drop
		duplicates report DATE
		tempfile tmp`i'
		save `tmp`i''
	restore
}
use `tmp1', clear
forval i = 2(1)`k' {
	append using `tmp`i''
}
kountry country, from(other) stuck
cap ren _ISO3N_ iso3n
kountry iso3n, from(iso3n) to(imfn)
cap ren _IMFN_ weo
cap drop iso3n 
drop if index_title=="MEXBOL Index"
compress
save "..\data\Trading_volumes_MissingfromReuters.dta", replace

* Combine extra volume and price info
use "..\data\Trading_volumes_Yellow.dta", clear
merge 1:1 weo index_title DATE using ///
	"..\data\Trading_volumes_CorrespondingPrices.dta", nogen
sort weo index_title DATE

* Combine FTSE MIB and MIB 30, adjusting for breaks
preserve
	keep if index_title=="MIB30 Index"
	replace index_title ="FTSEMIB Index"
	keep weo DATE country index_title index volume
	cap ren index index_mib30
	cap ren volume volume_mib30
	compress
	tempfile mib30
	save `mib30'
restore
merge m:1 weo country DATE index_title using `mib30', nogen
sort weo index_title DATE

/* The base value of the FTSE MIB Index was set at the level of the MIB 30 Index 
at the close of trading on 31 October 2003 (10,644). Historical values for the 
FTSE MIB Index have been back calculated to 31 December 1997 (24,402). The value 
for FTSE MIB Banks 15% Capped Index was set at 10,000 at the close of trading on 
21 September 2012. */

replace index = index_mib30 ///
	if index_title=="FTSEMIB Index" & DATE<=td(31oct2003)
gen tmp_ratio = volume / volume_mib30 ///
	if index_title=="FTSEMIB Index" & DATE==td(31oct2003)
bys index_title: egen break_adj = min(tmp_ratio)
replace volume = volume_mib30 * break_adj ///
	if mi(volume) & index_title=="FTSEMIB Index"
sort weo index_title DATE
cap drop tmp_*
cap drop *_adj
cap drop *_mib30
drop if index_title=="MIB30 Index"
xtset weo DATE
order weo DATE index_title index volume

* Combine with volume and price info for remaining countries
append using "..\data\Trading_volumes_Data.dta"
append using "..\data\Trading_volumes_MissingfromReuters.dta"
xtset weo DATE
kountry weo, from(imfn) to(iso2c)
ren _ISO2C_ iso
tab iso, m
compress
keep DATE index volume iso
cap ren index index_
cap ren volume volume_
reshape wide index_ volume_, i(DATE) j(iso) string
tsset DATE
compress
save "..\data\Trading_volumes.dta", replace


********************************************************************************
*** STOCK INDEX FROM DATASTREAM ***
********************************************************************************

* COUNTRIES: AR AU BR CA CH CL CN DE ES FR GB GR HU ID IE IN IT JP KR MX MY NO 
* NZ PE PH PL PT RU SE TH TR US ZA
insheet using "..\data\INDEX.csv", comma clear case
drop in 1/3
ds v1, not
foreach var in `r(varlist)' {
	loc lb = `var'[1]
	loc lb = regexr("`lb'", " [U]?[$]", " USD$")
	if "`lb'"=="." drop `var'
	else {
		di "LABEL: `lb'"
		loc nm = `var'[2]
		loc nm = regexr("`nm'", "[$]", "U")
		loc nm = regexr("`nm'", "[&]", "A")
		di "VARNAME: `nm'"
		lab var `var' "`lb'"
		ren `var' `nm'
	}
}
gen DATE = date(v1, "MDY", 2000), after(v1)
format DATE %tdnn/dd/CCYY
lab var DATE "Date (MM/DD/YYYY)"
drop v1
drop in 1/2
ds DATE, not
foreach var in `r(varlist)' {
	destring `var', ignore("NA") replace
}
cap ren ARGMERV INDLOCAR 
*cap ren BRBOVES INDLOCBR 
cap ren IGPAGEN INDLOCCL 
*cap ren CHSCOMP INDLOCCN 
*cap ren DAXINDX INDLOCDE 
*cap ren IBEX35I INDLOCES 
*cap ren FRCAC40 INDLOCFR 
*cap ren GRAGENL INDLOCGR 
*cap ren JAKCOMP INDLOCID 
*cap ren ISEQUIT INDLOCIE 
*cap ren IBOMSEN INDLOCIN 
*cap ren FTSEMIB INDLOCIT 
cap ren JAPDOWA INDLOCJP 
*cap ren KORCOMP INDLOCKR 
*cap ren MXIPC35 INDLOCMX 
*cap ren FBMKLCI INDLOCMY 
cap ren PEGENRL INDLOCPE 
*cap ren PSECOMP INDLOCPH 
*cap ren POLWIGI INDLOCPL 
*cap ren POPSI20 INDLOCPT 
*cap ren RSRTSIN INDLOCRU 
*cap ren BNGKSET INDLOCTH 
*cap ren TKNAT30 INDLOCTR 
*cap ren DJINDUS INDLOCUS 
*cap ren FJSR40L INDLOCZA
keep DATE INDLOC*
order DATE INDLOC*, alpha
tsset DATE
tsfill

* Merge in new index and volume data
merge 1:1 DATE using "..\data\Trading_volumes.dta"
cap drop _merge
ds index_* volume_*
foreach vv in `r(varlist)' {
	di "COUNT: `vv'"
	count if !mi(`vv')
}
foreach cc in AR CL JP PE {
	replace index_`cc' = INDLOC`cc' 
}
cap drop INDLOC*

* Calculate daily returns to stock index
cap drop R_*
ds index*
foreach vv of varlist `r(varlist)' { 
	qui tsset DATE
	qui cap drop notmiss
	qui gen notmiss = `vv' < .
	qui cap drop SDATE // ignore weekends when calculating returns
	qui gen SDATE = cond(notmiss, sum(notmiss), .)
	tsset SDATE
	gen R_`vv' = ((`vv' / L.`vv') - 1) * 100
	lab var R_`vv' "Daily returns of `vv' (%)"
	cap drop notmiss SDATE
}
d
tsset DATE

* Reshape into a panel of countries
reshape long index_ R_index_ volume_, i(DATE) j(iso) string
cap ren *index_ *index
cap ren volume_ volume
cap ren R_index returns
replace returns = . if returns==0 // these are holidays during the week
replace volume = . if mi(returns) // these are holidays during the week

* Name of stock index
gen index_title= ""
replace index_title = "ARGENTINA MERVAL - PRICE INDEX" if iso=="AR"
replace index_title = "BRAZIL BOVESPA - TOT RETURN IND" if iso=="BR"
replace index_title = "CHILE SANTIAGO SE GENERAL (IGPA) - PRICE INDEX" if iso=="CL"
replace index_title = "SHANGHAI SE COMPOSITE - PRICE INDEX" if iso=="CN"
replace index_title = "DAX 30 PERFORMANCE - PRICE INDEX" if iso=="DE"
replace index_title = "IBEX 35 - PRICE INDEX" if iso=="ES"
replace index_title = "FRANCE CAC 40 - PRICE INDEX" if iso=="FR"
replace index_title = "ATHEX COMPOSITE - PRICE INDEX" if iso=="GR"
replace index_title = "IDX COMPOSITE - PRICE INDEX" if iso=="ID"
replace index_title = "IRELAND SE OVERALL (ISEQ) - PRICE INDEX" if iso=="IE"
replace index_title = "S&P BSE (SENSEX) 30 SENSITIVE - PRICE INDEX" if iso=="IN"
replace index_title = "FTSE MIB INDEX - PRICE INDEX" if iso=="IT" // MIB 30 INDEX (before 10/31/2003)
replace index_title = "NIKKEI 225 STOCK AVERAGE - PRICE INDEX" if iso=="JP"
replace index_title = "KOREA SE COMPOSITE (KOSPI) - PRICE INDEX" if iso=="KR"
replace index_title = "MEXICO IPC (BOLSA) - PRICE INDEX" if iso=="MX"
replace index_title = "FTSE BURSA MALAYSIA KLCI - PRICE INDEX" if iso=="MY"
replace index_title = "S&P/BVL GENERAL (IGBVL) - PRICE INDEX" if iso=="PE"
replace index_title = "PHILIPPINE SE INDEX (PSEI) - PRICE INDEX" if iso=="PH"
replace index_title = "WIG20 INDEX" if iso=="PL"
replace index_title = "PORTUGAL PSI-20 - PRICE INDEX" if iso=="PT"
replace index_title = "MXRU INDEX" if iso=="RU"
replace index_title = "BANGKOK S.E.T. - PRICE INDEX" if iso=="TH"
replace index_title = "BIST NATIONAL 100 - PRICE INDEX" if iso=="TR"
replace index_title = "DOW JONES INDUSTRIALS - PRICE INDEX" if iso=="US"
replace index_title = "JALSH INDEX" if iso=="ZA"
tab index_title, m
kountry iso, from(iso2c) to(imfn)
ren _IMFN_ weo
order weo, before(iso)
xtset weo DATE
tsfill
lab var index "Local currency stock index"
lab var returns "Daily returns to stock index (%)"
lab var volume "Trading volume of stock index"
lab var index_title "Description of stock index"
order index_title, after(iso)
compress
save "..\data\INDEX.dta", replace

********************************************************************************
*** DOW JONES GLOBAL INDEX + MSCI EM ***
********************************************************************************

insheet using "..\data\MSCI.csv", comma clear
drop in 1/3
ds v1, not
foreach var in `r(varlist)' {
	loc lb = `var'[1]
	loc lb = regexr("`lb'", " [U]?[$]", " USD$")
	if "`lb'"=="." drop `var'
	else {
		di "LABEL: `lb'"
		loc nm = `var'[2]
		loc nm = regexr("`nm'", "[$]", "U")
		loc nm = regexr("`nm'", "[&]", "A")
		di "VARNAME: `nm'"
		lab var `var' "`lb'"
		ren `var' `nm'
	}
}
gen DATE = date(v1, "MDY", 2000), after(v1)
format DATE %tdnn/dd/CCYY
lab var DATE "Date (MM/DD/YYYY)"
drop v1
drop in 1/2
ds DATE, not
foreach var in `r(varlist)' {
	destring `var', ignore("NA") replace
}
tsset DATE
compress
save "..\data\MSCI.dta", replace

import excel using "..\data\global proxies.xlsx", clear
ren B date
ren C DJ_global
keep date DJ_global
lab var DJ_global "Dow Jones Global Index: World (Avg, Dec-31-91=100)"
drop in 1/10
destring DJ_global, replace
gen DATE = date(date, "DMY", 2000), after(date)
format DATE %tdnn/dd/CCYY
lab var DATE "Date (MM/DD/YYYY)"
drop date
tsset DATE
tsfill
merge 1:1 DATE using "..\data\MSCI.dta", keepusing(MSEMKFU) nogen
ren MSEMKFU MSCI_EM
d
* Calculate daily returns to index
cap drop R_*
ds DJ_global MSCI_EM
foreach vv of varlist `r(varlist)' { 
	qui tsset DATE
	qui cap drop notmiss
	qui gen notmiss = `vv' < .
	qui cap drop SDATE // ignore weekends when calculating returns
	qui gen SDATE = cond(notmiss, sum(notmiss), .)
	tsset SDATE
	gen R_`vv' = ((`vv' / L.`vv') - 1) * 100
	lab var R_`vv' "Daily returns of `vv' (%)"
	cap drop notmiss SDATE
}
d
tsset DATE
compress
save "..\data\global_proxies.dta", replace

********************************************************************************
*** VIX ***
********************************************************************************

insheet using "..\data\VIXCLS.csv", comma clear
ren vixcls vix

gen DATE = date(date, "YMD", 2000), after(date)
format DATE %tdnn/dd/CCYY

keep DATE vix
duplicates drop

cap destring vix, replace
lab var vix "CBOE Volatility Index"

tsset DATE
tsfill

winsor2 vix, suffix(_w) cuts(0.5 99.5)

compress
d
save "..\data\VIXCLS.dta", replace

********************************************************************************
*** COMMODITY INDICES ***
********************************************************************************

import excel using "..\data\Commodity indexes.xlsx", clear

drop in 3/15
cap drop A
ren B date

ds date, not
foreach var in `r(varlist)' {
	loc lb = `var'[2]
	loc lb = regexr("`lb'", " [U]?[$]", " USD$")
	di "LABEL: `lb'"
	loc nm = `var'[1]
	loc nm = regexr("`nm'", "[@]", "U")
	di "VARNAME: `nm'"
	lab var `var' "`lb'"
	ren `var' `nm'
}
drop in 1/2

gen DATE = date(date, "DMY", 2000), after(date)
format DATE %tdnn/dd/CCYY
lab var DATE "Date (MM/DD/YYYY)"
drop date

tsset DATE
tsfill

ds DATE, not
foreach var in `r(varlist)' {
	destring `var', ignore("NA") replace
}

rename GSCIUDAILY commo_index
rename GSENUDAILY energy_index
rename GSINUDAILY metal_index
rename GSPMUDAILY gold_index
rename GSAGUDAILY agr_index

ds commo_index
foreach vv of varlist `r(varlist)' { 
	qui tsset DATE
	qui cap drop notmiss
	qui gen notmiss = `vv' < .
	qui cap drop SDATE // ignore weekends when calculating returns
	qui gen SDATE = cond(notmiss, sum(notmiss), .)
	tsset SDATE
	gen commo = ((`vv' / L.`vv') - 1) * 100
	lab var commo "Daily returns of `vv' (%)"
	cap drop notmiss SDATE
}
compress
d
tsset DATE
save "..\data\Commodity_indexes.dta", replace

********************************************************************************
*** CITI ECONOMIC SURPRISE INDEX ***
********************************************************************************

* Citi ESI
import excel using "..\data\CESI.xlsx", clear 
cap ren B date
gen DATE = date(date, "DMY"), after(date)
format DATE %td
cap drop A date
ren C cesi_usd
ren D cesi_eur
ren E cesi_cny
ren F cesi_g10
drop if mi(DATE)
keep DATE cesi*
destring, replace
format cesi* %9.2fc
tsset DATE
cap drop R_*
ds cesi*
foreach vv of varlist `r(varlist)' { 
	qui tsset DATE
	qui cap drop notmiss
	qui gen notmiss = `vv' < .
	qui cap drop SDATE // ignore weekends when calculating returns
	qui gen SDATE = cond(notmiss, sum(notmiss), .)
	tsset SDATE
	gen R_`vv' = ((`vv' / L.`vv') - 1) * 100
	lab var R_`vv' "Daily returns of `vv' (%)"
	cap drop notmiss SDATE
}
compress
d
tsset DATE
save "..\data\CESI.dta", replace

********************************************************************************
*** CLEAN EPU ***
********************************************************************************

insheet using "..\data\EPU_All_Daily_Policy_Data.csv", comma clear

gen DATE = mdy(month, day, year)
format DATE %td
tsset DATE
gen weo = 111
ren daily_policy_index epu

keep weo DATE *epu*
order weo DATE *epu*
gen ln_epu = ln(epu)
tsset DATE

ds epu 
foreach vv in `r(varlist)' {
	gen `vv'_raw = `vv'
	cap ren `vv' _raw
	cap drop _mean _sd _raw
	egen _mean = mean(_raw)
	egen _sd = sd(_raw)
	di "gen `vv' = (_raw - _mean) / _sd"
	gen `vv' = (_raw - _mean) / _sd
	cap drop _mean _sd _raw
}

compress
save "..\data\EPU_All_Daily_Policy_Data.dta", replace

********************************************************************************
*** GOOD VS BAD TIMES ***
********************************************************************************

******************************************************
*** Good vs bad times based on real GDP growth

* Real GDP growth from IFS	

insheet using "..\data\IFS_09-25-2020 21-57-15-95.csv", comma clear nonames

ren v1 country
ren v2 weo
ren v3 variable
ren v4 code
ren v5 str_yq
ren v6 value
ren v7 status
drop v8

drop in 1

gen byte keep_obs = 0
replace keep_obs = 1 if regexm(lower(variable), "gross domestic product")
replace keep_obs = 1 if regexm(lower(variable), "consumer price index")
keep if keep_obs==1
	drop keep_obs
	
gen byte keep_obs = 0
gen name = ""

replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Domestic Currency"
replace name = "gdp_nsa" if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Domestic Currency"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Seasonally Adjusted, Domestic Currency"
replace name = "gdp_sa" if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Seasonally Adjusted, Domestic Currency"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Percent Change, Previous Period, Percent"
replace name = "g_gdp_nsa" if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Percent Change, Previous Period, Percent"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Percent Change, Previous Period, Seasonally Adjusted, Percent"
replace name = "g_gdp_sa" if variable=="Gross Domestic Product, Expenditure Approach, Nominal, Percent Change, Previous Period, Seasonally Adjusted, Percent"

replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Real, Domestic Currency"
replace name = "rgdp_nsa" if variable=="Gross Domestic Product, Expenditure Approach, Real, Domestic Currency"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Real, Seasonally Adjusted, Domestic Currency"
replace name = "rgdp_sa" if variable=="Gross Domestic Product, Expenditure Approach, Real, Seasonally Adjusted, Domestic Currency"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Real, Percent Change, Previous Period, Percent"
replace name = "g_rgdp_nsa" if variable=="Gross Domestic Product, Expenditure Approach, Real, Percent Change, Previous Period, Percent"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Real, Percent Change, Previous Period, Seasonally Adjusted, Percent"
replace name = "g_rgdp_sa" if variable=="Gross Domestic Product, Expenditure Approach, Real, Percent Change, Previous Period, Seasonally Adjusted, Percent"

replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Real, Spliced Historical Series, Domestic Currency"
replace name = "rgdp_spliced_nsa" if variable=="Gross Domestic Product, Expenditure Approach, Real, Spliced Historical Series, Domestic Currency"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Real, Spliced Historical Series, Seasonally Adjusted, Domestic Currency"
replace name = "rgdp_spliced_sa" if variable=="Gross Domestic Product, Expenditure Approach, Real, Spliced Historical Series, Seasonally Adjusted, Domestic Currency"

replace keep_obs = 1 if variable=="Statistical Discrepancy in Gross Domestic Product, Nominal, Domestic Currency"
replace name = "discrepancy_gdp_nsa" if variable=="Statistical Discrepancy in Gross Domestic Product, Nominal, Domestic Currency"
replace keep_obs = 1 if variable=="Statistical Discrepancy in Gross Domestic Product, Nominal, Seasonally Adjusted, Domestic Currency"
replace name = "discrepancy_gdp_sa" if variable=="Statistical Discrepancy in Gross Domestic Product, Nominal, Seasonally Adjusted, Domestic Currency"

replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Index"
replace name = "deflator_nsa" if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Index"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Seasonally Adjusted, Index"
replace name = "deflator_sa" if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Seasonally Adjusted, Index"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Percent Change, Previous Period, Percent"
replace name = "g_deflator_nsa" if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Percent Change, Previous Period, Percent"
replace keep_obs = 1 if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Percent Change, Previous Period, Seasonally Adjusted, Percent"
replace name = "g_deflator_sa" if variable=="Gross Domestic Product, Expenditure Approach, Deflator, Percent Change, Previous Period, Seasonally Adjusted, Percent"

replace keep_obs = 1 if variable=="Prices, Consumer Price Index, All items, Index"
replace name = "cpi" if variable=="Prices, Consumer Price Index, All items, Index"
replace keep_obs = 1 if variable=="Prices, Consumer Price Index, All items, Percentage change, Previous period, Percent"
replace name = "g_cpi" if variable=="Prices, Consumer Price Index, All items, Percentage change, Previous period, Percent"

keep if keep_obs==1
	drop keep_obs
	
keep country weo name str_yq value

gen year = substr(str_yq, 1, 4), after(str_yq)
gen quarter = substr(str_yq, 6, 1), after(str_yq)

destring year quarter, replace
	drop str_yq

gen yq = yq(year, quarter), after(year)
	format yq %tq

destring weo value, replace
reshape wide value, i(country weo quarter year yq) j(name) string
ren value* *

kountry weo, from(imfn) to(iso2c)
	ren _ISO2C_ iso
	drop if mi(iso)
	drop if mi(weo)

order country weo iso quarter year yq ///
	gdp_nsa gdp_sa g_gdp_nsa g_gdp_sa rgdp_nsa rgdp_sa g_rgdp_nsa g_rgdp_sa ///
	rgdp_spliced_nsa rgdp_spliced_sa discrepancy_gdp_nsa discrepancy_gdp_sa ///
	deflator_nsa deflator_sa g_deflator_nsa g_deflator_sa cpi g_cpi

xtset weo yq
tsfill	

* Adjust for breaks
ds gdp_nsa gdp_sa deflator_nsa deflator_sa 
foreach vv in `r(varlist)' {
	qui {
		gen _tmp = `vv'
		gen g0 = abs((_tmp - L._tmp) / L._tmp)
		gen g1 = abs((L._tmp - _tmp) / _tmp)
		gen g2 = max(g0, g1)
	}
	
	su g2, d
	gen byte break = g2 > (30 * `r(sd)') if !mi(g2)
	
	di "Breaks in variable: `vv'"
	tab country yq if break==1
	
	qui {
		gen F_g = (F._tmp - _tmp) / _tmp
		replace _tmp = F._tmp * (1 - F.F_g) if F.break==1
		replace break = F.break if F.break==1
		
		loc iter = 0
		while `iter'==0 {
			replace _tmp = F._tmp * (1 - F_g) if F.break==1 & break==0
			replace break = F.break if F.break==1 & !mi(break)
			count if F.break==1 & break==0
			if r(N)==0 {
				loc `iter++'
			}
		}
		replace _tmp = F._tmp * (1 - F_g) if F.break==1 & break==.
		replace break = F.break if F.break==1 & mi(break)
		replace `vv' = _tmp if break==1
		drop g0 g1 g2 break F_g _tmp
	}
}

merge m:1 weo using "..\data\country.dta", nogen keepusing(country) keep(3)

* Real GDP

gen rgdp = gdp_sa / deflator_sa
replace rgdp = gdp_nsa / deflator_nsa if iso=="TR"
replace rgdp = rgdp_spliced_nsa if iso=="AR"
replace rgdp = rgdp_spliced_nsa if iso=="CL"
replace rgdp = rgdp_spliced_nsa if iso=="PE"
replace rgdp = gdp_nsa / cpi if iso=="ID"
replace rgdp = gdp_nsa / deflator_nsa if iso=="MY"
replace rgdp = gdp_nsa / deflator_nsa if iso=="PH"
replace rgdp = gdp_nsa / cpi if iso=="RU"
replace rgdp = gdp_nsa / cpi if iso=="CN"

* YoY growth in quarterly real GDP

xtset weo yq
gen g4_rgdp = (rgdp - L4.rgdp) / rgdp * 100
	su g4_rgdp, d

* Dummy for negative GDP growth

gen byte neg_g4_rgdp = (g4_rgdp < 0)
	tab neg_g4_rgdp, m
	
compress
save "..\data\IFS_09-25-2020 21-57-15-95.dta", replace

******************************************************
*** Define market downturns using each country's stock index

* Bear or Bull based on stock index above or below HP filtered trend 

use "..\data\INDEX.dta", replace

keep weo DATE index returns
	xtset weo DATE
gen year = year(DATE)
gen month = month(DATE)

collapse (mean) index returns, by(weo year month)

drop if mi(year)
gen ym = ym(year, month)
	format ym %tm
xtset weo ym
tsfill

ds index returns
foreach vv in `r(varlist)' {
	
	bys weo: ipolate `vv' ym, gen(ip_`vv')
	replace `vv' = ip_`vv'
	drop ip_`vv'
	
	di "by weo: hprescott `vv', stub(hp)"
	by weo: hprescott `vv', stub(hp)
	
	egen double hpsm_`vv' = rowtotal(hp_`vv'_sm_*)
	drop hp_`vv'_sm_*
	
	egen double hpres_`vv' = rowtotal(hp_`vv'_*)
	drop hp_`vv'_*
	
	su hpres_`vv', d
	replace hpres_`vv' = hpres_`vv' / `r(sd)'
}

cap drop D1_*
cap drop D0_*
foreach vv in index returns {
	gen D1_`vv' = (hpres_`vv' < 0) & !mi(hpres_`vv')
	gen D0_`vv' = (hpres_`vv' > 0) & !mi(hpres_`vv')
}
save "..\data\recessionsIndex.dta", replace

******************************************************
/* Federal Reserve Bank of St. Louis, 
NBER based Recession Indicators for the United States 
from the Period following the Peak through the Trough [USREC], 
retrieved from FRED, Federal Reserve Bank of St. Louis; 
https://fred.stlouisfed.org/series/USREC, May 20, 2021. */

insheet using "..\data\USREC.csv", comma clear case

ren DATE sDATE
gen DATE = date(sDATE, "YMD")
gen year = year(DATE)
gen month = month(DATE)
drop DATE sDATE

gen ym = ym(year, month)
	format ym %tm

order ym year month USREC

compress
save "..\data\USREC.dta", replace

******************************************************
*** Define market downturns - us, global

use "..\data\INDEX.dta", replace

keep if weo==111

ren index DJ_US

merge 1:1 DATE using "..\data\global_proxies.dta", nogen keep(1 3)

ren DJ_global DJ_GL
keep DATE DJ_US DJ_GL

tsset DATE
gen year = year(DATE)
gen month = month(DATE)

collapse (mean) DJ_US DJ_GL, by(year month)

drop if mi(year)
gen ym = ym(year, month)
	format ym %tm
	tsset ym
	
ds DJ_US DJ_GL
foreach vv in `r(varlist)' {
	
	di "hprescott `vv', stub(hp)"
	hprescott `vv', stub(hp)
	
	egen double hpsm_`vv' = rowtotal(hp_`vv'_sm_*)
	drop hp_`vv'_sm_*
	
	egen double hpres_`vv' = rowtotal(hp_`vv'_*)
	drop hp_`vv'_*
	
	su hpres_`vv', d
	replace hpres_`vv' = hpres_`vv' / `r(sd)'
}

cap drop D1_*
cap drop D0_*
foreach vv in DJ_US DJ_GL {
	gen D1_`vv' = (hpres_`vv' < 0) & !mi(hpres_`vv')
	gen D0_`vv' = (hpres_`vv' > 0) & !mi(hpres_`vv')
}

merge m:1 year month using "..\data\USREC.dta", nogen keep(1 3)

gen D1_NBER = (USREC==1)
gen D0_NBER = (USREC==0)

compress
save "..\data\recessions.dta", replace

******************************************************
*** Define market downturns - country-specific

use "..\data\INDEX.dta", replace

merge m:1 DATE using "..\data\global_proxies.dta", nogen keep(1 3)

ren DJ_global DJ_GL
keep weo DATE index DJ_GL
xtset weo DATE

gen year = year(DATE)
gen month = month(DATE)

collapse (mean) index DJ_GL, by(weo year month)

drop if mi(year)
gen ym = ym(year, month)
	format ym %tm

xtset weo ym
bys weo (ym): replace index = index[_n-1] if mi(index) & !mi(index[_n-1])

ds index DJ_GL
foreach vv in `r(varlist)' {
	
	di "by weo: hprescott `vv', stub(hp)"
	by weo: hprescott `vv', stub(hp)
	
	egen double hpsm_`vv' = rowtotal(hp_`vv'_sm_*)
	drop hp_`vv'_sm_*
	
	egen double hpres_`vv' = rowtotal(hp_`vv'_*)
	drop hp_`vv'_*
	
	cap drop _sd
	egen _sd = sd(hpres_`vv'), by(weo)
	by weo: replace hpres_`vv' = hpres_`vv' / _sd
}

cap drop D1_*
cap drop D0_*
foreach vv in index DJ_GL {
	gen D1_`vv' = (hpres_`vv' < 0) & !mi(hpres_`vv') if !mi(`vv')
	gen D0_`vv' = (hpres_`vv' > 0) & !mi(hpres_`vv') if !mi(`vv')
}

merge m:1 year month using "..\data\USREC.dta", nogen keep(1 3)

gen D1_NBER = (USREC==1)
gen D0_NBER = (USREC==0)

compress
save "..\data\recessions_byc.dta", replace


cap log close
