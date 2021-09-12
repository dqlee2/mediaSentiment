cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\table_a1_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Table A1. Country and Time Coverage
********************************************************************************

* Start of coverage and end for news 
* by Country, Total number of articles, Average per day

use "..\data\news.dta", clear

cap ren ISO2 iso
xtset weo DATE

* Stylized facts for Total sample
preserve

ds sent1_best 
foreach vv in `r(varlist)' {
	
	egen `vv'_Start = min(cond(!mi(`vv'), DATE, .))
	egen `vv'_Stop = max(cond(!mi(`vv'), DATE, .))
	
	format `vv'_Start `vv'_Stop %tdnn/dd/CCYY
	tab iso `vv'_Start, m
}
compress

collapse (mean) *_Start *_Stop (sum) total_art0 = art0 ///
		(mean) average_art0 = art0
tempfile tmp_Total
save `tmp_Total'

restore


* Stylized facts for non-US sample
preserve

drop if iso=="US"

ds sent1_best 
foreach vv in `r(varlist)' {
	
	egen `vv'_Start = min(cond(!mi(`vv'), DATE, .))
	egen `vv'_Stop = max(cond(!mi(`vv'), DATE, .))
	
	format `vv'_Start `vv'_Stop %tdnn/dd/CCYY
	tab iso `vv'_Start, m
}
compress

collapse (mean) *_Start *_Stop (sum) total_art0 = art0 ///
		(mean) average_art0 = art0
tempfile tmp_US
save `tmp_US'

restore

* Stylized facts by country
ds sent1_best 
foreach vv in `r(varlist)' {
	
	bys weo (DATE): egen `vv'_Start = min(cond(!mi(`vv'), DATE, .))
	bys weo (DATE): egen `vv'_Stop = max(cond(!mi(`vv'), DATE, .))
	
	format `vv'_Start `vv'_Stop %tdnn/dd/CCYY
	tab iso `vv'_Start, m
}
compress

collapse (mean) *_Start *_Stop (sum) total_art0 = art0 ///
		(mean) average_art0 = art0, by(weo)

merge 1:1 weo using "..\data\country.dta", nogen keepusing(country)

append using `tmp_US'
append using `tmp_Total'

ren sent1_best_* News_*
ren total_art0 Number_Articles
ren average_art0 Average_per_day

replace country = "Non-US" if _n==_N-1
replace country = "Total" if _n==_N

do "cmap.do"
gen AE_EM = "EM"
replace AE_EM = "AE" if AE==1
replace AE_EM = "AE/EM" if country=="Non-US"
replace AE_EM = "AE/EM" if country=="Total"
drop weo AE

order country AE_EM News_Start News_Stop Number_Articles Average_per_day // weo 
	format Number_Articles %9.0fc
	format Average_per_day %9.2fc

compress
export excel using "..\results\results.xlsx", /// 
	sheet("table_a1") cell(A3) sheetmodify firstrow(variables) // 

* Average number of articles per non-US country
qui su Number_Articles if country=="Non-US"
loc art = r(mean)
distinct country if !inlist(country, "Non-US", "Total")
di `art' / r(ndistinct)

* Share of US articles
qui su Number_Articles if country=="United States"
loc art_US = r(mean)
qui su Number_Articles if country=="Total"
loc art_Total = r(mean)
di `art_US' / `art_Total'

cap log close
