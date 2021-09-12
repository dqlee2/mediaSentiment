cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\table_a3_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Table A3. Asset Price Coverage - Stock Indices
********************************************************************************

* Start of coverage and end for stock indices by country

use "..\data\INDEX.dta", clear

drop if year(DATE) > 2015
drop if year(DATE) < 1991

xtset weo DATE

ds index 
foreach vv in `r(varlist)' {
	
	bys weo (DATE): egen `vv'_Start = min(cond(!mi(`vv'), DATE, .))
	bys weo (DATE): egen `vv'_End = max(cond(!mi(`vv'), DATE, .))
	
	format `vv'_Start `vv'_End %tdnn/dd/CCYY
	tab iso `vv'_Start, m
}
compress

collapse (mean) *_Start *_End, by(weo index_title)

ren index_Start Sample_Start
ren index_End Sample_End
ren index_title Index

merge 1:1 weo using "..\data\country.dta", nogen keepusing(country)

order weo country Sample_Start Sample_End Index
sort country

replace Index = subinstr(Index, " - PRICE INDEX", "", .)
replace Index = subinstr(Index, " - TOT RETURN IND", "", .)

compress
export excel using "..\results\results.xlsx", /// 
	sheet("table_a3") cell(A3) sheetmodify firstrow(variables) // 

cap log close
