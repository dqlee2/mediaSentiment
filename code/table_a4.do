cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\table_a4_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Table A4. Summary Statistics of the Sentiment Index - by country ***
********************************************************************************

use "..\data\regsample.dta", clear
xtset weo DATE

* Raw sentiment index (no rescaling)
cap noi drop sent1_best
merge 1:1 weo DATE using "..\data\news.dta" ///
	, nogen keep(1 3) ///
	keepusing(positive negative sent1_best positiveTonal negativeTonal)
ren sent1_best raw_sent1_best

* Variable names for summary statistics
ds raw_sent1_best
loc vlist = "`r(varlist)'"
foreach stat in count mean sd min p25 p50 p75 max {
	loc `stat'
	foreach y in `vlist' {
		loc `stat' ``stat'' `stat'_`y'=`y'
	}
	di `"``stat''"'
}

* Summary statistics by total sample
preserve

collapse (count) `count' (mean) `mean' (sd) `sd' ///
	(min) `min' (p25) `p25' (p50) `p50' (p75) `p75' (max) `max' 

gen Country = "Total"
gen Variable = "Sentiment", after(Country)
ren count N
ren mean Mean
ren sd SD
ren min Min
ren p25 p25
ren p50 Median
ren p75 p75
ren max Max

order Country Variable N Mean SD Min p25 Median p75 Max

export excel using "..\results\results.xlsx", /// 
	sheet("table_a4") cell(A30) sheetmodify firstrow(variables) // 

restore

* Summary statistics by AE vs. EM
preserve

collapse (count) `count' (mean) `mean' (sd) `sd' ///
	(min) `min' (p25) `p25' (p50) `p50' (p75) `p75' (max) `max' ///
	, by(AE)

gen Country = ""
replace Country = "AE" if AE==1
replace Country = "EM" if AE==0
sort Country
drop AE

gen Variable = "Sentiment", after(Country)
ren count N
ren mean Mean
ren sd SD
ren min Min
ren p25 p25
ren p50 Median
ren p75 p75
ren max Max

order Country Variable N Mean SD Min p25 Median p75 Max
	
export excel using "..\results\results.xlsx", /// 
	sheet("table_a4") cell(A28) sheetmodify firstrow(variables) // 

restore

* Summary statistics by country
preserve

collapse (count) `count' (mean) `mean' (sd) `sd' ///
	(min) `min' (p25) `p25' (p50) `p50' (p75) `p75' (max) `max' ///
	, by(iso)

ren iso Country
gen Variable = "Sentiment", after(Country)
ren count N
ren mean Mean
ren sd SD
ren min Min
ren p25 p25
ren p50 Median
ren p75 p75
ren max Max

order Country Variable N Mean SD Min p25 Median p75 Max

export excel using "..\results\results.xlsx", /// 
	sheet("table_a4") cell(A3) sheetmodify firstrow(variables) // 

restore

cap log close
