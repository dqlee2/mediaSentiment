cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_a1_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
* Figure A1. Main topics Covered - All countries
********************************************************************************

* Stylized facts on articles 
* Number of articles total, Average per day, main topics

use "..\data\news.dta", clear
cap drop *_loc0 *_loc1

cap ren ISO2 iso
xtset weo DATE
	
cap drop *_long
cap drop *best*
gen constant = 1

ds art0 t_art0_*
loc vlist = "`r(varlist)'"
foreach stat in total mean {
	loc `stat'
	foreach y in `vlist' {
		loc `stat' ``stat'' `stat'_`y'=`y'
	}
	di `"``stat''"'
}

collapse (sum) `total' (mean) `mean', by(constant)

cap ren total_art0 total_t_art0_alltopics
cap ren mean_art0 mean_t_art0_alltopics

reshape long total_t_art0_ mean_t_art0_, i(constant) j(topic) string
cap ren *t_art*_ *art*_topic

cap drop order
gen order = 1 if topic=="alltopics"

loc cnt = 2
foreach tt in commfinmkt commmkt equitymkt debtmkt derivscrty moneyfxmkt ///
	ecn ecnind politgen ecnmoneypol govtfin tradeextrnl corpindl local {
	
	di "replace order = `cnt++' if topic==`tt'"
	replace order = `cnt++' if topic=="`tt'"
}

cap drop tname
gen tname = ""

replace tname = "All Topics" if topic=="alltopics"
replace tname = "Commodity/Financial" if topic=="commfinmkt"
replace tname = "Commodity" if topic=="commmkt"
replace tname = "Corporate/Industrial" if topic=="corpindl"
replace tname = "Debt/Bond" if topic=="debtmkt"
replace tname = "Derivative Securities" if topic=="derivscrty"
replace tname = "Economic" if topic=="ecn"
replace tname = "Economic/Indicators" if topic=="ecnind"
*replace tname = "Economic Performance/Indicators" if topic=="ecnind"
replace tname = "Monetary Policy" if topic=="ecnmoneypol"
replace tname = "Equity" if topic=="equitymkt"
replace tname = "Government Finance" if topic=="govtfin"
replace tname = "Local" if topic=="local"
replace tname = "Money/Forex" if topic=="moneyfxmkt"
replace tname = "Political/General" if topic=="politgen"
replace tname = "Table" if topic=="table"
replace tname = "Tables" if topic=="tables"
replace tname = "Trade/External Payments" if topic=="tradeextrnl"

drop if inlist(topic, "table")
drop if inlist(topic, "tables")
drop if inlist(topic, "tradeextrnl")
drop if inlist(topic, "ecnind")
drop if inlist(topic, "debtmkt")

sort order
cap drop TNAME
*encode tname, gen(TNAME)
gen TNAME = _n
labmask TNAME, values(tname)

* Figure A1. Main topics Covered - All countries

twoway bar total_art0_topic TNAME, ///
	ytitle(Count) ylabel(#3) graphregion(color(white) margin(medlarge)) /// 
	title("") xlabel(1(1)12, angle(vertical) valuelabel labsize(medium)) ///
	ylabel(, labsize(medium) format(%9.0fc)) ///
	ytitle("Total number of articles", size(medium)) xtitle("")
graph export "..\results\figure_a1.pdf", as(pdf) replace 
graph close

* Stylized facts on articles with local news
qui su total_art0_topic if topic=="local"
loc art0_local = r(mean)
qui su total_art0_topic if topic=="alltopics"
loc art0_alltopics = r(mean)

* Number of articles with local news
di `art0_local' 

* Share of articles with local news
di `art0_local' / `art0_alltopics'

* Share of articles discussing multiple countries
di 1 - `art0_local' / `art0_alltopics'

* Number of articles dropped by excluding articles mentioning any other country
di `art0_alltopics' - `art0_local'


********************************************************************************
* Extra: Similar distribution of topics observed across AE and EM

foreach sample in AE EM {

* Stylized facts on articles 
* Number of articles total, Average per day, main topics

use "..\data\news.dta", clear
cap drop *_loc0 *_loc1

cap ren ISO2 iso
xtset weo DATE

do "cmap.do"

if "`sample'"=="AE" {
	keep if AE==1
}
if "`sample'"=="EM" {
	keep if AE==0
}
	
cap drop *_long
cap drop *best*
gen constant = 1

ds art0 t_art0_*
loc vlist = "`r(varlist)'"
foreach stat in total mean {
	loc `stat'
	foreach y in `vlist' {
		loc `stat' ``stat'' `stat'_`y'=`y'
	}
	di `"``stat''"'
}

collapse (sum) `total' (mean) `mean', by(constant)

cap ren total_art0 total_t_art0_alltopics
cap ren mean_art0 mean_t_art0_alltopics

reshape long total_t_art0_ mean_t_art0_, i(constant) j(topic) string
cap ren *t_art*_ *art*_topic

cap drop order
gen order = 1 if topic=="alltopics"

loc cnt = 2
foreach tt in commfinmkt commmkt equitymkt debtmkt derivscrty moneyfxmkt ///
	ecn ecnind politgen ecnmoneypol govtfin tradeextrnl corpindl local {
	
	di "replace order = `cnt++' if topic==`tt'"
	replace order = `cnt++' if topic=="`tt'"
}

cap drop tname
gen tname = ""

replace tname = "All Topics" if topic=="alltopics"
replace tname = "Commodity/Financial" if topic=="commfinmkt"
replace tname = "Commodity" if topic=="commmkt"
replace tname = "Corporate/Industrial" if topic=="corpindl"
replace tname = "Debt/Bond" if topic=="debtmkt"
replace tname = "Derivative Securities" if topic=="derivscrty"
replace tname = "Economic" if topic=="ecn"
replace tname = "Economic/Indicators" if topic=="ecnind"
*replace tname = "Economic Performance/Indicators" if topic=="ecnind"
replace tname = "Monetary Policy" if topic=="ecnmoneypol"
replace tname = "Equity" if topic=="equitymkt"
replace tname = "Government Finance" if topic=="govtfin"
replace tname = "Local" if topic=="local"
replace tname = "Money/Forex" if topic=="moneyfxmkt"
replace tname = "Political/General" if topic=="politgen"
replace tname = "Table" if topic=="table"
replace tname = "Tables" if topic=="tables"
replace tname = "Trade/External Payments" if topic=="tradeextrnl"

drop if inlist(topic, "table")
drop if inlist(topic, "tables")
drop if inlist(topic, "tradeextrnl")
drop if inlist(topic, "ecnind")
drop if inlist(topic, "debtmkt")

sort order
cap drop TNAME
*encode tname, gen(TNAME)
gen TNAME = _n
labmask TNAME, values(tname)

twoway bar total_art0_topic TNAME, ///
	ytitle(Count) ylabel(#3) graphregion(color(white) margin(medlarge)) /// 
	title("") xlabel(1(1)12, angle(vertical) valuelabel labsize(medium)) ///
	ylabel(, labsize(medium) format(%9.0fc)) ///
	ytitle("Total number of articles", size(medium)) xtitle("")
graph export "..\results\figure_a1_`sample'.pdf", as(pdf) replace 
graph close
} // end sample loop

cap log close
