cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_7_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 7. Multi-Country News and Global News Sentiment ***
********************************************************************************

* Quartiles of global news sentiment

use "..\data\factor_sent1_best.dta", clear

* Standardize sentiment index

ds factor_sent1_best_filter
foreach vv in `r(varlist)' {
	cap ren `vv' _raw
	cap drop _mean _sd _raw
	egen _mean = mean(_raw)
	egen _sd = sd(_raw)
	di "bys weo: gen `vv' = (_raw - _mean) / _sd"
	gen `vv' = (_raw - _mean) / _sd
	cap drop _mean _sd _raw
}

cap drop abs_factor_sent1_best_filter
gen abs_factor_sent1_best_filter = abs(factor_sent1_best_filter)

cap drop pc_abs_factor_sent1_best_filter
xtile pc_abs_factor_sent1_best_filter = abs_factor_sent1_best_filter, nq(4)
	tab pc_abs_factor_sent1_best_filter, m

compress
save "..\data\pct_abs_factor_sent1_best.dta", replace

*** art1: Number of articles with sent1 data

insheet using "..\data\news.csv", comma clear
compress
d
append using "..\data\features-1991-1996-missing-countries.dta", gen(source)

* generate sentiment by article also in its binary version, called sent1

gen sent1 = positive - negative
gen sent2 = positivetfidf - negativetfidf

gen sent1_bin = 0 if sent1!=. & sent1!=0
gen sent2_bin = 0 if sent2!=. & sent2!=0
replace sent1_bin = 1 if sent1>0
replace sent2_bin = 1 if sent2>0

gen art0 = 1 if words!=0
gen art1 = 1 if sent1_bin==0 | sent1_bin==1

cap order sent* art*, after(words)

* count topic-specific articles

cap ren *commodity* *commdty*
cap ren *monetary* *money*
cap ren *external* *extrnl*
cap ren *political* *polit*
cap ren *general* *gen*
cap ren *policy* *pol*
cap ren *performance* *perform*
cap ren *corporate* *corp*
cap ren *derivative* *deriv*
cap ren *market* *mkt*
cap ren *economic* *ecn*
cap ren *government* *govt*
cap ren *indicators* *ind*
cap ren *financial* *fin*
cap ren *payments* *pay*
cap ren *securities* *scrty*
cap ren *industrial* *indl*
cap ren *forex* *fx*
cap ren *pay* **
cap ren *mkts* *mkt*
cap ren *bond* **
cap ren *perform* **
cap ren *commdty* *comm*
cap ren *finance* *fin*

ds commmkt-tradeextrnl, varwidth(30)
loc topic_list = "`r(varlist)'"

foreach vv of varlist `topic_list' { 
	gen t_art0_`vv' = (`vv'==1 & art0!=.)
	gen t_art1_`vv' = (`vv'==1 & art1!=.)
}

* Non-local (global) article count

gen t_art1_nonlocal = art1 if local!=1

* panel of countries

gen DATE = date(date, "YMD", 2000), after(date)
format DATE %tdnn/dd/CCYY
cap drop date

gen ISO2 = "", after(country)
replace ISO2 = "AR" if country=="Argentina"
replace ISO2 = "BR" if country=="Brazil"
replace ISO2 = "CL" if country=="Chile"
replace ISO2 = "CN" if country=="China"
replace ISO2 = "FR" if country=="France"
replace ISO2 = "DE" if country=="Germany"
replace ISO2 = "GR" if country=="Greece"
replace ISO2 = "IN" if country=="India"
replace ISO2 = "ID" if country=="Indonesia"
replace ISO2 = "IE" if country=="Ireland"
replace ISO2 = "IT" if country=="Italy"
replace ISO2 = "JP" if country=="Japan"
replace ISO2 = "KR" if country=="Korea"
replace ISO2 = "MY" if country=="Malaysia"
replace ISO2 = "MX" if country=="Mexico"
replace ISO2 = "PE" if country=="Peru"
replace ISO2 = "PH" if country=="Philippines"
replace ISO2 = "PL" if country=="Poland"
replace ISO2 = "PT" if country=="Portugal"
replace ISO2 = "RU" if country=="Russia"
replace ISO2 = "ZA" if country=="South Africa"
replace ISO2 = "ES" if country=="Spain"
replace ISO2 = "TH" if country=="Thailand"
replace ISO2 = "TR" if country=="Turkey"
replace ISO2 = "US" if country=="United States"
tab ISO2, m

* Stylized facts about local vs. non-local (multi-country) news articles

* Nonlocal dummy
gen byte nonlocal = (local!=1) if !mi(local)

* Number of topics excluding local
egen ntopics = rowtotal(commmkt-tradeextrnl)
replace ntopics = ntopics - local
	tab ntopics, m

* Tonal words in each article
gen tonal = (positive + negative) * words

* Strong words in each article
gen nstrong = strong * words

* Summary statistics by local vs. multi-country news
bys nonlocal: su words, d
bys nonlocal: su ntopics, d
bys nonlocal: su tonal, d
bys nonlocal: su nstrong, d

* Controlling for length of article
reghdfe ntopics nonlocal words, absorb(ISO2) vce(cluster DATE ISO2)
reghdfe tonal nonlocal words, absorb(ISO2) vce(cluster DATE ISO2)
reghdfe nstrong nonlocal words, absorb(ISO2) vce(cluster DATE ISO2)

* Articles per country

levelsof ISO2, local(clist)
foreach cc in `clist' {
	cap drop c_art1_`cc'
	di "gen c_art1_`cc' = (art1!=.) & ISO2==`cc'"
	gen byte c_art1_`cc' = (art1!=.) & ISO2=="`cc'"
}

merge m:1 DATE using "..\data\pct_abs_factor_sent1_best.dta", nogen

keep ISO2 DATE art1 t_art1_* c_art1_* *factor_sent1_best_filter
ren art1 art1_total
ren t_art1_* art1_*
ren c_art1_* art1_*

tab pc_abs_factor_sent1_best_filter, m

ds DATE art1_total *factor_sent1_best_filter, not
foreach vv in `r(varlist)' {
	gen `vv'_tot = `vv'
	gen `vv'_pos = `vv' ///
		if (factor_sent1_best_filter > 0 & !mi(factor_sent1_best_filter))
	gen `vv'_neg = `vv' ///
		if (factor_sent1_best_filter < 0 & !mi(factor_sent1_best_filter))
}

ds art1_local art1_nonlocal art1_total
foreach vv in `r(varlist)' {
	forval i = 1(1)4 {
		cap drop `vv'_p`i'
		gen `vv'_p`i' = `vv' if pc_abs_factor_sent1_best_filter==`i'
	}
	cap drop `vv'
}

* generate daily sentiment dataset

keep DATE art1_*
collapse (sum) art1_*, by(DATE)
d
compress
save "..\data\art1.dta", replace

********************************************************************************
*** Figure 7. Multi-Country News and Global News Sentiment 

use "..\data\art1.dta", clear

tsset DATE
tsfill
cap drop *_long_*
cap drop *_best_local_*

fcollapse (sum) art1_*

keep *local* *nonlocal* *total*

gen byte constant = 1
reshape long art1_, i(constant) j(sign) string

split sign, gen(var) parse("_")
cap drop sign
cap drop constant

reshape wide art1_, i(var1) j(var2) string

ren var1 category
sort category
format art1_* %9.0fc

sort category
gen constant = 1

forval i = 1(1)4 {
	
	di "gen _tmp = art1_p`i' if category==total"
	gen _tmp = art1_p`i' if category=="total"
	
	cap drop art1_p`i'_total
	egen art1_p`i'_total = min(_tmp)
	cap drop _tmp
	
	cap drop shr_art1_p`i'
	di "gen shr_art1_p`i' = (art1_p`i' / art1_p`i'_total) * 100"
	gen shr_art1_p`i' = (art1_p`i' / art1_p`i'_total) * 100
}

forval i = 2(1)4 {
	
	cap drop D_shr_art1_p`i'
	di "gen D_shr_art1_p`i' = shr_art1_p`i' - shr_art1_p1"
	gen D_shr_art1_p`i' = shr_art1_p`i' - shr_art1_p1
}

keep if category=="nonlocal"

keep D_shr_art1_p2 D_shr_art1_p3 D_shr_art1_p4

gen constant = 1
reshape long D_shr_art1_, i(constant) j(pctile) string

replace constant = _n + 1
lab var D_shr_art1_ "Change in the share of articles (%)"

graph bar D_shr_art1_, over(constant, label(labsize(medlarge))) /// 
	graphregion(color(white) margin(medlarge)) ///
	b1title("Quartiles", size(medlarge)) ///
	ytitle("Change in the share of multi-country news (%)", size(medlarge)) ///
	ylabel(#3) title("", size(medlarge)) ///
	ylabel(, labsize(medlarge) format(%9.1fc)) // 0(.2).8
graph export ///
	"..\results\figure_7.pdf" ///
	, as(pdf) replace 
graph close

cap log close
