cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\news_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** NEWS SENTIMENT DATA ***
********************************************************************************

insheet using "..\data\features-1991-1996-missing-countries.csv", comma clear
compress
drop v1
d
save "..\data\features-1991-1996-missing-countries.dta", replace

insheet using "..\data\news.csv", comma clear
compress
d
append using "..\data\features-1991-1996-missing-countries.dta", gen(source)

****************************************
* Baseline News Sentiment (positive or negative as share of all words)

* generate sentiment by article, called sent1
gen sent1 = positive - negative
gen sent2 = positivetfidf - negativetfidf

* also in its binary version
gen sent1_bin = 0 if sent1!=. & sent1!=0
gen sent2_bin = 0 if sent2!=. & sent2!=0
replace sent1_bin = 1 if sent1 > 0
replace sent2_bin = 1 if sent2 > 0

* Count number of articles with sentiment
gen art0 = 1 if words!=0
gen art1 = 1 if sent1_bin==0 | sent1_bin==1
gen art2 = 1 if sent2_bin==0 | sent2_bin==1
cap order sent* art*, after(words)

****************************************
* Tonal News sentiment (positive or negative as share of tonal words)

* Share of positive or negative out of tonal words (positive + negative)
* = x * words / (positive * words + negative * words)
gen positiveTonal = positive / (positive + negative)
gen negativeTonal = negative / (positive + negative)
gen positivetfidfTonal = positivetfidf / (positivetfidf + negativetfidf)
gen negativetfidfTonal = negativetfidf / (positivetfidf + negativetfidf)

* generate sentiment by article, called sent1
gen sentTonal1 = positiveTonal - negativeTonal
gen sentTonal2 = positivetfidfTonal - negativetfidfTonal

* also in its binary version
gen sentTonal1_bin = 0 if sentTonal1!=. & sentTonal1!=0
gen sentTonal2_bin = 0 if sentTonal2!=. & sentTonal2!=0
replace sentTonal1_bin = 1 if sentTonal1 > 0 & !mi(sentTonal1)
replace sentTonal2_bin = 1 if sentTonal2 > 0 & !mi(sentTonal2)

* Count number of articles with sentiment
gen artTonal0 = 1 if words!=0
gen artTonal1 = 1 if sentTonal1_bin==0 | sentTonal1_bin==1
gen artTonal2 = 1 if sentTonal2_bin==0 | sentTonal2_bin==1
cap order sent* art*, after(words)

****************************************
* Uncertainty sentiment

rename uncertainty sentu1
rename uncertaintytfidf sentu2
gen artu1 = 1 if sentu1!=0
gen artu2 = 1 if sentu2!=0
cap order sent* art*, after(words)

****************************************
* Count topic-specific articles

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
	gen t_art2_`vv' = (`vv'==1 & art2!=.)
	gen t_artTonal0_`vv' = (`vv'==1 & artTonal0!=.)
	gen t_artTonal1_`vv' = (`vv'==1 & artTonal1!=.)
	gen t_artTonal2_`vv' = (`vv'==1 & artTonal2!=.)
}

* generate sentiment by topic
foreach vv of varlist `topic_list' { 
	gen t_sent1_`vv' = positive - negative if `vv'==1
	gen t_sent2_`vv' = positivetfidf - negativetfidf if `vv'==1
	gen t_sentTonal1_`vv' = positiveTonal - negativeTonal if `vv'==1
	gen t_sentTonal2_`vv' = positivetfidfTonal - negativetfidfTonal if `vv'==1
}
ds t_sent*
global vlist_topic "`r(varlist)'"

* Aggregate baseline sentiment excluding topics with no signal
gen sent1_best = positive - negative ///
	if !(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen sent2_best = positivetfidf - negativetfidf ///
	if !(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen art1_best = (art1!=.) & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen art2_best = (art2!=.) & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)

* Local baseline sentiment excluding topics with no signal
gen t_sent1_best_local = positive - negative if local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_sent2_best_local = positivetfidf - negativetfidf if local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_art1_best_local = (art1!=.) & local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_art2_best_local = (art2!=.) & local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)

* Non-Local baseline sentiment excluding topics with no signal
gen t_sent1_best_nonLocal = positive - negative if local==0 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_sent2_best_nonLocal = positivetfidf - negativetfidf if local==0 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_art1_best_nonLocal = (art1!=.) & local==0 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_art2_best_nonLocal = (art2!=.) & local==0 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)

* Aggregate uncertainty excluding topics with no signal
gen sentu1_best = sentu1 ///
	if !(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen sentu2_best = sentu2 ///
	if !(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen artu1_best = (art1!=.) & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen artu2_best = (art2!=.) & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)

* Local uncertainty excluding topics with no signal
gen t_sentu1_best_local = sentu1 if local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_sentu2_best_local = sentu2 if local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_artu1_best_local = (art1!=.) & local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_artu2_best_local = (art2!=.) & local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)

* Aggregate tonal sentiment excluding topics with no signal
gen sentTonal1_best = positiveTonal - negativeTonal ///
	if !(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen sentTonal2_best = positivetfidfTonal - negativetfidfTonal ///
	if !(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen artTonal1_best = (artTonal1!=.) & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen artTonal2_best = (artTonal2!=.) & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)

* Local tonal sentiment excluding topics with no signal
gen t_sentTonal1_best_local = positiveTonal - negativeTonal ///
	if local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_sentTonal2_best_local = positivetfidfTonal - negativetfidfTonal ///
	if local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_artTonal1_best_local = (artTonal1!=.) & ///
	local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)
gen t_artTonal2_best_local = (artTonal2!=.) & ///
	local==1 & ///
	!(table==1 | tables==1 | tradeextrnl==1 | ecnind==1 | debtmkt==1)

* generate local/non-local sentiment by topic
foreach vv of varlist `topic_list' { 
	gen t_sent1_`vv'_loc0 = positive - negative if `vv'==1 & local==0
	gen t_sent1_`vv'_loc1 = positive - negative if `vv'==1 & local==1
}
foreach vv of varlist `topic_list' { 
	gen t_art0_`vv'_loc0 = (`vv'==1 & art0!=.) & local==0
	gen t_art0_`vv'_loc1 = (`vv'==1 & art0!=.) & local==1
	gen t_art1_`vv'_loc0 = (`vv'==1 & art1!=.) & local==0
	gen t_art1_`vv'_loc1 = (`vv'==1 & art1!=.) & local==1
}

* generate daily sentiment dataset
fcollapse ///
	(mean) positive negative positivetfidf negativetfidf ///
		sent1 sent2 sent1_best sent2_best ///
		t_sent1_* t_sent2_* /// 
		positiveTonal negativeTonal positivetfidfTonal negativetfidfTonal ///
		sentTonal1 sentTonal2 sentTonal1_best sentTonal2_best ///
		t_sentTonal1_* t_sentTonal2_* /// 
		sentu1 sentu2 sentu1_best sentu2_best ///
		t_sentu1_best_local t_sentu2_best_local ///
	(sum) art0 art1 art2 ///
		t_art0_* t_art1_* t_art2_* /// 
		art1_best art2_best ///
		artTonal0 artTonal1 artTonal2 ///
		t_artTonal0_* t_artTonal1_* t_artTonal2_* /// 
		artTonal1_best artTonal2_best ///
		artu1 artu2 ///
		artu1_best artu2_best ///
		t_artu1_best_local t_artu2_best_local ///
	, by(country date)

label variable positive "positive fraction"
label variable negative "negative fraction"
label variable positivetfidf "positive fraction (fidf)"
label variable negativetfidf "negative fraction (fidf)"
label variable sent1 "net positive fraction"
label variable sent2 "net positive fraction with weight TFIDF"
label variable art0 "count of total articles per day"
label variable art1 "count of effective articles per day"
label variable art2 "count of effective articles per day with weight"
foreach vv in `topic_list' {
	label variable t_art1_`vv' "count of articles with a tone and tagged `vv'"
	label variable t_art2_`vv' "count of articles with a tone and tagged `vv', weight TFIDF"
}
cap order t_art*_*, last
format t_art*_* %12.3g

* Panel of countries
gen DATE = date(date, "YMD", 2000), after(date)
format DATE %tdnn/dd/CCYY
cap drop date
gen iso = "", after(country)
replace iso = "AR" if country=="Argentina"
replace iso = "BR" if country=="Brazil"
replace iso = "CL" if country=="Chile"
replace iso = "CN" if country=="China"
replace iso = "FR" if country=="France"
replace iso = "DE" if country=="Germany"
replace iso = "GR" if country=="Greece"
replace iso = "IN" if country=="India"
replace iso = "ID" if country=="Indonesia"
replace iso = "IE" if country=="Ireland"
replace iso = "IT" if country=="Italy"
replace iso = "JP" if country=="Japan"
replace iso = "KR" if country=="Korea"
replace iso = "MY" if country=="Malaysia"
replace iso = "MX" if country=="Mexico"
replace iso = "PE" if country=="Peru"
replace iso = "PH" if country=="Philippines"
replace iso = "PL" if country=="Poland"
replace iso = "PT" if country=="Portugal"
replace iso = "RU" if country=="Russia"
replace iso = "ZA" if country=="South Africa"
replace iso = "ES" if country=="Spain"
replace iso = "TH" if country=="Thailand"
replace iso = "TR" if country=="Turkey"
replace iso = "US" if country=="United States"
	tab iso, m

kountry iso, from(iso2c) to(imfn)
ren _IMFN_ weo
order weo, before(iso)

xtset weo DATE
tsfill
compress
d
save "..\data\news.dta", replace

****************************************************
* Country name - country code crosswalk file

use "..\data\news.dta", clear
keep country weo iso
keep if iso!=""
duplicates drop
compress
save "..\data\country.dta", replace

****************************************************
* Updated lags of sentiment filling in missing news

use "..\data\news.dta", clear

xtset weo DATE

* Rescale variables (mean 0, sd 1)
ds positive negative positivetfidf negativetfidf ///
	sent1 sent2 sent1_best sent2_best ///
	t_sent1_* t_sent2_* /// 
	positiveTonal negativeTonal positivetfidfTonal negativetfidfTonal ///
	sentTonal1 sentTonal2 sentTonal1_best sentTonal2_best ///
	t_sentTonal1_* t_sentTonal2_* /// 
	sentu1 sentu2 sentu1_best sentu2_best ///
	t_sentu1_best_local t_sentu2_best_local
	
foreach vv in `r(varlist)' {
	cap ren `vv' _raw
	cap drop _mean _sd _raw
	bys weo: egen _mean = mean(_raw)
	bys weo: egen _sd = sd(_raw)
	bys weo: gen `vv' = (_raw - _mean) / _sd
	cap drop _mean _sd _raw
}

* Create lags
ds positive negative positivetfidf negativetfidf ///
	sent1 sent2 sent1_best sent2_best ///
	t_sent1_* t_sent2_* /// 
	positiveTonal negativeTonal positivetfidfTonal negativetfidfTonal ///
	sentTonal1 sentTonal2 sentTonal1_best sentTonal2_best ///
	t_sentTonal1_* t_sentTonal2_* /// 
	sentu1 sentu2 sentu1_best sentu2_best ///
	t_sentu1_best_local t_sentu2_best_local ///
	art0 art1 art2 art1_best art2_best ///
	t_art1_* t_art2_* /// 
	artTonal0 artTonal1 artTonal2 ///
	artTonal1_best artTonal2_best ///
	t_artTonal1_* t_artTonal2_* /// 
	artu1 artu2 artu1_best artu2_best ///
	t_artu1_best_local t_artu2_best_local 
	
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
		save "..\data\lags_`vv'.dta", replace
	restore
}

cap log close
