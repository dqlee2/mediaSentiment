cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\dfm_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** DYNAMIC FACTOR MODEL FOR GLOBAL SENTIMENT ***
********************************************************************************

use "..\data\news.dta", clear

d sent1_best
keep weo DATE sent1_best
xtset weo DATE
tsfill

reshape wide sent1_best, i(DATE) j(weo)
tsset DATE
tsfill

keep if year(DATE) > 1990 & year(DATE) < 2016

ds sent1_best*
loc vlist = "`r(varlist)'"
foreach vv in `vlist' {
	di "count if !mi(`vv')"
	count if !mi(`vv') & `vv'!=0
	replace `vv' = 0 if mi(`vv')
}
compress

dfactor ((`vlist') = , noconstant) (f = , ar(1/8))

ereturn list
predict fhat_sent1_best, y
predict factor_sent1_best_onestep if e(sample), factor smethod(onestep)
predict factor_sent1_best_smooth if e(sample), factor smethod(smooth)
predict factor_sent1_best_filter if e(sample), factor smethod(filter)

compress
save "..\data\factor_sent1_best.dta", replace

********************************************************************************
*** DYNAMIC FACTOR MODEL FOR GLOBAL TF-IDF SENTIMENT ***
********************************************************************************

use "..\data\news.dta", clear

d sent2_best
keep weo DATE sent2_best
xtset weo DATE
tsfill

reshape wide sent2_best, i(DATE) j(weo)
tsset DATE
tsfill

keep if year(DATE) > 1990 & year(DATE) < 2016

ds sent2_best*
loc vlist = "`r(varlist)'"
foreach vv in `vlist' {
	di "count if !mi(`vv')"
	count if !mi(`vv') & `vv'!=0
	replace `vv' = 0 if mi(`vv')
}
compress

dfactor ((`vlist') = , noconstant) (f = , ar(1/2))

ereturn list
predict fhat_sent2_best, y
predict factor_sent2_best_onestep if e(sample), factor smethod(onestep)
predict factor_sent2_best_smooth if e(sample), factor smethod(smooth)
predict factor_sent2_best_filter if e(sample), factor smethod(filter)

compress
save "..\data\factor_sent2_best.dta", replace

********************************************************************************
*** DYNAMIC FACTOR MODEL FOR GLOBAL SENTIMENT (NEGATIVE TONAL WORDS ONLY) ***
********************************************************************************

use "..\data\news.dta", clear

d negativeTonal
keep weo DATE negativeTonal
xtset weo DATE
tsfill

reshape wide negativeTonal, i(DATE) j(weo)
tsset DATE
tsfill

keep if year(DATE) > 1990 & year(DATE) < 2016

ds negativeTonal*
loc vlist = "`r(varlist)'"
foreach vv in `vlist' {
	di "count if !mi(`vv')"
	count if !mi(`vv') & `vv'!=0
	replace `vv' = 0 if mi(`vv')
}
compress

dfactor ((`vlist') = , ) (f = , ar(1/2))

ereturn list
predict fhat_negativeTonal, y
predict factor_negativeTonal if e(sample), factor smethod(filter)

compress
save "..\data\factor_negativeTonal.dta", replace

********************************************************************************
*** DYNAMIC FACTOR MODEL FOR GLOBAL SENTIMENT (NEGATIVE WORDS ONLY) ***
********************************************************************************

use "..\data\news.dta", clear

d negative
keep weo DATE negative
xtset weo DATE
tsfill

reshape wide negative, i(DATE) j(weo)
tsset DATE
tsfill

keep if year(DATE) > 1990 & year(DATE) < 2016

ds negative*
loc vlist = "`r(varlist)'"
foreach vv in `vlist' {
	di "count if !mi(`vv')"
	count if !mi(`vv') & `vv'!=0
	replace `vv' = 0 if mi(`vv')
}
compress

dfactor ((`vlist') = , ) (f = , ar(1/2))

ereturn list
predict fhat_negative, y
predict factor_negative_onestep if e(sample), factor smethod(onestep)
predict factor_negative_smooth if e(sample), factor smethod(smooth)
predict factor_negative_filter if e(sample), factor smethod(filter)

compress
save "..\data\factor_negative.dta", replace

********************************************************************************
*** DYNAMIC FACTOR MODEL FOR GLOBAL UNCERTAINTY NEWS ***
********************************************************************************

use "..\data\news.dta", clear

d sentu1_best
keep weo DATE sentu1_best
xtset weo DATE
tsfill

reshape wide sentu1_best, i(DATE) j(weo)
tsset DATE
tsfill

keep if year(DATE) > 1990 & year(DATE) < 2016

ds sentu1_best*
loc vlist = "`r(varlist)'"
foreach vv in `vlist' {
	di "count if !mi(`vv')"
	count if !mi(`vv') & `vv'!=0
	replace `vv' = 0 if mi(`vv')
}
compress

dfactor ((`vlist') = , noconstant) (f = , ar(1/2))

ereturn list
predict fhat_sentu1_best, y
predict factor_sentu1_best_onestep if e(sample), factor smethod(onestep)
predict factor_sentu1_best_smooth if e(sample), factor smethod(smooth)
predict factor_sentu1_best_filter if e(sample), factor smethod(filter)

compress
save "..\data\factor_sentu1_best.dta", replace

cap log close
