cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_8_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure 8. Global News Sentiment and News Coverage ***
********************************************************************************

use "..\data\art1.dta", clear

tsset DATE
tsfill
cap drop *_long_*
cap drop *_best_local_*

fcollapse (sum) art1_*

gen constant = 1
reshape long art1_, i(constant) j(sign) string

split sign, gen(var) parse("_")
cap drop sign
cap drop constant

drop if mi(var2)

reshape wide art1_, i(var1) j(var2) string

ren var1 category
sort category
format art1_* %9.0fc

gen group = ""
replace group = "country" in 1/25
replace group = "topic" in 26/43
replace group = "total" if category=="total"

order group category art1_tot art1_pos art1_neg

replace category = "All Topics" if category=="alltopics"
replace category = "Commodity/Financial" if category=="commfinmkt"
replace category = "Commodity" if category=="commmkt"
replace category = "Corporate/Industrial" if category=="corpindl"
replace category = "Debt/Bond" if category=="debtmkt"
replace category = "Derivative Securities" if category=="derivscrty"
replace category = "Economic" if category=="ecn"
replace category = "Economic/Indicators" if category=="ecnind"
replace category = "Monetary Policy" if category=="ecnmoneypol"
replace category = "Equity" if category=="equitymkt"
replace category = "Government Finance" if category=="govtfin"
replace category = "Local" if category=="local"
replace category = "Money/Forex" if category=="moneyfxmkt"
replace category = "Political/General" if category=="politgen"
replace category = "Table" if category=="table"
replace category = "Tables" if category=="tables"
replace category = "Trade/External" if category=="tradeextrnl"
replace category = "Non-Local" if category=="nonlocal"

drop if category=="Table"
drop if category=="Tables"

sort group category

foreach tt in tot pos neg {

	cap drop art1_`tt'_category
	bys group: egen art1_`tt'_category = total(art1_`tt')

	cap drop shr_art1_`tt'
	gen shr_art1_`tt' = (art1_`tt' / art1_`tt'_category) * 100
}

sort group category
encode category, gen(CATEGORY)

* Delta shares beween global sent > 0 & total sample

gen art1_pos_tot = art1_pos - art1_tot
gen art1_tot_neg = art1_tot - art1_neg

gen D_shr_art1_pos_tot = shr_art1_pos - shr_art1_tot
gen D_shr_art1_tot_neg = shr_art1_tot - shr_art1_neg

bys group (D_shr_art1_pos_tot): gen D_rnk_shr_art1_pos_tot = _n
bys group (D_shr_art1_tot_neg): gen D_rnk_shr_art1_tot_neg = _n

cap drop color
gen color = "black"

replace color = "red" if category=="AR" // Argentina
replace color = "red" if category=="BR" // Brazil
replace color = "red" if category=="CL" // Chile
replace color = "red" if category=="CN" // China
replace color = "green" if category=="DE" // Germany
replace color = "green" if category=="ES" // Spain
replace color = "green" if category=="FR" // France
replace color = "green" if category=="GR" // Greece
replace color = "red" if category=="ID" // Indonesia
replace color = "green" if category=="IE" // Ireland
replace color = "red" if category=="IN" // India
replace color = "green" if category=="IT" // Italy
replace color = "green" if category=="JP" // Japan
replace color = "green" if category=="KR" // Korea
replace color = "red" if category=="MX" // Mexico
replace color = "red" if category=="MY" // Malaysia
replace color = "red" if category=="PE" // Peru
replace color = "red" if category=="PH" // Philippines
replace color = "red" if category=="PL" // Poland
replace color = "green" if category=="PT" // Portugal
replace color = "red" if category=="RU" // Russia
replace color = "red" if category=="TH" // Thailand
replace color = "red" if category=="TR" // Turkey
replace color = "green" if category=="US" // United States
replace color = "red" if category=="ZA" // South Africa

*replace color = "green" if category=="All Topics"
replace color = "red" if category=="Commodity/Financial"
replace color = "red" if category=="Commodity"
*replace color = "green" if category=="Corporate/Industrial"
replace color = "red" if category=="Debt/Bond"
replace color = "red" if category=="Derivative Securities"
replace color = "green" if category=="Economic"
replace color = "green" if category=="Economic/Indicators"
replace color = "green" if category=="Monetary Policy"
replace color = "red" if category=="Equity"
replace color = "green" if category=="Government Finance"
*replace color = "green" if category=="Local"
replace color = "red" if category=="Money/Forex"
replace color = "green" if category=="Political/General"
*replace color = "green" if category=="Table"
*replace color = "green" if category=="Tables"
replace color = "green" if category=="Trade/External"

replace D_shr_art1_pos_tot = D_shr_art1_pos_tot * 100

* Figure 8. Global News Sentiment and News Coverage

* Figure 8.A. Locations
preserve

keep if group=="country"

cap drop CATEGORY
gsort -D_shr_art1_pos_tot
gen CATEGORY = _n
labmask CATEGORY, values(category)

twoway (bar D_shr_art1_pos_tot CATEGORY ///
		if group=="country" & color=="green", color(gs2)) ///
	(bar D_shr_art1_pos_tot CATEGORY ///
		if group=="country" & color=="red", color(gs10)), ///
	ytitle("Change in the share of articles (%)", size(medlarge)) ///
	ylabel(#3) graphregion(color(white) margin(medlarge)) title("") /// 
	ylabel(-80(40)80, labsize(medlarge) format(%9.0fc)) /// 
	xlabel(1(1)25, labsize(small) valuelabel) xtitle("") /// 
	legend(order(1 "AE" 2 "EMDE") ///
		symxsize(*.7) symysize(*.7) size(medlarge) row(1))
graph export ///
	"..\results\figure_8a.pdf" ///
	, as(pdf) replace 
graph close

restore

* Figure 8.B. Topics
preserve

keep if group=="topic"

cap drop CATEGORY
gsort -D_shr_art1_pos_tot
gen CATEGORY = _n
labmask CATEGORY, values(category)

twoway (bar D_shr_art1_pos_tot CATEGORY ///
		if group=="topic" & color=="green", color(black)) ///
	(bar D_shr_art1_pos_tot CATEGORY ///
		if group=="topic" & color=="red", color(gs8)) ///
	(bar D_shr_art1_pos_tot CATEGORY ///
		if group=="topic" & color=="black", color(gs14)), ///
	ytitle("Change in the share of articles (%)", size(medlarge)) ///
	ylabel(#3) graphregion(color(white) margin(medlarge)) title("") /// 
	ylabel(-50(50)50, labsize(medlarge) format(%9.0fc)) /// 
	xlabel(1(1)15, labsize(medium) angle(vertical) valuelabel) xtitle("") /// 
	legend(order(1 "Economic" 2 "Financial" 3 "Other") ///
		symxsize(*.7) symysize(*.7) size(medlarge) row(1))
graph export ///
	"..\results\figure_8b.pdf" ///
	, as(pdf) replace 
graph close

restore

cap log close
