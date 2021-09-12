cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_a8_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Figure A8. Definition of Global Bull and Bear Markets ***
********************************************************************************

* Sentiment during good vs bad times
* Appendix: Figure showing periods of global bull or bear markets

use "..\data\recessions.dta", clear
cap ren *index *DJ_US

drop if year < 1991
drop if year > 2015
tsset ym

replace hpres_DJ_US = . if hpres_DJ_US==0
replace hpres_DJ_GL = . if hpres_DJ_GL==0

gen zero = 0

twoway (tsline hpres_DJ_GL if tin(1991m1, 2015m12) ///
		, lpattern(solid) lwidth(medium)) || ///
	(tsline zero if tin(1991m1, 2015m12) ///
		, color(black) lwidth(thin)) ///
	, graphregion(color(white)) ///
	xlabel(, format(%tm) labsize(medlarge)) ylabel(, labsize(medlarge)) ///
	ytitle("Dow Jones World Index (HP-detrended)", size(medlarge)) xtitle("") ///
	legend(off) 
graph export "..\results\figure_a8.pdf", as(pdf) replace 
cap graph close

cap log close
