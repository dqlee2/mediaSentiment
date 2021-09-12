cap log close
loc logdate = string(d(`c(current_date)'), "%dNDY")
log using "..\log\figure_1_`logdate'.txt", text append
version 14
set linesize 225

********************************************************************************
*** Event study around larve deviations in equity returns
********************************************************************************

use "..\data\regsample.dta", clear
xtset weo DATE
	
* Events defined by large movements in stock prices

foreach dd in 3 4 5 {
	
	cap drop mean_F0_index sd_F0_index
	bys weo: egen mean_F0_index = mean(F0_index)
	bys weo: egen sd_F0_index = sd(F0_index)
	
	gen byte F0_index_chigh`dd'sd = ///
		(F0_index > mean_F0_index + `dd' * sd_F0_index) if !mi(F0_index)
	gen byte F0_index_clow`dd'sd = ///
		(F0_index < mean_F0_index - `dd' * sd_F0_index) if !mi(F0_index)
	gen byte F0_index_cabs`dd'sd = ///
		(F0_index > mean_F0_index + `dd' * sd_F0_index) | ///
		(F0_index < mean_F0_index - `dd' * sd_F0_index) if !mi(F0_index)

	tab F0_index_*high`dd'sd, m
	tab F0_index_*low`dd'sd, m
	
	preserve
	
	keep weo DATE F0_index_*`dd'sd
	cap drop time_*
	
	foreach ss in chigh clow {
		
		gen time_`ss'`dd'sd = .
		
		levelsof weo if F0_index_`ss'`dd'sd==1, local(clist)
		foreach cc in `clist' {
			
			levelsof DATE if F0_index_`ss'`dd'sd==1 & weo==`cc', local(tlist)
			foreach tt in `tlist' {
				
				qui replace time_`ss'`dd'sd = 0 if DATE==`tt' & weo==`cc'
				
				forval ii = 1(1)10 {
					
					qui replace time_`ss'`dd'sd = `ii' if ///
						mi(time_`ss'`dd'sd) & DATE==(`tt' + `ii') & weo==`cc'
					
					qui replace time_`ss'`dd'sd = -`ii' ///
						if mi(time_`ss'`dd'sd) & DATE==(`tt' - `ii') & weo==`cc'
				}
			}
		}
	}
	tempfile time_`dd'sd
	save `time_`dd'sd'
	
	restore
	
	merge 1:1 weo DATE using `time_`dd'sd', nogen
	
}

* construct z-scores

ds F0_index sent1_best t_sent1_best_local factor_sent1_best_filter
foreach vv in `r(varlist)' {
	
	cap drop mean_`vv' 
	bys weo: egen mean_`vv' = mean(`vv')
	
	cap drop sd_`vv' 
	bys weo: egen sd_`vv' = sd(`vv')
	
	cap drop zscore_`vv'
	gen zscore_`vv' = (`vv' - mean_`vv') / sd_`vv'
}

* Major events

gen byte time_gfc = 0 if tin(15sep2008, 15sep2008) // Lehman brothers file chapter 11 bankruptcy
gen byte time_china2015 = 0 if tin(13jun2015, 13jun2015) // chinese stock market turbulence
gen byte time_sep11 = 0 if tin(11sep2001, 11sep2001) // 9/11
gen byte time_argentina2001 = 0 if tin(29nov2001, 29nov2001) // Argentina 2001 crisis. Major investors began to withdraw their deposits from banks, causing the collapse of the Argentinian banking system. 

foreach ss in gfc china2015 sep11 argentina2001 {
	levelsof DATE if time_`ss'==0, local(tlist)
	foreach tt in `tlist' {
		forval ii = 1(1)10 {
			qui replace time_`ss' = `ii' if DATE==(`tt' + `ii')
			qui replace time_`ss' = -`ii' if DATE==(`tt' - `ii')
		}
	}
}
cap gen byte zero = 0

twoway (line zscore_sent1_best time_china2015 ///
		if !mi(time_china2015) & iso=="CN" ///
		, lcolor(black) lpattern(solid) lwidth(thick)) /// 
	(line zero time_china2015, lcolor(black) lw(medium)), ///
	title(, color(black) size(medium)) ///
	xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
	ytitle("News sentiment index (z-score)", size(large)) ///
	xtitle("Event time (Days)", size(large)) ///
	ylabel(, labsize(large)) xlabel(, labsize(large)) ///
	graphregion(color(white)) plotregion(color(white)) ///
	legend(off) /// 
	title("") 
graph export "..\results\figure_1_china2015_CN.pdf", as(pdf) replace 
cap graph close

twoway (line zscore_sent1_best time_gfc ///
		if !mi(time_gfc) & iso=="US" ///
		, lcolor(black) lpattern(solid) lwidth(thick)) /// 
	(line zero time_gfc, lcolor(black) lw(medium)), ///
	title(, color(black) size(medium)) ///
	xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
	ytitle("News sentiment index (z-score)", size(large)) ///
	xtitle("Event time (Days)", size(large)) ///
	ylabel(, labsize(large)) xlabel(, labsize(large)) ///
	graphregion(color(white)) plotregion(color(white)) ///
	legend(off) /// 
	title("")
graph export "..\results\figure_1_gfc_US.pdf", as(pdf) replace 
cap graph close

twoway (line zscore_sent1_best time_sep11 ///
		if !mi(time_sep11) & iso=="US" ///
		, lcolor(black) lpattern(solid) lwidth(thick)) /// 
	(line zero time_sep11, lcolor(black) lw(medium)), ///
	title(, color(black) size(medium)) ///
	xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
	ytitle("News sentiment index (z-score)", size(large)) ///
	xtitle("Event time (Days)", size(large)) ///
	ylabel(, labsize(large)) xlabel(, labsize(large)) ///
	graphregion(color(white)) plotregion(color(white)) ///
	legend(off) /// 
	title("")
graph export "..\results\figure_1_sep11_US.pdf", as(pdf) replace 
cap graph close

twoway (line zscore_sent1_best time_argentina2001 ///
		if !mi(time_argentina2001) & iso=="AR" ///
		, lcolor(black) lpattern(solid) lwidth(thick)) /// 
	(line zero time_argentina2001, lcolor(black) lw(medium)), ///
	title(, color(black) size(medium)) ///
	xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
	ytitle("News sentiment index (z-score)", size(large)) ///
	xtitle("Event time (Days)", size(large)) ///
	ylabel(, labsize(large)) xlabel(, labsize(large)) ///
	graphregion(color(white)) plotregion(color(white)) ///
	legend(off) /// 
	title("")
graph export "..\results\figure_1_argentina2001_AR.pdf", as(pdf) replace 
cap graph close

* Average over Events defined by large movements in stock prices

foreach dd in 3 4 5 {
foreach ss in chigh clow {
	
	* Average over all countries
	
	preserve
	
		keep weo DATE F0_index_*`dd'sd time_* zscore_* ///
			F0_index sent1_best t_sent1_best_local factor_sent1_best_filter
			
		drop if mi(time_`ss'`dd'sd)
		
		collapse ///
			(mean) mean_F0_index = F0_index ///
				mean_sent1_best = sent1_best ///
				mean_t_sent1_best_local = t_sent1_best_local ///
				mean_glb_sent1_best = factor_sent1_best_filter ///
				mean_zscore_F0_index = zscore_F0_index ///
				mean_zscore_sent1_best = zscore_sent1_best ///
				mean_zscore_t_sent1_best_local = zscore_t_sent1_best_local ///
				mean_zscore_glb_sent1_best = zscore_factor_sent1_best_filter ///
			(sd) sd_F0_index = F0_index ///
				sd_sent1_best = sent1_best ///
				sd_t_sent1_best_local = t_sent1_best_local ///
				sd_glb_sent1_best = factor_sent1_best_filter ///
			, by(time_`ss'`dd'sd)
			
		compress
		save "..\results\figure_1_`ss'`dd'sd.dta", replace
		
		tsset time_`ss'`dd'sd
		gen zero = 0
		
		twoway (line mean_zscore_sent1_best time_`ss'`dd'sd ///
					, lcolor(black) lpattern(solid) lwidth(thick)) /// 
				(line zero time_`ss'`dd'sd, lcolor(black) lw(medium)), ///
				title(, color(black) size(medium)) ///
				xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
				ytitle("News sentiment index (z-score)", size(large)) ///
				xtitle("Event time (Days)", size(large)) ///
				ylabel(, labsize(large)) xlabel(, labsize(large)) ///
				graphregion(color(white)) plotregion(color(white)) ///
				legend(off) /// 
				title("")
		graph export "..\results\figure_1_`ss'`dd'sd.pdf", as(pdf) replace 
		cap graph close
		
	restore
}
}


* Average over major events

foreach ss in gfc china2015 sep11 argentina2001 {
	
	* Average over all countries
	preserve
	
		keep weo DATE time_* zscore_* ///
			F0_index sent1_best t_sent1_best_local factor_sent1_best_filter
			
		drop if mi(time_`ss')
		
		collapse ///
			(mean) mean_F0_index = F0_index ///
				mean_sent1_best = sent1_best ///
				mean_t_sent1_best_local = t_sent1_best_local ///
				mean_glb_sent1_best = factor_sent1_best_filter ///
				mean_zscore_F0_index = zscore_F0_index ///
				mean_zscore_sent1_best = zscore_sent1_best ///
				mean_zscore_t_sent1_best_local = zscore_t_sent1_best_local ///
				mean_zscore_glb_sent1_best = zscore_factor_sent1_best_filter ///
			(sd) sd_F0_index = F0_index ///
				sd_sent1_best = sent1_best ///
				sd_t_sent1_best_local = t_sent1_best_local ///
				sd_glb_sent1_best = factor_sent1_best_filter ///
			, by(time_`ss')
			
		compress
		save "..\results\figure_1_`ss'.dta", replace
		
		tsset time_`ss'
		gen zero = 0
		
		twoway (line mean_zscore_sent1_best time_`ss' ///
					, lcolor(black) lpattern(solid) lwidth(thick)) /// 
				(line zero time_`ss', lcolor(black) lw(medium)), ///
				title(, color(black) size(medium)) ///
				xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
				ytitle("News sentiment index (z-score)", size(large)) ///
				xtitle("Event time (Days)", size(large)) ///
				ylabel(, labsize(large)) xlabel(, labsize(large)) ///
				graphregion(color(white)) plotregion(color(white)) ///
				legend(off) /// 
				title("")
		graph export "..\results\figure_1_`ss'.pdf", as(pdf) replace 
		cap graph close
		
	restore
	
	* Average over country groups
	
	foreach cg in AE {
	
	preserve
	
		keep weo DATE `cg' time_* zscore_* ///
			F0_index sent1_best t_sent1_best_local factor_sent1_best_filter
			
		drop if mi(time_`ss')
		
		collapse ///
			(mean) mean_F0_index = F0_index ///
				mean_sent1_best = sent1_best ///
				mean_t_sent1_best_local = t_sent1_best_local ///
				mean_glb_sent1_best = factor_sent1_best_filter ///
				mean_zscore_F0_index = zscore_F0_index ///
				mean_zscore_sent1_best = zscore_sent1_best ///
				mean_zscore_t_sent1_best_local = zscore_t_sent1_best_local ///
				mean_zscore_glb_sent1_best = zscore_factor_sent1_best_filter ///
			(sd) sd_F0_index = F0_index ///
				sd_sent1_best = sent1_best ///
				sd_t_sent1_best_local = t_sent1_best_local ///
				sd_glb_sent1_best = factor_sent1_best_filter ///
			, by(`cg' time_`ss')
			
		compress
		save "..\results\figure_1_`ss'_`cg'.dta", replace
		
		xtset `cg' time_`ss'
		gen zero = 0
		
		* Sentiment index AE
		
		twoway (line mean_zscore_sent1_best time_`ss' ///
					if `cg'==1, lcolor(black) lpattern(solid) lwidth(thick)) /// 
				(line zero time_`ss' if `cg'==1, lcolor(black) lw(medium)), ///
				title(, color(black) size(medium)) ///
				xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
				ytitle("News sentiment index (z-score)", size(large)) ///
				xtitle("Event time (Days)", size(large)) ///
				ylabel(, labsize(large)) xlabel(, labsize(large)) ///
				graphregion(color(white)) plotregion(color(white)) ///
				legend(off) /// 
				title("")
		graph export "..\results\figure_1_`ss'_`cg'1.pdf", as(pdf) replace 
		cap graph close
			
		* Sentiment index EM
		
		twoway (line mean_zscore_sent1_best time_`ss' ///
					if `cg'==0, lcolor(black) lpattern(solid) lwidth(thick)) /// 
				(line zero time_`ss' if `cg'==0, lcolor(black) lw(medium)), ///
				title(, color(black) size(medium)) ///
				xline(0, lcolor(red) lwidth(medium) lpattern(dash)) ///
				ytitle("News sentiment index (z-score)", size(large)) ///
				xtitle("Event time (Days)", size(large)) ///
				ylabel(, labsize(large)) xlabel(, labsize(large)) ///
				graphregion(color(white)) plotregion(color(white)) ///
				legend(off) /// 
				title("")
		graph export "..\results\figure_1_`ss'_`cg'0.pdf", as(pdf) replace 
		cap graph close
		
	restore
	}
}

cap log close
