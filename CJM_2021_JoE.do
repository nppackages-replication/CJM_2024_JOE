********************************************************************************
* Local Regression Distribution Estimators -- Replication
* M.D. Cattaneo, M. Jansson, X. Ma
* Jan 27, 2021
********************************************************************************

* net install lpdensity, from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace

clear all
set more off

*-------------------------------------------------------------------------------
* import data
*-------------------------------------------------------------------------------

use "jtpa.dta"

preserve

*-------------------------------------------------------------------------------
* Figure 2
*-------------------------------------------------------------------------------

capture drop grid // set up grid points
qui gen grid = 1.97 + 0.03 * _n if _n <= 101

capture drop eduFull_*
capture drop edu1_*
capture drop edu0_*

// full sample
qui lpdensity logincome if treatment == 0, bwselect(imse-dpi) grid(grid) ///
	ciuniform genvars(eduFull)
	
// with HS or GED
qui lpdensity logincome if treatment == 0 & hsorged > 0, bwselect(imse-dpi) grid(grid) ///
	cweights(hsorged) ciuniform	genvars(edu1)
	
// no HS or GED
qui lpdensity logincome if treatment == 0 & hsorged == 0, bwselect(imse-dpi) grid(grid) ///
	ciuniform genvars(edu0)
	
// Figure 2(a)
twoway 	(line eduFull_f_p eduFull_grid	, col(red))			///
		(line edu1_f_p edu1_grid		, col(blue))		///
		(line edu0_f_p edu0_grid		, col(green))		///
		(rarea eduFull_CI_l eduFull_CI_r eduFull_grid	, col(red%20)   lcolor(red%0))	 ///
		(rarea edu1_CI_l edu1_CI_r edu1_grid			, col(blue%20)  lcolor(blue%0))	 ///
		(rarea edu0_CI_l edu0_CI_r edu0_grid			, col(green%20) lcolor(green%0)) ///
		,								///
		legend(cols(3) order(1 "Full" 2 "HS or GED" 3 "No HS or GED")) ///
		xtitle("log(income)") ytitle("")

// counterfactual weights estimation
capture drop male2nonwhite
capture drop male2wkless13
capture drop nonwhite2wkless13
capture drop afdc2male
capture drop cweights
qui gen male2nonwhite = male * nonwhite
qui gen male2wkless13 = male * wkless13
qui gen nonwhite2wkless13 = nonwhite * wkless13
qui gen afdc2male = afdc * male

qui reg hsorged male nonwhite wkless13 married male2nonwhite male2wkless13 nonwhite2wkless13 afdc2male /// 
	age2225 age2629 age3035 age3644 age4554 if treatment == 0
qui predict cweights
qui replace cweights = (1 - cweights) / cweights
qui replace cweights = cweights * hsorged if hsorged > 0

// estimate counterfactual density
capture drop educ_*
qui lpdensity logincome if treatment == 0 & hsorged > 0, bwselect(imse-dpi) grid(grid) ///
	cweights(cweights) ciuniform genvars(educ)
	
// Figure 2(b)
twoway 	(line educ_f_p educ_grid , col(red))		///
		(line edu1_f_p edu1_grid , col(blue))		///
		(line edu0_f_p edu0_grid , col(green))		///
		(rarea educ_CI_l educ_CI_r educ_grid , col(red%20)   lcolor(red%0))	  ///
		(rarea edu1_CI_l edu1_CI_r edu1_grid , col(blue%20)  lcolor(blue%0))  ///
		(rarea edu0_CI_l edu0_CI_r edu0_grid , col(green%20) lcolor(green%0)) ///
		,								///
		legend(cols(3) order(1 "Counterfactual" 2 "HS or GED" 3 "No HS or GED")) ///
		xtitle("log(income)") ytitle("")

*-------------------------------------------------------------------------------
* Figure 4
*-------------------------------------------------------------------------------

// full sample
capture drop full_*
qui lpdensity logincome, bwselect(imse-dpi) grid(grid) genvars(full) ciuniform

// JTPA offer
capture drop offer1_* 
capture drop offer0_* 
// JTPA offer
qui lpdensity logincome if instrument == 1, bwselect(imse-dpi) grid(grid) ///
	ciuniform genvars(offer1)
// no JTPA offer
qui lpdensity logincome if instrument == 0, bwselect(imse-dpi) grid(grid) ///
	ciuniform genvars(offer0)

// JTPA Enroll
capture drop treat1_* 
capture drop treat0_* 
// Enrolled in JTPA
qui lpdensity logincome if treatment == 1, bwselect(imse-dpi) grid(grid) ///
	ciuniform genvars(treat1)
// not Enrolled in JTPA
qui lpdensity logincome if treatment == 0, bwselect(imse-dpi) grid(grid) ///
	ciuniform genvars(treat0)

// weights for compliers, Abadie (2003)
capture drop cweights
capture drop cweights0
capture drop cweights1
capture drop hsorged2male
capture drop hsorged2nonwhite
capture drop hsorged2wkless13
capture drop male2nonwhite
capture drop male2wkless13
capture drop nonwhite2wkless13
qui gen hsorged2male = hsorged * male
qui gen hsorged2nonwhite = hsorged * nonwhite
qui gen hsorged2wkless13 = hsorged * wkless13
qui gen male2nonwhite = male * nonwhite
qui gen male2wkless13 = male * wkless13
qui gen nonwhite2wkless13 = nonwhite * wkless13

qui reg instrument male hsorged nonwhite wkless13 married hsorged2male  hsorged2nonwhite ///
	hsorged2wkless13 male2nonwhite male2wkless13 nonwhite2wkless13
qui predict cweights
qui gen cweights0 = cweights
qui gen cweights1 = cweights
qui replace cweights = 1 - instrument * (1 - treatment) / cweights - (1 - instrument) * treatment / (1 - cweights)
qui replace cweights1 = treatment * (instrument - cweights1) / (cweights1 * (1 - cweights1))
qui replace cweights0 = (1 - treatment) * (1 - instrument - 1 + cweights0) / (cweights0 * (1 - cweights0))

// density for compliers
capture drop complier_* 
qui lpdensity logincome, bwselect(imse-dpi) grid(grid) cweights(cweights) ///
	ciuniform genvars(complier)

capture drop complier0_* 
qui lpdensity logincome, bwselect(imse-dpi) grid(grid) cweights(cweights0) ///
	ciuniform genvars(complier0)

capture drop complier1_* 
qui lpdensity logincome, bwselect(imse-dpi) grid(grid) cweights(cweights1) ///
	ciuniform genvars(complier1)

// Figure 4(a)
twoway 	(line full_f_p full_grid , col(red))			///
		(line complier_f_p complier_grid , col(blue))	///
		(rarea full_CI_l full_CI_r full_grid , col(red%20)   lcolor(red%0))	  ///
		(rarea complier_CI_l complier_CI_r complier_grid , col(blue%20)  lcolor(blue%0))  ///
		,								///
		legend(cols(3) order(1 "Full" 2 "Compliers")) ///
		xtitle("log(income)") ytitle("")
		
// Figure 4(b)
twoway 	(line offer0_f_p offer0_grid , col(red))	///
		(line offer1_f_p offer1_grid , col(blue))	///
		(rarea offer0_CI_l offer0_CI_r offer0_grid , col(red%20)   lcolor(red%0))	///
		(rarea offer1_CI_l offer1_CI_r offer1_grid , col(blue%20)  lcolor(blue%0))  ///
		,								///
		legend(cols(3) order(1 "JTPA Offer = 0" 2 "1")) ///
		xtitle("log(income)") ytitle("")

// Figure 4(c)
twoway 	(line treat0_f_p treat0_grid , col(red))	///
		(line treat1_f_p treat1_grid , col(blue))	///
		(rarea treat0_CI_l treat0_CI_r treat0_grid , col(red%20)   lcolor(red%0))	///
		(rarea treat1_CI_l treat1_CI_r treat1_grid , col(blue%20)  lcolor(blue%0))  ///
		,								///
		legend(cols(3) order(1 "JTPA Enrollment = 0" 2 "1")) ///
		xtitle("log(income)") ytitle("")
		
// Figure 4(d)
twoway 	(line complier0_f_p complier0_grid , col(red))	///
		(line complier1_f_p complier1_grid , col(blue))	///
		(rarea complier0_CI_l complier0_CI_r complier0_grid , col(red%20)   lcolor(red%0))	///
		(rarea complier1_CI_l complier1_CI_r complier1_grid , col(blue%20)  lcolor(blue%0)) ///
		,								///
		legend(cols(3) order(1 "x(0) for compliers" 2 "x(1) for compliers")) ///
		xtitle("log(income)") ytitle("")
		
*-------------------------------------------------------------------------------
* Figure 3
*-------------------------------------------------------------------------------

// not offered, not enrolled
qui count if instrument == 0
local scale = r(N)
qui count if instrument == 0 & treatment == 0
local scale = r(N) / `scale'
qui lpdensity logincome if instrument==0 & treatment==0, bwselect(imse-dpi) grid(grid) ///
	scale(`scale') ciuniform genvars(nOnE) 

// offered, not enrolled
qui count if instrument == 1
local scale = r(N)
qui count if instrument == 1 & treatment == 0
local scale = r(N) / `scale'
qui lpdensity logincome if instrument==1 & treatment==0, bwselect(imse-dpi) grid(grid) ///
	scale(`scale') ciuniform genvars(OnE) 

// Figure 3
twoway 	(line nOnE_f_p nOnE_grid, col(red))	///
		(line OnE_f_p OnE_grid  , col(blue))	///
		(rarea nOnE_CI_l nOnE_CI_r nOnE_grid, col(red%20)   lcolor(red%0))	///
		(rarea OnE_CI_l OnE_CI_r OnE_grid 	, col(blue%20)  lcolor(blue%0)) ///
		,								///
		legend(cols(3) order(1 "Not Offered, Not Enrolled" 2 "Offered, Not Enrolled")) ///
		xtitle("log(income)") ytitle("")
	
*-------------------------------------------------------------------------------	
	
restore
