*===================================================================================================================================================
* Project : 	USPCALING GROUNDNUT PRODUCTIVITY
* Program:		The Adoption of Climate-Resilient Groundnut Varieties Increases Agricultural Production and Smallholder Commercialization in West Africa
* ==================================================================================================================================================
 
 *This was perfored in STATA 17
 
clear all
set maxvar 30000
set more off
set varabbrev off, permanently
set seed 2038947

****************************************************************************************************************************************************
* SET FILE PATHS
**************************************************



** Loading the data**
use "Groundnut", clear

****************************************************************************************************************************************************
*DATA CLEANING AND PREPARATION
****************************************************************************************************************************************************
foreach var of varlist typsoil1 typsoil2 typsoil3 nbrhoejour {
replace `var'=0 if `var' ==.
}

gen objective = 1 if typeagri==1
replace objective = 0 if typeagri!=1

gen prod_value = gprod* uprice
gen sales_value = qsale* uprice

****Generating Instrumental Variable
*ssc install asrol
bys district: asrol adopt, stat(mean) xf(focal)

 foreach v of var country region district villag {
        	encode `v', gen(`v'_1)
        }

		
gen ihs_sale = asinh(qsale)		
gen ihs_sales_value = asinh(sales_value)	

gen ihs_prod_value = asinh(prod_value)

** recode the training on groundnut variable

replace formarrach= 4 if formarrach==0
	
****************************************************************************************************************************************************
****GENERATING TIME AVERAGES FOR CRE MODEL*****
egen agebar = mean(age), by (id)
egen nbschoolbar = mean( nbschool), by (id)
egen hhsizebar  = mean(hhsize), by (id)
egen cooperativebar = mean(cooperative), by (id)
egen formagribar = mean(formagri), by (id)
egen formarrachbar = mean(formarrach), by (id)
egen visitpublicbar = mean(visitpublic), by (id)
egen extensionbar = mean(extension), by (id)
egen creditebar = mean(credite), by (id)
egen creditnbar = mean( creditn), by (id)
egen crotationbar = mean(crotation), by (id)
egen cmixtbar = mean(cmixt), by (id)
egen nbrhoejourbar = mean(nbrhoejour), by (id)
egen upricebar = mean(uprice), by (id)
egen cseed_habar = mean(cseed_ha), by (id)
egen cfert_habar = mean(cfert_ha), by (id)
egen cpest_habar = mean(cpest_ha), by (id)
egen clabor_habar = mean(clabor_ha), by (id)
egen gsizebar = mean(gsize), by (id)
egen off_farmbar = mean(off_farm), by (id)
egen dratiobar = mean(dratio), by (id)


**************************************************************************************************************************************************
*****GLOBAL VARIABLES*******
global ylist sellers qsale sales_value 

global mlist gprod prod_value gyield

global xlist age sexe nbschool hhsize cooperative formagri formarrach visitpublic extension credite creditn dmurbain dmvillage crotation ///
cmixt nbrhoejour uprice cseed_ha cfert_ha cpest_ha clabor_ha gsize off_farm dratio typsoil1 typsoil2 typsoil3

global iv access 

global tlist agebar nbschoolbar hhsizebar cooperativebar formagribar formarrachbar visitpublicbar extensionbar creditebar creditnbar ///
 crotationbar cmixtbar nbrhoejourbar upricebar cseed_habar cfert_habar cpest_habar clabor_habar gsizebar off_farmbar dratiobar

/*VAriable Labels for SM design*/
*************************************************************************************************************************************
label var adopt "Adoption dummy"
label var improvsup "Area under adoption (ha)"
label var age "Age of household head (years)"
label var sexe "Sex of household head (dummy, male=1)"
label var nbschool "Education level (Number of years)"
label var hhsize "Household size (number of persons)"
label var cooperative "Farmers group membership (dummy) "
label var formagri "Training on agriculture (dummy)"
label var formarrach "Training on groundnut farming (dummy)"
label var visitpublic "Public agricultural extension service (number of visits)"
label var extension "Private agricultural extension service (number of visits)"
label var credite "Cash credit for groundnut farming (dummy)"
label var creditn "Credit in kind for groundnut farming (dummy)"
label var dmurbain "Distance to the nearest urban market (km)"
label var dmvillage "Distance the nearest village market (km)"
label var crotation "Crop rotation (dummy)"
label var cmixt "Mixed Crops (dummy)"
label var nbrhoejour "Labor force (man.day)"
label var uprice "Unit selling price (USD/kg)"
label var cseed_ha "Seed cost (USD/ha)"
label var cfert_ha "Fertilizer cost (USD/ha)"
label var cpest_ha "Pesticide cost (USD/ha)"
label var clabor_ha "Labor cost (USD/ha)"
label var gsize "Groundnut area (ha)"
label var off_farm "Off-farm income (dummy)"
label var dratio "Dependency ratio"
label var typsoil1 "Clay soil (dummy)"
label var typsoil2 "Sandy-clay soil (dummy)"
label var typsoil3 "Silty soil (dummy)"


/*==================Data description and results in the main manuscript file================*/

*** Figure 1. Kernel density distribution outcome variables

tabstat gyield gprod sellers qsale sales_value prod_value, by(adopt)

twoway 	(kdensity gyield if adopt==1, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity gyield if adopt==0, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Groundnut yield (kg/ha)") ///
		xline(587.74, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=587.74' "Non-adopters", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(918.86, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=918.86' "Adopters", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-adopters") label(2 "Adopters")) saving(Yield, replace)
		
		graph export "Yield.png", as(png) replace

twoway 	(kdensity gprod if adopt==1 & gprod<5000, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity gprod if adopt==0 & gprod<5000, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Groundnut production (kg)") ///
		xline(880.43, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=880.43' "Non-adopters", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(1608.06, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=1608.06' "Adopters", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-adopters") label(2 "Adopters")) saving(Prod, replace)
		
		graph export "Prod.png", as(png) replace

		
twoway 	(kdensity qsale if adopt==1 & qsale<5000, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity qsale if adopt==0 & qsale<5000, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Groundnut quantity sold (kg)") ///
		xline(427.31, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=427.31' "Non-adopters", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(754.59, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=754.59' "Adopters", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-adopters") label(2 "Adopters")) saving(QSold, replace)
		
		graph export "QSold.png", as(png) replace

	
twoway 	(kdensity sales_value if adopt==1 & qsale<5000, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity sales_value if adopt==0 & qsale<5000, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Sales value (USD)") ///
		xline(785.28, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=785.28' "Non-adopters", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(1046.93, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=1046.93' "Adopters", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-adopters") label(2 "Adopters")) saving(SValue, replace)
		
		graph export "SValue.png", as(png) replace
		
gr combine Yield.gph Prod.gph QSold.gph SValue.gph, col(2) iscale(.5) commonscheme
 
graph export "outcomes.png", as(png) replace



***Figure 2. Adoption rate over time



***Figure 3. 2SLS estimates of the relationship between adoption, production, and yields

xtivreg gyield $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using main_tbl6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Yield") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg gyield $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using main_tbl6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Yield") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg gprod $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using main_tbl6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg gprod $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using main_tbl6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg prod_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using main_tbl6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production value") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg prod_value $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using main_tbl6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production value") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append




***Figure 4. 2SLS estimates of the relationship between adoption and commercialization (adoption)

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("RE-MP") keep (adopt)   long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("FE-MP") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("RE-QS") keep (adopt)   long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("FE-QS") keep (adopt)   long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("RE-SV") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("FE-SV") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append
 

***Figure 4. 2SLS estimates of the relationship between adoption and commercialization (area)

xtivreg sellers $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("RE-MP") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg sellers $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("FE-MP") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab replace

 
xtivreg ihs_sale $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("RE-QS") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("FE-QS") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append


xtivreg ihs_sales_value $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("RE-SV") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sales_value $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Figure4, sideway stats(coef se aster  ci_low ci_high ) ctitle("FE-SV") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append


****Figure 5. 2SLS estimates of the relationship between continuous adoption and commercialization

/*
gen adopt_2017= (adopt) if year==2017
gen adopt_2018= (adopt) if year==2018
gen adopt_2019= (adopt) if year==2019
egen adopt_hist= rowtotal(adopt_2017 adopt_2018 adopt_2019) , missing
preserve
collapse (sum) adopt_hist, by(id)
*save AdoptHistory_2, replace
restore
*/
drop _merge
merge m:1 id  using AdoptHistory_2
gen adopt3y= (adopt_hist==3)
gen adopt2y= (adopt_hist==2 | adopt_hist==3)

label var adopt3y "Three years continuous adoption"
label var adopt2y "Two years continuous adoption"

xtivreg sellers $xlist $tlist i.year i. district_1 (adopt3y=$iv), re 
outreg2 using Figure5, sideway stats(coef se aster  ci_low ci_high ) ctitle("MP") keep (adopt3y)  long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using Figure5, sideway stats(coef se aster  ci_low ci_high ) ctitle("QS") keep (adopt3y)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sales_value $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using Figure5, sideway stats(coef se aster  ci_low ci_high ) ctitle("SV") keep (adopt3y)  long  nocons  nonotes noparen dec(3) quote lab append



****Figure 6. Cross-country analysis of adoption and commercialization


*GHANA

xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Ghana", re
outreg2 using Figure6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Yield") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg sellers $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Ghana", re
outreg2 using Figure6, sideway stats(coef se aster  ci_low ci_high ) ctitle("MP") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append


*MALI
xtivreg gyield $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe
outreg2 using Figure6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Yield") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe
outreg2 using Figure6, sideway stats(coef se aster  ci_low ci_high ) ctitle("MP") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append


*NIGERIA
xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", re
outreg2 using Figure6, sideway stats(coef se aster  ci_low ci_high ) ctitle("Yield") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg sellers $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", re
outreg2 using Figure6, sideway stats(coef se aster  ci_low ci_high ) ctitle("MP") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append



***Figure 7. Quantile estimates of adoption and sales value (IHS)
ssc install qregpd
ssc install moremata
***QUANTILE REGRESSION FOR INCOME***
sqreg ihs_sales_value improvsup , quantile (.1 .2 .3 .4 .5 .6 .7 .8 .9) reps(100)
preserve
gen q = _n*10 in 1/9
foreach var of varlist improvsup {
    gen _b_`var'  = .
    gen _lb_`var' = .
    gen _ub_`var' = .
    local i = 1
    foreach q of numlist 10(10)90 {
        replace _b_`var' = _b[q`q':`var'] in `i'
        replace _lb_`var' = _b[q`q':`var'] - _se[q`q':`var']*invnormal(.975) in `i'
        replace _ub_`var' = _b[q`q':`var'] + _se[q`q':`var']*invnormal(.975) in `i++'
    }
}
keep q _b_* _lb_* _ub_*
keep in 1/9
reshape long _b_ _lb_ _ub_, i(q) j(var) string
set scheme s1color
twoway rarea _lb_ _ub_ q, astyle(ci) yline(0) acolor(%90) || ///
   line _b_ q,                                               ///
   by(var, yrescale xrescale note("") legend(at(4) pos(0)))  ///
   legend(order(2 "effect"                                   ///      
                1 "95% confidence" "interval")               ///
          cols(1))                                           ///
   ytitle(effect on percentile of area under adoption)                       ///
   ylab(,angle(0) format(%7.0gc))                            ///    
   xlab(10(10)90) xtitle(area under adoption)
restore



*****Figure 8. Linear IV mediation analysis of adoption and commercialization
*Mediator = production
ivmediate sellers $xlist, mediator(gprod) treatment(adopt) instrument(access) absorb (district_1) full
outreg2 using Figure8, sideway stats(coef se aster  ci_low ci_high ) ctitle("Sellers") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) dec(3)  replace

ivmediate ihs_sale $xlist, mediator(gprod) treatment(adopt) instrument(access) absorb (district_1) full
outreg2 using Figure8, sideway stats(coef se aster  ci_low ci_high ) ctitle("Quantity Sold") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) dec(3)  append

ivmediate ihs_sales_value $xlist, mediator(gprod) treatment(adopt) instrument(access) absorb (district_1) full
outreg2 using Figure8, sideway stats(coef se aster  ci_low ci_high ) ctitle("Sales Value") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) dec(3)  append


*Mediator = yield
ivmediate sellers $xlist, mediator(gyield) treatment(adopt) instrument(access) absorb (district_1)
outreg2 using mediation_yield, replace  ctitle("Sellers") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)

ivmediate ihs_sale $xlist, mediator(gyield) treatment(adopt) instrument(access) absorb (district_1)
outreg2 using mediation_yield, append  ctitle("Quantity Sold") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)

ivmediate ihs_sales_value $xlist, mediator(gyield) treatment(adopt) instrument(access) absorb (district_1)  full
outreg2 using mediation_yield, append  ctitle("Sales Value") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)









/*==================Supplementary material================*/


** Supplementary Note plots**


****SM Figure 1 OLS estimates of the relationship between adoption and commercialization***
areg sellers adopt $xlist, absorb(district) r

outreg2 using main_tbl2, sideway stats(coef se aster  ci_low ci_high ) keep (adopt)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab replace


areg ihs_sale adopt $xlist, absorb(district) r

outreg2 using main_tbl2, sideway stats(coef se aster  ci_low ci_high ) keep (adopt)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

areg ihs_sales_value adopt $xlist, absorb(district) r

outreg2 using main_tbl2, sideway stats(coef se aster  ci_low ci_high ) keep (adopt)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

areg sellers improvsup $xlist, absorb(district) r
outreg2 using main_tbl2, sideway stats(coef se aster  ci_low ci_high ) keep (improvsup)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

areg ihs_sale improvsup $xlist, absorb(district) r
outreg2 using main_tbl2, sideway stats(coef se aster  ci_low ci_high ) keep (improvsup)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

areg ihs_sales_value improvsup $xlist, absorb(district) r
outreg2 using main_tbl2, sideway stats(coef se aster  ci_low ci_high ) keep (improvsup)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append




****************
****SM Figure 1 OLS estimates of the relationship between adoption and production*****
****************

****Adoption****
areg gprod adopt $xlist, absorb(district) r
outreg2 using main_tbl3, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production") keep (adopt)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab replace

areg prod_value adopt $xlist, absorb(district) r
outreg2 using main_tbl3, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production value") keep (adopt)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

areg gyield adopt $xlist, absorb(district) r
outreg2 using main_tbl3, sideway stats(coef se aster  ci_low ci_high ) ctitle("Yield") keep (adopt)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append



areg gprod improvsup $xlist, absorb(district) r
outreg2 using main_tbl3, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production") keep (improvsup)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

areg prod_value improvsup $xlist, absorb(district) r
outreg2 using main_tbl3, sideway stats(coef se aster  ci_low ci_high ) ctitle("Production value")  keep (improvsup)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

areg gyield improvsup $xlist, absorb(district) r
outreg2 using main_tbl3, sideway stats(coef se aster  ci_low ci_high ) ctitle("Yield")  keep (improvsup)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append



/*==================Supplementary material================*/




**Table SM1: Summary Statistics by year and adoption status**

eststo clear
by year adopt, sort: eststo: quietly estpost sum $xlist assoc
esttab using sum_stat.csv, replace cell(mean(fmt(2)) sd(par fmt(2))) p(2) label nodepvar

eststo clear
by adopt, sort: eststo: quietly estpost sum $xlist assoc
esttab using sum_stat2.csv, replace cell(mean(fmt(2)) sd(par fmt(2))) p(2) label nodepvar


*****POOLED OLS REGRESSIONS******
**Table SM2: Full OLS estimates of the relationship between adoption and commercialization(Adoption)

areg sellers adopt $xlist, absorb(district) r
outreg2 using Table_SM2.txt, addstat(F test, e(F))  long  nocons  nonotes noparen dec(3)  lab replace

areg ihs_sale adopt $xlist, absorb(district) r
outreg2 using Table_SM2.txt, addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab append

areg ihs_sales_value adopt $xlist, absorb(district) r
outreg2 using Table_SM2.txt, addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab append


***Table SM3: Full OLS estimates of the relationship between adoption and commercialization (Area under Adoption)
areg sellers improvsup $xlist, absorb(district) r
outreg2 using Table_SM3.txt, addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab replace

areg ihs_sale improvsup $xlist, absorb(district) r
outreg2 using Table_SM3.txt, addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab append

areg ihs_sales_value improvsup $xlist, absorb(district) r
outreg2 using Table_SM3.txt, addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab append


****Table SM4: Full OLS estimates of the relationship between adoption, production and yields(Adoption)

areg gprod adopt $xlist, absorb(district) r
outreg2 using Table_SM4.txt, replace ctitle("Production") addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab  

areg prod_value adopt $xlist, absorb(district) r
outreg2 using Table_SM4.txt, append ctitle("Production value") addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  

areg gyield adopt $xlist, absorb(district) r
outreg2 using Table_SM4.txt, append ctitle("Yield") addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab  


***Table SM5: OLS estimates of the relationship between adoption, production and yields(Area under Adoption)

areg gprod improvsup $xlist, absorb(district) r
outreg2 using Table_SM5.txt, ctitle("Production") addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab replace

areg prod_value improvsup $xlist, absorb(district) r
outreg2 using Table_SM5.txt, ctitle("Production value") addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab append

areg gyield improvsup $xlist, absorb(district) r
outreg2 using Table_SM5.txt, ctitle("Yield") addstat(F test, e(F)) long  nocons  nonotes noparen dec(3)  lab append




*****PANEL REGRESSIONS******
xtset id year

**** Table SM6: Full 2SLS estimates of the relationship between adoption and commercialization(Adoption)

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using Table_SM6.txt, replace ctitle(FE_MP) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)
 
xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using Table_SM6.txt, append ctitle(RE_MP) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using Table_SM6.txt, append ctitle(FE_QS) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using Table_SM6.tex, append ctitle(RE_QS) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using Table_SM6.txt, append ctitle(FE_SV) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

 
xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using Table_SM6.txt, append ctitle(RE_SV) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


***Table SM7: Full 2SLS estimates of the relationship between adoption and commercialization (Area under Adoption)
xtivreg sellers $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM7.txt, replace ctitle(FE_MP) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg sellers $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Table_SM7.txt, append ctitle(RE_MP) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)
 
xtivreg ihs_sale $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM7.txt, append ctitle(FE_QS) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Table_SM7.txt, append ctitle(RE_QS) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sales_value $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM7.txt, append ctitle(FE_SV) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sales_value $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Table_SM7.txt, append ctitle(RE_SV) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)


***Table SM8: Full 2SLS estimates of the relationship between adoption, production and yields

xtivreg gprod $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using Table_SM8.tex, replace ctitle(FE_Prod) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg gprod $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM8.tex, append ctitle(FE_ProdSUP) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg prod_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using Table_SM8.tex, append ctitle(FE_ProdVal) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


xtivreg prod_value $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM8.tex, append ctitle(FE_ProdValSup) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)


xtivreg gyield $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using Table_SM8.tex, append ctitle(FE_Yield) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


xtivreg gyield $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM8.tex, append ctitle(FE_YieldSup) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)



***Table SM9: Full 2SLS estimates of the relationship between continous adoption,
production and yields

xtivreg sellers $xlist $tlist i.year i. district_1 (adopt3y=$iv), re 
outreg2 using Table_SM9.tex, replace ctitle(MP) dec(3) lab keep(adopt3y $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using Table_SM9.tex, append ctitle(QS) dec(3) lab tex (frag pr land) keep(adopt3y $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sales_value $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using Table_SM9.tex, append ctitle(SV) dec(3) lab tex (frag pr land) keep(adopt3y $xlist) addtext(District FE, YES, Year FE, YES)


****Table SM10: Full Correlated random effects estimations of the relationship between adoption, commercialization and market orientation

*Generating commercialization index
gen share= qsale/gprod
drop if share>1   /*this is because some households reported sales greater than production, 
                    which could be due to storage. However, we drop all these hhs*/

*Market orientation
gen orientation=1 if share>0.5
replace orientation=0 if orientation!=1

**Adoption
***Share
****OLS
areg share adopt $xlist, absorb(district) r
outreg2 using Table_SM10.tex, replace ctitle(OLS_Share) dec(3) lab tex (frag pr land) keep(adopt $xlist) 

****FE
xtivreg share $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using Table_SM10.tex, append ctitle(FE_Share) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


****RE
xtivreg share $xlist $tlist i.year i. district_1 (adopt=$iv), re 
outreg2 using Table_SM10.tex, append ctitle(RE_Share) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


***orientation
****OLS
areg orientation adopt $xlist, absorb(district) r
outreg2 using Table_SM10.tex, append ctitle(OLS_orientation) dec(3) lab tex (frag pr land) keep(adopt $xlist) 

****FE

xtivreg orientation $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using Table_SM10.tex, append ctitle(FE_orientation) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE,YES)


****RE
xtivreg orientation $xlist $tlist i.year i. district_1 (adopt=$iv), re 
outreg2 using Table_SM10.tex, append ctitle(RE_orientation) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


***Table SM11: Full Correlated random effects estimations of the relationship between adoption (area), commercialization and market orientation

***Share
****OLS
areg share improvsup $xlist, absorb(district) r
outreg2 using Table_SM11.tex, replace ctitle(OLS_Share) dec(3) lab tex (frag pr land) 

****FE
xtivreg share $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM11.tex, append ctitle(FE_Share) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

****RE
xtivreg share $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Table_SM11.tex, append ctitle(RE_share) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

***orientation
****OLS
areg orientation improvsup $xlist, absorb(district) r
outreg2 using Table_SM11.tex, append ctitle(OLS_orientation) dec(3) lab tex (frag pr land) keep(improvsup $xlist) 


****FE
xtivreg orientation $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using Table_SM11.tex, append ctitle(FE_orientation) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

****RE
xtivreg orientation $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using Table_SM11.tex, append ctitle(RE_orientation) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)


***Table SM12: Linear IV mediation analysis of adoption and commercialization (production)
ivmediate sellers $xlist, mediator(gprod) treatment(adopt) instrument(access) absorb (district_1) full
outreg2 using Table_SM12.tex, replace  ctitle("Sellers") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)

ivmediate ihs_sale $xlist, mediator(gprod) treatment(adopt) instrument(access) absorb (district_1) full
outreg2 using Table_SM12.tex, append ctitle("Quantity Sold") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)

ivmediate ihs_sales_value $xlist, mediator(gprod) treatment(adopt) instrument(access) absorb (district_1) full
outreg2 using Table_SM12.tex, append ctitle("Sales Value") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)


***Table SM13: Linear IV mediation analysis of adoption and commercialization (Yield)
ivmediate sellers $xlist, mediator(gyield) treatment(adopt) instrument(access) absorb (district_1)
outreg2 using Table_SM13, replace  ctitle("Sellers") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)

ivmediate ihs_sale $xlist, mediator(gyield) treatment(adopt) instrument(access) absorb (district_1)
outreg2 using Table_SM13, append  ctitle("Quantity Sold") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)

ivmediate ihs_sales_value $xlist, mediator(gyield) treatment(adopt) instrument(access) absorb (district_1)  full
outreg2 using Table_SM13, append  ctitle("Sales Value") label(proper) addstat(Mediator share of TE (%), `e(mepct)', F statistic on instrument (1), `e(fstat1)', F statistic on instruments (2), `e(fstat2)' ) addtext(Additional controls, YES, District dummies, YES)



****Table SM14: 2SLS estimates of the relationship between adoption and commercialization and yield across countries


*GHANA

xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Ghana", re

outreg2 using Table_SM14.tex, replace ctitle(Yields Ghana) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Ghana", re

outreg2 using Table_SM14.tex, append ctitle(Commercialization Ghana) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


*MALI
xtivreg gyield $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe

outreg2 using Table_SM14.tex, append ctitle(Yields Mali) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe

outreg2 using Table_SM14.tex, append ctitle(Commercialization Mali) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

*NIGERIA
xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", re

outreg2 using Table_SM14.tex, append ctitle(Yields Nigeria) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", re

outreg2 using Table_SM14.tex, append ctitle(Commercialization Nigeria) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)



/*===============Robustness check=====================*/


***Table SM15: Full Control function estimations of the relationship between adoption and market participation

***Control function approach***
xtreg adopt $xlist $tlist $iv i.year i. district_1, re
predict double xb if e(sample), xb
gen double resid=adopt-xb if e(sample)
drop xb

xtreg sellers adopt resid $xlist i.year i. district_1, fe 
outreg2 using Table_SM15.tex, replace ctitle(MP_FE) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtreg sellers adopt resid $xlist $tlist i.year i. district_1, re
outreg2 using Table_SM15.tex, append ctitle(MP_CRE) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtreg ihs_sale adopt resid $xlist i.year i. district_1, fe
outreg2 using Table_SM15.tex, append ctitle(QS_FE) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)
 
xtreg ihs_sale adopt resid $xlist $tlist i.year i. district_1, re
outreg2 using Table_SM15.tex, append ctitle(QS_CRE) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtreg ihs_sales_value adopt resid $xlist i.year i. district_1, fe
outreg2 using Table_SM15.tex, append ctitle(SV_FE) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)
 
xtreg ihs_sales_value adopt resid $xlist $tlist i.year i. district_1, re 
outreg2 using Table_SM15.tex, append ctitle(SV_CRE) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)



***Table SM16: Full HAUSMAN TAYLOR IV estimations
xthtaylor sellers adopt $xlist, endog(adopt)
outreg2 using Table_SM16.tex, replace ctitle(MP) dec(3) lab tex (frag pr land)

xthtaylor ihs_sale adopt $xlist, endog(adopt)
outreg2 using Table_SM16.tex, append ctitle(QS) dec(3) lab tex (frag pr land)

xthtaylor ihs_sale adopt $xlist, endog(adopt)
outreg2 using Table_SM16.tex, append ctitle(SV) dec(3) lab tex (frag pr land)



***Table SM17: Full Lewbel instrumental variable estimations of the relationship between adoption and commercialization. 
***LEWBELS TEST****

*ssc install ivreg2h
*ssc install ivhettest
*ssc install center
ivreg2h sellers adopt $xlist (adopt=), fe r
outreg2 using Table_SM17.tex, replace ctitle(MP) dec(3) lab tex (frag pr land)

ivreg2h ihs_sale adopt $xlist (adopt=), fe r
outreg2 using Table_SM17.tex, append ctitle(QS) dec(3) lab tex (frag pr land)

ivreg2h ihs_sales_value adopt $xlist (adopt=), fe r
outreg2 using Table_SM17.tex, append ctitle(SV) dec(3) lab tex (frag pr land)

ivreg2h sellers adopt $xlist (adopt=access), fe r
outreg2 using Table_SM17.tex, append ctitle(MP_Access) dec(3) lab tex (frag pr land)

ivreg2h ihs_sale adopt $xlist (adopt=access), fe r
outreg2 using Table_SM17.tex, append ctitle(QS_Access) dec(3) lab tex (frag pr land)

ivreg2h ihs_sales_value adopt $xlist (adopt=access), fe r
outreg2 using Table_SM17.tex, append ctitle(SV_Access) dec(3) lab tex (frag pr land)












