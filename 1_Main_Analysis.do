*===================================================================================================================================================
* Project : 	USPCALING GROUNDNUT PRODUCTIVITY
* Paper title:		Adoption of Climate-Resilient Groundnut Varieties Increases Agricultural Production and Smallholder Commercialization in West Africa
* authors:		Martin Paul Jr. Tabe-Ojong, Jourdain Lokossou, Bisrat Gebrekidan, Hippolyte D. Affognon
* Date:			July 2023
* Stata version:	17
* license:		MIT


* ==================================================================================================================================================
 

 
clear all
set maxvar 30000
set more off
set varabbrev off, permanently
set seed 2038947

****************************************************************************************************************************************************
* SET FILE PATHS
**************************************************
cd "/Users/bisrat/Library/CloudStorage/Dropbox/Commercialization/Final revision/Adoption-of-Climate-Resilient-Groundnut-Varieties/"


** Loading the data**
use "data/Groundnut.dta", clear
merge 1:1 id year country using "data/HomeConsumption.dta", nogen

gen qcons2= gprod*(1-ssale)
gen qcons3= qcons
replace qcons3=. if qcons>=gprod
gen qcons4= asinh(qcons3)
gen qcons5=(qcons<gprod)
gen gcons=asinh(qcons)

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
label var qcons "Household groundnut consoumption"
label var gprod "Total quantity of groundnut harvested"

/*==================Data description and results in the main manuscript file================*/

***Figure 2. Adoption rate over time

** please refer to the RMarkdown file for the code

***Figure 2. Adoption rate over time

** please refer to the RMarkdown file for the code


xtset id year

***Figure 3. 2SLS estimates of the relationship between adoption, production, and yields

xtivreg gyield $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yield") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg gyield $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yield") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg prod_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Production value") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg prod_value $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Production value") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg qcons $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Consumption") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg qcons $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/figures_table/Figure3.txt, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Consumption") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append



***Figure 4. 2SLS estimates of the relationship between adoption and commercialization (adoption)

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("RE-MP") keep (adopt)   long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FE-MP") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("RE-QS") keep (adopt)   long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FE-QS") keep (adopt)   long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("RE-SV") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FE-SV") keep (adopt) long  nocons  nonotes noparen dec(3) quote lab append
 

***Figure 4. 2SLS estimates of the relationship between adoption and commercialization (area)

xtivreg sellers $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("RE-MP") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg sellers $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FE-MP") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append

 
xtivreg ihs_sale $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("RE-QS") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FE-QS") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append


xtivreg ihs_sales_value $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("RE-SV") keep (improvsup) long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sales_value $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/figures_table/Figure4, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("FE-SV") keep (improvsup)  long  nocons  nonotes noparen dec(3) quote lab append


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
merge m:1 id  using "data/AdoptHistory_2"
gen adopt3y= (adopt_hist==3)
gen adopt2y= (adopt_hist==2 | adopt_hist==3)

label var adopt3y "Three years continuous adoption"
label var adopt2y "Two years continuous adoption"



xtivreg gyield $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using stata_outputs/figures_table/Figure5, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yields") keep (adopt3y)  long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg qcons $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using stata_outputs/figures_table/Figure5, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Consumption") keep (adopt3y)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using stata_outputs/figures_table/Figure5, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("QS") keep (adopt3y)  long  nocons  nonotes noparen dec(3) quote lab append



****Figure 6. Cross-country analysis of adoption and commercialization


*GHANA
xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Ghana", re
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yield") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab replace

xtivreg qcons $xlist i.year i. district_1 (adopt=$iv) if country=="Ghana", fe
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Consumption") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv) if country=="Ghana", fe
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Quantity sold") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append


*MALI
xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Mali", re
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yield") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg qcons $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Consumption") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Quantity sold") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append


*NIGERIA
xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", re
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yield") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg qcons $xlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", fe
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Consumption") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", fe
outreg2 using stata_outputs/figures_table/Figure6, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Quantity sold") keep (adopt)  long  nocons  nonotes noparen dec(3) quote lab append



***Production, consumption and commercialization***

xtivreg sellers gprod qcons $xlist $tlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/figures_table/Figure7, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Market participation") keep (gprod qcons)  long  nocons  nonotes noparen dec(6) quote lab replace


xtivreg ihs_sale gprod qcons $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/figures_table/Figure7, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Quantity sold") keep (gprod qcons)  long  nocons  nonotes noparen dec(6) quote lab append


xtivreg ihs_sales_value gprod qcons $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/figures_table/Figure7, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Sales value") keep (gprod qcons)  long  nocons  nonotes noparen dec(6) quote lab append



***Figure 8. Quantile estimates of adoption and sales value (IHS)
*ssc install qregpd
*ssc install moremata
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
export excel _b_ _lb_ _ub_ using "extract1.xlsx", firstrow(variables) replace
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



/*==================Supplementary material================*/

*****POOLED OLS REGRESSIONS******
**Table SM2: Full OLS estimates of the relationship between adoption and commercialization(Adoption)

areg sellers adopt $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM2.tex, keep(adopt $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab replace
outreg2 using stata_outputs/figures_table/Figure_S1, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Market participation") keep(adopt)  long  nocons  nonotes noparen dec(6) quote lab replace

areg ihs_sale adopt $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM2.tex, stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab keep(adopt $xlist) append
outreg2 using stata_outputs/figures_table/Figure_S1, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Quantity Sold") keep(adopt) long  nocons  nonotes noparen dec(6) quote lab append

areg ihs_sales_value adopt $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM2.tex,  stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F))   nocons  nonotes  dec(3)  lab keep(adopt $xlist) append
outreg2 using stata_outputs/figures_table/Figure_S1, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Sale Value") keep(adopt)  long  nocons  nonotes noparen dec(6) quote lab append

***Table SM3: Full OLS estimates of the relationship between adoption and commercialization (Area under Adoption)
areg sellers improvsup $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM3.tex, keep(improvsup $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab replace
outreg2 using stata_outputs/figures_table/Figure_S1, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Market participation") keep(improvsup)  long  nocons  nonotes noparen dec(6) quote lab append

areg ihs_sale improvsup $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM3.tex, keep(improvsup $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append
outreg2 using stata_outputs/figures_table/Figure_S1, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Quantity Sold") keep(improvsup) long  nocons  nonotes noparen dec(6) quote lab append

areg ihs_sales_value improvsup $xlist, absorb(district) r
*xtreg sellers improvsup $xlist i.year i. district_1, fe
outreg2 using stata_outputs/SM_tables/Table_SM3.tex, keep(improvsup $xlist)  stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append
outreg2 using stata_outputs/figures_table/Figure_S1, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Sale Value") keep(improvsup)  long  nocons  nonotes noparen dec(6) quote lab append



****Table SM4: Full OLS estimates of the relationship between adoption, production and yields(Adoption)

areg gprod adopt $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, ctitle("Production") keep(adopt $xlist) stats(coef se pval)  addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab replace
outreg2 using stata_outputs/figures_table/Figure_S2, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Production") keep(adopt)  long  nocons  nonotes noparen dec(6) quote lab replace

areg prod_value adopt $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, ctitle("Production value") keep(adopt $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append
outreg2 using stata_outputs/figures_table/Figure_S2, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Production value") keep(adopt)  long  nocons  nonotes noparen dec(6) quote lab append

areg gyield adopt $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, ctitle("Yield") keep(adopt $xlist)  stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append
outreg2 using stata_outputs/figures_table/Figure_S2, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yield") keep(adopt)  long  nocons  nonotes noparen dec(6) quote lab append


areg qcons adopt $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM4.tex, ctitle("Consumption") keep(adopt $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append


***Table SM5: OLS estimates of the relationship between adoption, production and yields(Area under Adoption)

areg gprod improvsup $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, ctitle("Production") keep(improvsup $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab replace
outreg2 using stata_outputs/figures_table/Figure_S2, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Production") keep(improvsup)  long  nocons  nonotes noparen dec(6) quote lab append

areg prod_value improvsup $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, ctitle("Production value") keep(improvsup $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append
outreg2 using stata_outputs/figures_table/Figure_S2, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Production value") keep(improvsup)  long  nocons  nonotes noparen dec(6) quote lab append

areg gyield improvsup $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, ctitle("Yield") keep(improvsup $xlist) stats(coef se pval) paren(se) bracket(pval) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append
outreg2 using stata_outputs/figures_table/Figure_S2, sideway stats(coef se pval aster  ci_low ci_high ) ctitle("Yield") keep(improvsup)  long  nocons  nonotes noparen dec(6) quote lab append

areg qcons improvsup $xlist, absorb(district) r
outreg2 using stata_outputs/SM_tables/Table_SM5.tex, ctitle("Consumption") stats(coef se pval) paren(se) bracket(pval) keep(improvsup $xlist) addstat(F test, e(F)) long  nocons  nonotes  dec(3)  lab append 




*****PANEL REGRESSIONS******
xtset id year

**** Table SM6: Full 2SLS estimates of the relationship between adoption and commercialization(Adoption)

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, replace ctitle(FE_MP) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)
 
xtivreg sellers $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, append ctitle(RE_MP)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, append ctitle(FE_QS) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, append ctitle(RE_QS) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, append ctitle(FE_SV)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sales_value $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM6.tex, append ctitle(RE_SV) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)  keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


***Table SM7: Full 2SLS estimates of the relationship between adoption and commercialization (Area under Adoption)
xtivreg sellers $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM7.tex, replace ctitle(FE_MP)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg sellers $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM7.tex, append ctitle(RE_MP)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)
 
xtivreg ihs_sale $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM7.tex, append ctitle(FE_QS)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM7.tex, append ctitle(RE_QS)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sales_value $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM7.tex, append ctitle(FE_SV)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sales_value $xlist $tlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM7.tex, append ctitle(RE_SV)  stats(coef se pval) paren(se) bracket(pval)  dec(3) lab tex (frag pr land) keep(improvsup $xlist) addtext(District FE, YES, Year FE, YES)


***Table SM8: Full 2SLS estimates of the relationship between adoption, production and yields
xtivreg prod_value $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM8.tex, replace ctitle(FE_ProdVal) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup adopt $xlist) addtext(District FE, YES, Year FE, YES)
xtivreg gyield $xlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM8.tex, append ctitle(RE_YieldSup)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup adopt $xlist) addtext(District FE, YES, Year FE, YES)
xtivreg prod_value $xlist i.year i. district_1 (improvsup=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM8.tex, append ctitle(RE_ProdValSup)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(improvsup adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg gyield $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using stata_outputs/SM_tables/Table_SM8.tex, append ctitle(FE_Yield)  stats(coef se pval) paren(se) bracket(pval)  dec(3) lab tex (frag pr land) keep(improvsup adopt $xlist) addtext(District FE, YES, Year FE, YES)



xtivreg qcons $xlist i.year i. district_1 (adopt=$iv), fe 
outreg2 using stata_outputs/SM_tables/Table_SM8.tex, append ctitle(FE_Consumption)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg qcons $xlist i.year i. district_1 (improvsup=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM8.tex, append ctitle(FE_Consumption_sup) stats(coef se pval) dec(3) lab tex (frag pr land) keep(improvsup adopt $xlist) addtext(District FE, YES, Year FE, YES)



***Table SM9: Full 2SLS estimates of the relationship between continous adoption, production, consumption and sales

xtivreg gyield $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, replace ctitle(Yields)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt3y $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg qcons $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, append ctitle(Consumption) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt3y $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist $tlist i.year i. district_1 (adopt3y=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM9.tex, append ctitle(QS) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt3y $xlist) addtext(District FE, YES, Year FE, YES)



****Table SM10: 2SLS estimates of the relationship between adoption and commercialization and yield across countries


*GHANA

xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Ghana", re
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, replace ctitle(Yields Ghana) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg qcons $xlist i.year i. district_1 (adopt=$iv) if country=="Ghana", fe
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(Conso Ghana) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv) if country=="Ghana", fe
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(SV Ghana) stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


*MALI
xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Mali", re
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(Yields Mali)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg qcons $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(Conso Mali)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg ihs_sale $xlist i.year i. district_1 (adopt=$iv) if country=="Mali", fe
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(SV Mali)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)


*NIGERIA
xtivreg gyield $xlist $tlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", re
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(Yields Nigeria)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg qcons $xlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", fe
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(Conso Nigeria) stats(coef se pval) paren(se) bracket(pval)  dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtivreg sellers $xlist i.year i. district_1 (adopt=$iv) if country=="Nigeria", fe
outreg2 using stata_outputs/SM_tables/Table_SM10.tex, append ctitle(SV Nigeria)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)



***Table SM11: 2SLS estimates of the relationship between adoption and commercialization, production and yield simultaneously

***Production, consumption and commercialization***

xtivreg sellers gprod qcons $xlist i.year i. district_1 (adopt=$iv), re
outreg2 using stata_outputs/SM_tables/Table_SM11.tex, replace ctitle("Market participation") stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(gprod qcons $xlist) addtext(District FE, YES, Year FE, YES)


xtreg ihs_sale gprod qcons $xlist i.year i. district_1, fe
outreg2 using stata_outputs/SM_tables/Table_SM11.tex, append ctitle("Quantity sold") stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(gprod qcons $xlist) addtext(District FE, YES, Year FE, YES)


xtivreg ihs_sales_value gprod qcons $xlist i.year i. district_1 (adopt=$iv), fe
outreg2 using stata_outputs/SM_tables/Table_SM11.tex, append ctitle("Sales value") stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(gprod qcons $xlist) addtext(District FE, YES, Year FE, YES)





/*===============Robustness check=====================*/


***Table SM12: Full Control function estimations of the relationship between adoption and market participation

***Control function approach***
xtreg adopt $xlist $tlist $iv i.year i. district_1, re
predict double xb if e(sample), xb
gen double resid=adopt-xb if e(sample)
drop xb

xtreg sellers adopt resid $xlist i.year i. district_1, fe 
outreg2 using stata_outputs/SM_tables/Table_SM12.tex, replace ctitle(MP_FE)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtreg sellers adopt resid $xlist $tlist i.year i. district_1, re
outreg2 using stata_outputs/SM_tables/Table_SM12.tex, append ctitle(MP_CRE)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtreg ihs_sale adopt resid $xlist i.year i. district_1, fe
outreg2 using stata_outputs/SM_tables/Table_SM12.tex, append ctitle(QS_FE)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)
 
xtreg ihs_sale adopt resid $xlist $tlist i.year i. district_1, re
outreg2 using stata_outputs/SM_tables/Table_SM12.tex, append ctitle(QS_CRE)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)

xtreg ihs_sales_value adopt resid $xlist i.year i. district_1, fe
outreg2 using stata_outputs/SM_tables/Table_SM12.tex, append ctitle(SV_FE)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)
 
xtreg ihs_sales_value adopt resid $xlist $tlist i.year i. district_1, re 
outreg2 using stata_outputs/SM_tables/Table_SM12.tex, append ctitle(SV_CRE)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist) addtext(District FE, YES, Year FE, YES)



***Table SM13: Full HAUSMAN TAYLOR IV estimations
xthtaylor sellers adopt $xlist, endog(adopt)
outreg2 using stata_outputs/SM_tables/Table_SM13.tex, replace ctitle(MP)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist)

xthtaylor ihs_sale adopt $xlist, endog(adopt)
outreg2 using stata_outputs/SM_tables/Table_SM13.tex, append ctitle(QS)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist)

xthtaylor ihs_sales_value adopt $xlist, endog(adopt)
outreg2 using stata_outputs/SM_tables/Table_SM13.tex, append ctitle(SV)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist)



***Table SM14: Full Lewbel instrumental variable estimations of the relationship between adoption and commercialization. 
***LEWBELS TEST****

*ssc install ivreg2h
*ssc install ivhettest
*ssc install center
ivreg2h sellers adopt $xlist (adopt=), fe r
outreg2 using stata_outputs/SM_tables/Table_SM14.tex, replace ctitle(MP)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)keep(adopt $xlist)

ivreg2h ihs_sale adopt $xlist (adopt=), fe r
outreg2 using stata_outputs/SM_tables/Table_SM14.tex, append ctitle(QS)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land)keep(adopt $xlist)

ivreg2h ihs_sales_value adopt $xlist (adopt=), fe r
outreg2 using stata_outputs/SM_tables/Table_SM14.tex, append ctitle(SV)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist)

ivreg2h sellers adopt $xlist (adopt=access), fe r
outreg2 using stata_outputs/SM_tables/Table_SM14.tex, append ctitle(MP_Access)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist)

ivreg2h ihs_sale adopt $xlist (adopt=access), fe r
outreg2 using stata_outputs/SM_tables/Table_SM14.tex, append ctitle(QS_Access)  stats(coef se pval)dec(3) paren(se) bracket(pval) lab tex (frag pr land) keep(adopt $xlist)

ivreg2h ihs_sales_value adopt $xlist (adopt=access), fe r
outreg2 using stata_outputs/SM_tables/Table_SM14.tex, append ctitle(SV_Access)  stats(coef se pval) paren(se) bracket(pval) dec(3) lab tex (frag pr land) keep(adopt $xlist)



****************************************************************************************************
*******************************************END******************************************************
****************************************************************************************************







