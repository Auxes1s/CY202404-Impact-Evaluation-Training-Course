clear all
cd "G:\My Drive\Teaching\201905 IE Training - DLSU\replication, did, aot18\"


// Combine datasets
use "blgf.dta", clear
  merge m:1 psgc using "psgc.dta", nogen keep(1 3)
  merge m:1 psgc using "city.dta", nogen keep(1 3)
  merge m:1 psgc using "larea.dta", nogen keep(1 3)
  merge m:1 year using "cpi.dta", nogen keep(1 3)
  merge 1:1 year psgc using "pop.dta", nogen keep(1 3)

  
// Geographic codes  
gen reg = substr(psgc, 1, 2)
gen prv = substr(psgc, 3, 2)
gen mun = substr(psgc, 5, 2)
  destring reg prv mun, replace
gen prv2 = prv  
  replace prv2 = 39 if inlist(prv, 39, 74, 75, 76) // NCR collapsed as one province
merge 1:1 year prv mun using "precip.dta", nogen keep(1 3)
  
// City ratification
recode cyear (. = 9999) 
gen t = year - cyear 
  replace t = 0 if t < 0  
  label var t "Years since city ratification"
recode t (1/3 = 3) (4/6 = 6) (7/9 = 9) (10/999 = 10), gen(tx)
  label var tx "Years since city ratification, recode"
gen cityx = (t > 0)
  label var cityx  "1 = City"

  
// Prices  
su cpi1990 if year==2000, meanonly
gen cpi2000 = cpi1990/r(mean)  
  label var cpi2000 "CPI 2000-base"

// Convert to constant PhP  
foreach xxx in lo na  {
  egen inc`xxx' = rowtotal(inc`xxx'*)
  gen inc`xxx'_r = inc`xxx'*1000000/cpi2000
 } 
    label var inclo_r "Income, local sources"
    label var inclo_r "Income, transfers"

	
foreach vvv in incna_r inclo_r popx larea {
  gen ln`vvv' = ln(`vvv')
    local lbl: var label `vvv'
	label var ln`vvv' "`lbl', log scale"
 }

foreach vvv in inc exp {
  gen ln`vvv'_pcr = ln((`vvv'*1000000/popx) / cpi2000)
    local lbl: var label `vvv'
	label var ln`vvv'_pcr "`lbl', per capita, log scale"
 }
 
encode psgc, gen(id)
  label var id "PSGC, numeric"
xtset id year
foreach vvv in inclo_r popx {
  gen `vvv'3 = (l.`vvv' + l2.`vvv' + l3.`vvv')/3
  gen ln`vvv'3 = ln(`vvv'3) 
 }


// SIMPLE DID
egen evercity = max(city), by(psgc)
table year evercity if inlist(year, 1992, 2015) & !(cyear<=2000), c(mean lninc_pcr) 
display (7.913204-5.914218)-(7.433907-5.927306) // between 1992 and 2015!
reg lninc_pcr i.year##i.evercity if inlist(year, 1992, 2015) & !(cyear<=2000)


// Models
local i = 1

* Full sample
xtreg lninc_pcr i.year i.cityx, fe vce(cluster prv)
  est store inc1a
xtreg lninc_pcr i.year i.cityx lninclo_r3 lnpopx3, fe vce(cluster prv)
  est store inc1b
   
// Tabulate	results  
global varlist0  1.cityx 3.tx 6.tx 9.tx 10.tx lninclo_r3 lnpopx3
global varlist1  1.cityx lninclo_r3 lnpopx3

* Full sample
esttab inc1a inc1b, ///
   order($varlist0) keep($varlist0) ///
   b(%4.3f) se(%4.3f) ///
   star(* 0.10 ** 0.05 *** 0.01) ///
   scalars(N r2_a testF testFpval)	
   
   
// Placebo Test
gen lnrain_pcr = precip_dlr
  label var lnrain_pcr "Rainfall, standardized to long-run normal"
  
* Test 1: Early ratification by 3 years
gen cityx3 = (year >= (cyear - 3))
  label var cityx3 "Earlier city ratification, 3 years"
  
foreach vvv in inc exp rain {
    xtset
    qui xtreg ln`vvv'_pcr i.year i.cityx3 if (cityx==0), fe vce(cluster prv)
      est store `vvv'p1a
    qui xtreg ln`vvv'_pcr i.year i.cityx3 l.(lninclo_r lnpopx) if (cityx==0), fe vce(cluster prv)
      est store `vvv'p1b
 }
	
  esttab incp1a incp1b expp1a expp1b rainp1a rainp1b, ///
     order(1.cityx3) keep(1.cityx3) ///
     b(%4.3f) se(%4.3f) ///
     star(* 0.10 ** 0.05 *** 0.01) ///
     scalars(N r2_a)	

	 
* Test 2: Random assignment
set seed 1986
tempvar rand randx city cityx
  gen `rand' = 1900 + floor(runiform()*(2015-1900)) + 1 if (year==2015)
  egen `randx' = max(`rand'), by(id)
  gen `city' = runiform() < 0.1 if (year==2015)
  egen `cityx' = max(`city'), by(id)
  gen cyear_rand = `randx'*`cityx' + 9999*(`cityx'==0)
  gen city_rand = year > cyear_rand

foreach vvv in inc exp rain {
    xtset
    qui xtreg ln`vvv'_pcr i.year i.city_rand, fe vce(cluster prv)
      est store `vvv'p2a
    qui xtreg ln`vvv'_pcr i.year i.city_rand l.(lninclo_r lnpopx), fe vce(cluster prv)
      est store `vvv'p2b
 }
	
  esttab incp2a incp2b expp2a expp2b rainp2a rainp2b , ///
     order(1.city_rand) keep(1.city_rand) ///
     b(%4.3f) se(%4.3f) ///
     star(* 0.10 ** 0.05 *** 0.01) ///
     scalars(N r2_a)	
	 
	 
* Test 3: Unrelated outcome - rainfall  
foreach vvv in precip_dlr {
    xtset
    qui xtreg `vvv' i.year i.cityx, fe vce(cluster prv)
      est store `vvv'p1a
    qui xtreg `vvv' i.year i.cityx l.(lninclo_r lnpopx), fe vce(cluster prv)
      est store `vvv'p1b
 }
	
  esttab precip_dlrp1a precip_dlrp1b , ///
     order(1.cityx) keep(1.cityx) ///
     b(%4.3f) se(%4.3f) ///
     star(* 0.10 ** 0.05 *** 0.01) ///
     scalars(N r2_a)	
  
  
  
  
  
// Parallel Trend - Figure
egen city1 = max(city*(cyear<=2000)), by(id)
egen city2 = max(city*(cyear>=2001)), by(id)
gen city3 = 1*city1 + 2*city2

keep city3 year lninc_pcr lnexp_pcr 
rename (ln*_pcr) (*)

	collapse (mean) inc exp, by(year city3)
reshape wide inc exp, i(year) j(city3)

* Per capita income
twoway ///
   (connected inc0 year, lw(thick) ms(none)) ///
   (connected inc1 year, lw(thick) lp(dash) ms(none)) ///
   (connected inc2 year, lw(thick) ms(Oh) msize(large)) ///
   , ///
   graphr(color(white)) ///
   xline(2001, lp(dash) lc(gs0)) ///
   text(7.9 2001 "R.A.9009 ", place(sw)) ///
   ytitle("Constant 20000 PhP, log scale") xtitle("Year") ///
   ylabel(`=ln(300)' "300" `=ln(550)' "550" `=ln(1000)' "1,000" `=ln(1700)' "1,700" `=ln(3000)' "3,000") ///
   legend(pos(5) ring(0) cols(1) symx(*0.8) region(color(none)) ///
      order(2 "Cities ratified pre-2001" 3 "Cities ratified post-2001" 1 "Municipalities" - -))

* Per capita expenditure
twoway ///
   (connected exp0 year, lw(thick) ms(none)) ///
   (connected exp1 year, lw(thick) lp(dash) ms(none)) ///
   (connected exp2 year, lw(thick) ms(Oh) msize(large)) ///
   , ///
   graphr(color(white)) ///
   xline(2001, lp(dash) lc(gs0)) ///
   text(7.9 2001 "R.A.9009 ", place(sw)) ///
   ytitle("Constant 20000 PhP, log scale") xtitle("Year") ///
   ylabel(`=ln(300)' "300" `=ln(550)' "550" `=ln(1000)' "1,000" `=ln(1700)' "1,700" `=ln(3000)' "3,000") ///
   legend(pos(5) ring(0) cols(1) symx(*0.8) region(color(none)) ///
      order(2 "Cities ratified pre-2001" 3 "Cities ratified post-2001" 1 "Municipalities" - -))
  

  
  
  
