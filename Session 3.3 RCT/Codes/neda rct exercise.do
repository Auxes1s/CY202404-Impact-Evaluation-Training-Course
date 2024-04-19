////////////////////////////////////////////////////////////////////////////////
// SESSION EXERCISE ON RANDOMIZED CONTROL TRIALS 

clear all 

use  "ipa-neda_rctexercise.dta"


/******************************************************************************
1. LOCALS */

loc balance age hhmemnum educ lit hunger6m											// Balance vars
loc outcome life_sat future_life_sat hhincome30 lh_asset loans						// Outcome vars


/******************************************************************************
2. BALANCE */

foreach m in `balance' {
	
	sum `m', detail
	
	areg `m' treatment, absorb(community)
	
	test treatment 
	
}

/******************************************************************************
3. REGRESSION */

foreach m in `outcome' {
	
	sum `m', detail
	
	areg `m' treatment, absorb(community)
	
	test treatment 
}

