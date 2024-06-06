local surname ROJAS

local firstname Luis 

local studentnumber 1003650676
cap log close _all
log using "ECO375_Assignment2_`surname'_`firstname'.log", replace text 
set more off 	
clear			
display "ECO375_Assignment2" _n "`surname' `firstname' `studentnumber'" _n c(current_date) c(current_time)
																		
/* 	====================================
	=========== Assignment 2 ===========
	==================================== */
	
	
use "Ayres.dta", clear		// Call the file to be used 
datasignature				// Checks data integrity 		
describe					// describes the data to be analized

gen ln_vio = ln(vio)
label variable ln_vio "Natural log of vio"
gen ln_rob = ln(rob)
label variable ln_vio "Natural log of Robbery rates"
gen ln_mur = ln(mur)
label variable ln_vio "Natural log of Murder rates"
gen ln_avginc = ln(avginc)
label variable ln_vio "Natural log of Average Income"

* Table 1 Summary Statistics

estpost sum vio rob mur shall incarc_rate density avginc pop pb1064 pw1064 pm1029 ln_vio ln_rob ln_mur ln_avginc

#delimit ;

esttab using "TABLE 1.2.rtf", replace

		cells("mean(fmt(3)) sd(fmt(3)) max(fmt(1)) min(fmt(1))")
		nomtitle nonumber
		coeflabels(shall "{\i shall}" 
				    incarc_rate "{\i incarc_rate}" 
					density "{\i density}" avginc "{\i avginc}" 				   pop "{\i pop}"  
					pb1064 "{\i pb1064}" pw1064 "{\i pw1064}" 
					pm1029 "{\i pm1029}" 
					vio "{\i vio}" rob "{\i rob}" mur "{\i mur}"
					ln_vio "{\i Ln(vio)}" ln_rob "{\i Ln(rob)}" 
					ln_mur "{\i Ln(mur)}" 
					ln_avginc "{\i Ln(avginc)}") 
		
		title({\b Table 1}: {\i Summary Statistics});

#delimit cr

* Define panel data

tsset stateid year

* Work for table 2 starts here

* Simple Regression 

reg ln_vio shall, robust
estimates store ols
estadd local fixeds "No", replace
estadd local fixedy "No", replace
estadd local clusters "No", replace

* Simple Regression with controls

reg ln_vio shall incarc_rate density avginc pop pb1064 pw1064 pm1029, robust 
estimates store ols_w_ctrls 
quietly estadd local fixeds "No", replace
quietly estadd local fixedy "No", replace
quietly estadd local clusters "No", replace
test pm1029 = pw1064 = pb1064 = 0
estadd scalar p_value2 = r(p)
estadd scalar Fs2 = r(F)



* Simple Regression with controls and State Fixed effects and cluster std errors

xtreg ln_vio shall incarc_rate density avginc pop pb1064 pw1064 pm1029, fe vce(cluster stateid)
estimates store state_effects 
quietly estadd local fixeds "Yes", replace
quietly estadd local fixedy "No", replace
quietly estadd local clusters "Yes", replace
test pm1029 = pw1064 = pb1064 = 0
estadd scalar p_value2 = r(p)
estadd scalar Fs2 = r(F)



* Simple Regression with controls and State and Time Fixed effects and cluster std errors at the state level

xtreg ln_vio shall incarc_rate density avginc pop pb1064 pw1064 pm1029 i.year, fe vce(cluster stateid)
estimates store state_time

testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)
test pm1029 = pw1064 = pb1064 = 0
estadd scalar p_value2 = r(p)
estadd scalar Fs2 = r(F)
quietly estadd local fixeds "Yes", replace
quietly estadd local fixedy "Yes", replace
quietly estadd local clusters "Yes", replace

* TABLE 2

#delimit ;
esttab ols ols_w_ctrls state_effects state_time using "Table 2.rtf",
	replace
	b(4) se(4) 
	
	keep(shall incarc_rate density avginc pop pb1064 pw1064 pm1029)
	
	coeflabels(shall "{\i shall}" 
				incarc_rate "{\i incarc_rate}"  				
				density "{\i density}" 
				avginc "{\i avginc}" 
				pop "{\i pop}"  
				pb1064 "{\i pb1064}" pw1064 "{\i pw1064}" 
				pm1029 "{\i pm1029}")
	
	stats(fixeds fixedy clusters Fs1 p_value1 Fs2 p_value2 r2_a, 
			label("{\i States Effects}" "{\i Time Effects}" 
				 "{\i Clustered s.e.'s}" 
				 "{\i Time Effects = 0}" "(p-value)"
				 "{\i Demogrphcs = 0}" "(p-value)"
				 "{\i R{\super 2} adj}"))
				 
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
	title({\b Table 2}: {\i Regression Analysis of the log of the 			violent crime rate and shall-issue laws})

	addnotes("These regressions are estimated using the panel data for all the states in the USA plus the district of Columbia. The dependent variable is the natural log of Vio. All regressions use data from 1977 to 1999. Clustered standard errors at the state level. Test on year dummy variables coefficients under the null Time Effects = 0. This panel data can be encountered in Ayres.dta. Note that the standard errors are given in parenthesis under coefficients and p-values are given in parenthesis under the F-statistics. The significance codes used are given at the {\i + p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001} significance levels. ")
	nomtitles;
	

#delimit cr

*Work for Table 3 Starts here

* Model 5
xtreg ln_vio shall incarc_rate density pop ln_avginc pb1064 pw1064 pm1029 i.year, fe vce(cluster stateid)
estimates store Model5 
test pm1029 = pw1064 = pb1064 = 0
estadd scalar p_value2 = r(p)
estadd scalar Fs2 = r(F)

test pw1064 = pb1064 = 0
estadd scalar p_value3 = r(p)
estadd scalar Fs3 = r(F)

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)


* Model 6
xtreg ln_rob shall incarc_rate density ln_avginc pop pb1064 pw1064 pm1029 i.year, fe vce(cluster stateid)
estimates store Model6
test pm1029 = pw1064 = pb1064 = 0
estadd scalar p_value2 = r(p)
estadd scalar Fs2 = r(F)
estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)

* Model 7 
xtreg ln_mur shall incarc_rate density ln_avginc pop pb1064 pw1064 pm1029 i.year, fe vce(cluster stateid)
estimates store Model7
test pm1029 = pw1064 = pb1064 = 0
estadd scalar p_value2 = r(p)
estadd scalar Fs2 = r(F)

test pw1064 = pb1064 = 0
estadd scalar p_value3 = r(p)
estadd scalar Fs3 = r(F)

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)

*TABLE 3

#delimit ;
esttab Model5 Model6 Model7 using "TABLE 3.rtf", replace

	b(4) se(4) 
	
	keep(shall incarc_rate density ln_avginc pop pb1064 
		pw1064 pm1029)
	
	coeflabels(shall "{\i shall}" 
				incarc_rate "{\i incarc_rate}"  				
				density "{\i density}" 
				ln_avginc "{\i Ln_avginc}" 
				pop "{\i pop}"  
				pb1064 "{\i pb1064}" pw1064 "{\i pw1064}" 
				pm1029 "{\i pm1029}")
	
	stats(fixeds fixedy clusters 
			Fs1 p_value1 Fs2 p_value2 Fs3 p_value3 r2_a, 
			label("{\i States Effects}" 
			"{\i Time Effects}"   				
			"{\i Clustered s.e.'s}" 
			"{\i Time Effects = 0}" "(p-value)"
			"{\i Demogrphcs = 0}" "(p-value)"
			"{\i Race = 0}" "(p-value)"
			"{\i R{\super 2} adj}"))
	
	title({\b Table 3}: {\i Regression Analysis for the log of 
	the three types of crimes and shall-issue laws})
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
	nonumber
	mtitles((5)Vio (6)Robbery (7)Murder)

	addnotes("These regressions are estimated using the panel data for all the states in the USA plus the district of Columbia. The dependent variables are the natural log of Vio, Rob and Mur, respectivelly.  All regressions use data from 1977 to 1999.  Clustered standard errors at the state level. Test on year dummy variables coefficients under the null Time Effects = 0. Test on pb1064 = pw1064 = pm1029 = 0 under the null Demographics = 0 and pb1064 = pw1064 = 0 under the null Race = 0. This panel data can be encountered in Ayres.dta. Note that the standard errors are given in parenthesis under coefficients and p-values are given in parenthesis under the F-statistics. The significance codes used are given at the {\i + p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001} significance levels.");
	
#delimit cr

*Work for table 4 starts here

* Model 8
xtreg ln_vio shall incarc_rate density pop ln_avginc pm1029 i.year, fe vce(cluster stateid)
estimates store Model8 

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)

* Model 9 
xtreg ln_rob shall incarc_rate density ln_avginc pop i.year, fe vce(cluster stateid)
estimates store Model9

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)

* Model 10

xtreg ln_mur shall incarc_rate density ln_avginc pop pm1029 i.year, fe vce(cluster stateid)
estimates store Model10

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)


*TABLE 4 

#delimit ;
esttab Model8 Model9 Model10 using "TABLE 4.rtf", replace

	b(4) se(4) 
	
	keep(shall incarc_rate density ln_avginc pop pm1029)
	
	coeflabels(shall "{\i shall}" 
				incarc_rate "{\i incarc_rate}"  				
				density "{\i density}" 
				ln_avginc "{\i Ln_avginc}" 
				pop "{\i pop}"  
				pm1029 "{\i pm1029}")
	
	stats(fixeds fixedy clusters 
			Fs1 p_value1 r2_a, 
			label("{\i States Effects}" 
			"{\i Time Effects}"   				
			"{\i Clustered s.e.'s}" 
			"{\i Time Effects = 0}"  "(p-value)"
			"{\i R{\super 2} adj}"))
	
	title({\b Table 4}: {\i Regression Analysis of the log of the 
	three types of crime and shall-issue laws, some variables 
	omitted.})
	nonumber
	mtitles((8)Vio (9)Robbery (10)Murder)
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
	
	addnotes("These regressions are estimated using the panel data for all the states in the USA plus the district of Columbia. The dependent variables are the natural log of Vio, Rob and Mur, respectivelly. All regressions use data from 1977 to 1999.  Clustered standard errors at the state level. Test on year dummy variables coefficients under the null Time Effects = 0. This panel data can be encountered in Ayres.dta. Note that the standard errors are given in parenthesis under coefficients and p-values are given in parenthesis under the F-statistics. The significance codes used are given at the {\i + p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001} significance levels.");
#delimit cr


* Work for Table 5 starts here

* Model 11

xtreg ln_vio shall density pop ln_avginc pm1029 i.year, fe vce(cluster stateid)
estimates store Model11 

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)

* Model 12 
xtreg ln_rob shall density ln_avginc pop i.year, fe vce(cluster stateid)
estimates store Model12

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)

* Model 13

xtreg ln_mur shall density ln_avginc pop pm1029 i.year, fe vce(cluster stateid)
estimates store Model13

estadd local fixeds "Yes", replace
estadd local fixedy "Yes", replace
estadd local clusters "Yes", replace
testparm i.year
estadd scalar p_value1 = r(p)
estadd scalar Fs1 = r(F)


*TABLE 5

#delimit ;
esttab Model11 Model12 Model13 using "TABLE 5.rtf", replace

	b(4) se(4) 
	
	keep(shall density ln_avginc pop pm1029)
	
	coeflabels(shall "{\i shall}" 
				density "{\i density}" 
				ln_avginc "{\i Ln_avginc}" 
				pop "{\i pop}"  
				pm1029 "{\i pm1029}")
	
	stats(fixeds fixedy clusters 
			Fs1 p_value1 r2_a, 
			label("{\i States Effects}" 
			"{\i Time Effects}"   				
			"{\i Clustered s.e.'s}" 
			"{\i Time Effects = 0}"  "(p-value)"
			"{\i R{\super 2} adj}"))
	
	title({\b Table 5}: {\i Regression Analysis of the log of the 
	three types of crime and shall-issue laws, incarc_rate 
	omitted.})
	nonumber
	mtitles((11)Vio (12)Robbery (13)Murder)
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

	addnotes("These regressions are estimated using the panel data for all the states in the USA plus the district of Columbia. The dependent variables are the natural log of Vio, Rob and Mur, respectivelly. All regressions use data from 1977 to 1999.  Clustered standard errors at the state level. Test on year dummy variables coefficients under the null Time Effects = 0. This panel data can be encountered in Ayres.dta. Note that the standard errors are given in parenthesis under coefficients and p-values are given in parenthesis under the F-statistics. The significance codes used are given at the {\i + p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001} significance levels.");
#delimit cr

log close				// closes the log file

/* 	=============================================================
	===================== 	END OF SCRIPT   =====================
	============================================================= */
