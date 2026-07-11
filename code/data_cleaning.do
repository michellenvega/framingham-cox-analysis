clear
cls 

cd "Z:\Desktop\518A\Final Project" /*change pathway to yours*/ 
use frmgham2.dta 

 /* ############################### STEP 1: DATA CLEANING ########################## */
 
* creating formats for variables*
label define sexf 1 "Men" 2 "Women"
label define cursmokef 0 "Not current smoker" 1 "Current smoker"
label define diabetesf 0 "Not a diabetic" 1 "Diabetic"
label define bpmedsf 0 "Not currently used" 1 "Current Use" 
label define educf 1 "0-11 years" 2 "High School Diploma, GED"  3 "Some College, Vocational School" 4 "College (BS, BA) degree or more"
label define prevchdf 0 "Free of disease" 1 "Prevalent disease" 
label define prevapf 0 "Free of disease" 1 "Prevalent disease" 
label define prevmif 0 "Free of disease" 1 "Prevalent disease" 
label define prevstrkf 0 "Free of disease" 1 "Prevalent disease" 
label define prevhypf 0 "Free of disease" 1 "Prevalent disease" 
label define deathf 0 "Free of event"  1 "Event occurs"
label define anginaf 0 "Free of event"  1 "Event occurs"
label define hospmif 0 "Free of event"  1 "Event occurs"
label define mi_fchdf 0 "Free of event"  1 "Event occurs"
label define anychdf 0 "Free of event"  1 "Event occurs"
label define strokef 0 "Free of event"  1 "Event occurs"
label define cvdf 0 "Free of event"  1 "Event occurs"
label define hypertenf 0 "Free of event"  1 "Event occurs"

* applying created formats to variables in open data set*
label value sex sexf
label value cursmoke cursmokef
label value diabetes diabetesf
label value bpmeds bpmedsf
label value educ educf
label value prevchd prevchdf
label value prevap prevapf
label value prevmi prevmif
label value prevstrk prevstrkf
label value prevhyp prevhypf
label value death deathf
label value angina anginaf
label value hospmi hospmif
label value mi_fchd mi_fchdf
label value anychd anychdf
label value stroke strokef
label value cvd cvdf
label value hyperten hypertenf

* 2a. convert the time scale (timemifc) from days to years*
generate timemifc_yrs = timemifc/365.25

* 2b. Confine your analysis to baseline measurements of risk factors (period = 1)* 
keep if inlist(period, 1) 

* 2c. Remove individuals who have a survival time of zero from the analysis. n = 4354*
drop if timemifc_yrs == 0
