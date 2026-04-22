* load in the data *
use frmgham2.dta, clear

* descriptive statistics*
// describe
// codebook

* ====================================
* 3a: TABLE - 1 Descriptive statistics
* ====================================

// can format this better in final product

clear
use frmgham2.dta, clear

* ----- Labels -----
label define diab 0 "No Diabetes (N=4240)" 1 "Diabetes (N=114)"
label values diabetes diab

label define sexlbl 1 "Male" 2 "Female"
label values sex sexlbl

* create education with missing as category
gen educ_cat = educ
replace educ_cat = 5 if missing(educ) // added this to account for missing!!!

label define educlbl2 ///
    1 "0–11 years" ///
    2 "High school/GED" ///
    3 "Some college" ///
    4 "College graduate+" ///
    5 "Missing"
label values educ_cat educlbl2

label variable age "Age (years)"
label variable totchol "Total cholesterol (mg/dL)"
label variable sex "Sex"
label variable educ_cat "Education"

* ----- Table 1 -----
dtable age totchol i.sex i.educ_cat, by(diabetes) ///
    continuous(age totchol, statistic(mean sd)) ///
    nformat(%6.1f) ///
    sformat("(%s)" sd)

* ----- Export -----
collect export Table1.docx, replace

* ----- Get missing counts for reporting (add manually in table) -----
tabulate educ diabetes, missing


* =================================================
* 3b: K-M Survival Curve - Diabetes vs Non-diabetes
* =================================================

clear
use frmgham2.dta, clear

stset lasttime, failure(mi_fchd)

* ----- Labels -----
label define diab 0 "No Diabetes" 1 "Diabetes"
label values diabetes diab

* ----- Kaplan–Meier curve with risk table -----
sts graph, by(diabetes) ///
    title("Kaplan–Meier Survival by Diabetes Status", size(medlarge)) ///
    xtitle("Time (years)", size(medium)) ///
    ytitle("Survival probability", size(medium)) ///
    legend(order(1 "No Diabetes" 2 "Diabetes") ///
           position(6) ring(0) cols(1) size(small)) ///
    risktable(0 5 10 15 20, ///
              size(vsmall) ///
              title("Number at risk")) ///
    xlabel(0(5)20, labsize(small)) ///
    ylabel(0(.2)1, labsize(small) angle(horizontal)) ///
    plot1opts(lcolor(navy) lwidth(medthick)) ///
    plot2opts(lcolor(maroon) lwidth(medthick)) ///
    graphregion(color(white)) ///
    bgcolor(white)

* ----- Export figure -----
graph export KM_curve.png, replace


* ================================
* PART 3(c)
* ================================

clear
use frmgham2.dta, clear

* ----- survival setup -----
stset lasttime, failure(mi_fchd)

* labels
label define diab 0 "No Diabetes" 1 "Diabetes"
label values diabetes diab

* (i) Median survival + 95% CI
* Median is where S(t) = 0.50
stci, by(diabetes)
* "Median survival was not reached in the non-diabetic group. Among diabetics, median survival was 22.1 years (95% CI: 18.0, not estimable)." *

* (ii) Survival at 5, 10, 15, 20 years + CI
sts list, by(diabetes) at(5 10 15 20)

* (iii) Test differences at each time point
* Log-rank test overall (for reference)
sts test diabetes

* Individual time-point comparisons
* (use Cox model to estimate hazard difference proxy)
foreach t in 5 10 15 20 {
    di "===== Time = `t' years ====="
    
    * restrict to subjects at risk at time t
    gen fail_`t' = (lasttime <= `t' & mi_fchd==1)
    
    logistic fail_`t' diabetes
}

* Bonferroni threshold
display "Bonferroni alpha = 0.01"


* how to submit it: stata-mp -b do descriptive_km.do *
