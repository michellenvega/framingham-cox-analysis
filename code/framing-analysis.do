clear
cls

use final_2026.dta

* ============================================================
* LABELS
* ============================================================

label define sexf 1 "Men" 2 "Women"
label define cursmokef 0 "No" 1 "Yes"
label define diabetesf 0 "No" 1 "Yes"
label define educf 1 "0-11 years" 2 "High School Diploma, GED" 3 "Some College, Vocational School" 4 "College (BS, BA) degree or more"
label define mi_fchdf 0 "Free of event" 1 "Event occurs"

label value sex sexf
label value cursmoke cursmokef
label value diabetes diabetesf
label value educ educf
label value mi_fchd mi_fchdf


* ============================================================
* 3A: TABLE 1 - Descriptive Statistics
* ============================================================

label define diab_t1 0 "No Diabetes (N=4240)" 1 "Diabetes (N=114)"
label values diabetes diab_t1

label define sexlbl 1 "Male" 2 "Female"
label values sex sexlbl

* create education with missing as category
gen educ_cat = educ
replace educ_cat = 5 if missing(educ)

label define educlbl2 ///
    1 "0-11 years" ///
    2 "High school/GED" ///
    3 "Some college" ///
    4 "College graduate+" ///
    5 "Missing"
label values educ_cat educlbl2

label variable age "Age (years)"
label variable totchol "Total cholesterol (mg/dL)"
label variable sex "Sex"
label variable educ_cat "Education"

dtable age totchol i.sex i.educ_cat, by(diabetes) ///
    continuous(age totchol, statistic(mean sd)) ///
    nformat(%6.1f) ///
    sformat("(%s)" sd)

collect export Table1.docx, replace

tabulate educ diabetes, missing
misstable summarize age totchol sex educ diabetes


* ============================================================
* 3B: Kaplan-Meier Survival Curve
* ============================================================

stset lasttime, failure(mi_fchd)

label define diab_km 0 "No Diabetes" 1 "Diabetes"
label values diabetes diab_km

sts graph, by(diabetes) ///
    title("Kaplan-Meier Survival by Diabetes Status", size(medlarge)) ///
    xtitle("Time (years)", size(medium)) ///
    ytitle("Survival probability", size(medium)) ///
    legend(order(1 "No Diabetes" 2 "Diabetes") ///
           position(6) ring(0) cols(1) size(small)) ///
    risktable(0 5 10 15 20, ///
	    order(1 "No Diabetes" 2 "Diabetes") ///
              size(vsmall) ///
              title("Number at risk")) ///
    xlabel(0(5)20, labsize(small)) ///
    ylabel(0(.2)1, labsize(small) angle(horizontal)) ///
    plot1opts(lcolor(navy) lwidth(medthick)) ///
    plot2opts(lcolor(maroon) lwidth(medthick)) ///
    graphregion(color(white)) ///
    bgcolor(white)

graph export KM_curve.png, replace


* ============================================================
* 3C: Median Survival + Survival at Time Points + Log-rank
* ============================================================

* (i) Median survival + 95% CI
stci, by(diabetes)

* (ii) Survival at 5, 10, 15, 20 years + CI
sts list, by(diabetes) at(5 10 15 20)

* (iii) Log-rank test overall
sts test diabetes

* Individual time-point comparisons
foreach t in 5 10 15 20 {
    di "===== Time = `t' years ====="
    sts test diabetes if lasttime >= `t' | (lasttime < `t' & mi_fchd == 1)
}

display "Bonferroni alpha = 0.01"


* ============================================================
* 3E(i): Univariate Cox Models
* ============================================================

summarize totchol
generate totchol_centered = (totchol - 236.854) / 10 /*centering and scaling */

stcox i.diabetes, nolog
stcox totchol_centered, nolog /* univariate cox models*/


* ============================================================
* 3E(ii): Martingale Residuals - Linearity Check
* ============================================================

/*MODEL DIAGNOSTICS- Attempt 1: need to transform totchol */

stcox, estimate nolog
predict mg, mgale

twoway (scatter mg totchol_centered, msize(tiny)) ///
       (lowess mg totchol_centered), ///
       xtitle("Centered total cholesterol (mg/dL)") ///
       ytitle("Martingale residuals") ///
       legend(off)

/*MODEL DIAGNOSTICS- Attempt 2: used going forward*/
generate log_totchol = log(totchol)

stcox, estimate nolog
predict cg, mgale

twoway (scatter cg log_totchol, msize(tiny)) ///
       (lowess cg log_totchol), ///
       xtitle("Log-transformed total cholesterol (mg/dL)") ///
       ytitle("Martingale residuals") ///
       legend(off)


/*MODEL DIAGNOSTICS- Attempt 3 (not needed since you don't center log_totchol)

generate log_totchol_centered = (log_totchol - 5.450208) / 10

stcox log_totchol_centered, nolog
predict yg, mgale
scatter yg log_totchol_centered, msize(tiny)
lowess yg log_totchol_centered */


* ============================================================
* 3F: Multivariable Model + Confounder Assessment
* ============================================================

stcox diabetes log_totchol, nolog /*diab: 3.91, log_chol: 7.02*/
stcox diabetes log_totchol age, nolog /*diab: 3.22, log_chol: 4.36*/
stcox diabetes log_totchol i.sex, nolog /* diab: 3.82, log_chol: 8.99*/
stcox diabetes log_totchol i.educ, nolog /* diab: 3.73, log_chol: 7.13*/

stcox diabetes log_totchol age i.sex, nolog /*final multivariable model: age and sex as confounders >= 10% change in base HR*/


* ============================================================
* 3F(i): PH Assumption - Schoenfeld Residuals
* ============================================================

stcox diabetes log_totchol age i.sex, nolog
estat phtest
estat phtest, detail /* evidence of PH violation by sex and log_totchol */

** (1) stratifying by sex
stcox diabetes log_totchol age, strata(sex) nolog
estat phtest, detail /* still evidence of PH violation --> log_totchol as a tvc*/

** (2) log_totchol as tvc
stcox diabetes log_totchol age, tvc(log_totchol) texp(log(_t)) strata(sex) nolog


* ============================================================
* 3F(ii): Model Diagnostics on Final Model
* ============================================================

** (1) Martingale residuals
stcox diabetes log_totchol, strata(sex) nolog
predict mg2, mgale

twoway (scatter mg2 age, msize(tiny)) ///
       (lowess mg2 age), ///
       xtitle("Age (years)") ///
       ytitle("Martingale residuals") ///
       legend(off)

** (2) Cox-Snell residuals
stcox diabetes
estat gofplot, title("Diabetes only", size(vsmall)) name(g1, replace)

stcox log_totchol
estat gofplot, title("Log-transformed cholesterol only", size(vsmall)) name(g2, replace)

stcox log_totchol diabetes
estat gofplot, title("Diabetes + log-transformed cholesterol", size(vsmall)) name(g3, replace)

stcox log_totchol diabetes age
estat gofplot, title("Diabetes + log-transformed cholesterol + age", size(vsmall)) name(g4, replace)

stcox log_totchol diabetes sex
estat gofplot, title("Diabetes + log-transformed cholesterol + sex", size(vsmall)) name(g5, replace)

stcox log_totchol diabetes age, strata(sex)
estat gofplot, title("Diabetes + log-transformed cholesterol + age (stratified by sex)", size(vsmall)) name(g6, replace)

graph combine g1 g2 g3 g4 g5 g6, cols(3)

** (3) Deviance residuals
stcox diabetes log_totchol age, strata(sex)
predict dres, deviance
scatter dres _t, msize(tiny) ///
    yline(0) ///
    xtitle("Time") ///
    ytitle("Deviance residuals") ///
    title("Deviance residuals vs time") ///
    legend(off)


/** (4) Likelihood displacement
stcox diabetes log_totchol age, strata(sex)
predict ld, ldisplace
sort ld
list _t _d ld in -10/l
scatter ld _t, msize(tiny) ///
    xtitle("Years since baseline exam") ///
    ytitle("Likelihood displacement") ///
    title("Likelihood displacement vs time") ///
    legend(off)*/


** (5) Difference in beta
stcox diabetes log_totchol age, strata(sex)
predict df*, dfbeta

scatter df1 _t, ///
    yline(0) ///
    xtitle("Years since baseline exam") ///
    ytitle("DFBETA for diabetes") ///
	name(f1, replace)

scatter df2 _t, ///
    yline(0) ///
    xtitle("Years since baseline exam") ///
    ytitle("DFBETA for log(totchol)") ///
	name(f2, replace)

scatter df3 _t, ///
    yline(0) ///
    xtitle("Years since baseline exam") ///
    ytitle("DFBETA for age") ///
	name(f3, replace)

graph combine f1 f2 f3, cols(1)


* ============================================================
* 3G: Interaction - Cholesterol x Diabetes
* ============================================================

stcox i.diabetes##c.log_totchol c.age, strata(sex) nolog /* p = 0.136 (no effect modification) */
	**didnt use extended cox model


* ============================================================
* 3H: Categorical Cholesterol Analysis
* ============================================================

gen chol_cat = .
replace chol_cat = 1 if totchol < 200
replace chol_cat = 2 if totchol >= 200 & totchol < 240
replace chol_cat = 3 if totchol >= 240

label define chol_lbl 1 "Healthy (<200)" 2 "At risk (200-239)" 3 "Dangerous (240+)"
label values chol_cat chol_lbl

***unadjusted association
stcox i.chol_cat, nolog

***adjusted association
stcox diabetes i.chol_cat age, strata(sex) nolog

sts test chol_cat, trend

sts graph, by(chol_cat) ///
    legend(order(1 "Healthy (<200)" 2 "At risk (200-239)" 3 "Dangerous (240+)")) ///
    title("Kaplan-Meier Survival by Cholesterol Category, Unadjusted") ///
    ytitle("Survival Probability") ///
    xtitle("Years since baseline exam")
