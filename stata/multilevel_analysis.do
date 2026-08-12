version 18
clear all
set more off

* Companion implementation for the R-generated synthetic panel.
* Run R/01_generate_nested_data.R before this do-file.

capture confirm file "data/synthetic_longitudinal_outcomes.csv"
if _rc {
    display as error "Generate the synthetic longitudinal data first."
    exit 601
}

import delimited using "data/synthetic_longitudinal_outcomes.csv", ///
    clear varnames(1)

encode organization_id, gen(organization_n)
encode person_id, gen(person_n)

isid organization_n person_n time
assert inrange(time, 0, 2)
xtset person_n time
xtdescribe

quietly summarize baseline_score
generate baseline_centered = baseline_score - r(mean)

* Unconditional model for variance partitioning.
mixed outcome || organization_n: || person_n:, mle
estimates store unconditional
estat icc

* Random-intercept conditional model.
mixed outcome c.time i.program c.baseline_centered c.ses_z ///
    i.multilingual || organization_n: || person_n:, mle
estimates store conditional

* Growth model with a random organization time slope.
mixed outcome c.time##i.program c.baseline_centered c.ses_z ///
    i.multilingual || organization_n: time, ///
    covariance(unstructured) || person_n:, mle
estimates store growth

lrtest conditional growth
estat icc

margins program, at(time=(0 1 2))
marginsplot, ///
    title("Adjusted outcome trajectories") ///
    xtitle("Measurement wave") ///
    ytitle("Predicted outcome") ///
    recast(line) recastci(rarea)

predict fitted, fitted
predict residual, residuals
rvfplot, yline(0)

estimates table conditional growth, b(%7.3f) se(%7.3f) stats(N ll aic bic)
