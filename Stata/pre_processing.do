** Open
. import delimited "G:\Meu Drive\GIT\ECO1\yearly_full_sample.csv"

** Set sector and quarter dummies
. tabulate setor_economatica, gen(dum_setor)
. tabulate quarter_index, gen (dum_quarter)

* Only if needed ssc install outreg2
****** Using lagged controls without squared defased
*global controle_gmm l.d_at l.cv l.tam  dum_setor*
*global controle2_gmm d_at cv tam dum_setor*
*global controle3_gmm l.d_at cv l.tam dum_setor* 
*global controle4_gmm l.d_at cv tam dum_setor*


* set panel identifiers
encode cdigo, generate(firm_id)
xtset firm_id quarter
. cd "G:\Meu Drive\GIT\ECO1\Stata"

****Only if needed***
* set lagged squared dependent variable and interactive variable
*sort firm_id quarter
*by firm_id: gen inv_atL1 = inv_at[_n-1] if quarter==quarter[_n-1]+1
*sort firm_id quarter
*by firm_id: gen SQinv_atL1 = inv_atL1^2
*Only if needed gen pu_fc = ln_pu * fc_at

* set variable labels
*label variable inv_at "INV"
*label variable inv_at_l1 "L1.INV"
*label variable inv_at_l1_quad "L1.INV^2"
*label variable fc_at "FC"
*label variable d_at "D"
*label variable cv "CV"
*label variable ln_pu "PU"
*label variable tam "TAM"
*label variable ln_pu_fc_at "PU x FC"
