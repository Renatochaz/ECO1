** Open
. import delimited "G:\Meu Drive\GIT\ECO1\yearly_full_sample.csv"

** Set sector and quarter dummies
. tabulate setor_economatica, gen(dum_setor)
*. tabulate quarter_index, gen (dum_quarter)

* Only if needed ssc install outreg2
****Only if needed***
* set lagged squared dependent variable and interactive variable

* set panel identifiers
encode cdigo, generate(firm_id)
xtset firm_id year
. cd "G:\Meu Drive\GIT\ECO1\Stata"

sort firm_id year
by firm_id: gen inv_atL1 = inv_at[_n-1] if year==year[_n-1]+1
sort firm_id year
by firm_id: gen SQinv_atL1 = inv_atL1^2
*Only if needed gen pu_fc = ln_pu * fc_at


global controle2_gmm d_at cv tam dum_setor* 
 
 ************ [FULL SAMPLE] ************
 
 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle2_gmm, /*
*/ gmmstyle(L.(inv_atL1), lag (2 5) collapse)/*
*/ gmmstyle(L.(fc_at), lag (2 2) collapse) /*
*/ gmmstyle((cv), lag (1 1) collapse) /*
*/ gmmstyle(L.(d_at), lag (1 1) collapse) /*
*/ gmmstyle((tam), lag (0 0) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

 ***MODEL WITH SQUARED INVESTMENT***
** Working model (too high fc and d coefficients)
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle2_gmm, /*
*/ gmmstyle(L.(inv_atL1 SQinv_atL1), lag (2 .) collapse)/*
*/ gmmstyle(L.(fc_at), lag (2 2) collapse) /*
*/ gmmstyle((cv), lag (1 1) collapse) /*
*/ gmmstyle(L.(d_at), lag (0 1) collapse) /*
*/ gmmstyle((tam), lag (0 0) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

*** working table



************ [KZ UNCONSTRAINED SAMPLE]************

***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==0, /*
*/ gmmstyle(L.(inv_atL1), lag (1 .) collapse)/*
*/ gmmstyle((fc_at), lag (1 1) collapse) /*
*/ gmmstyle((cv), lag (1 1) collapse) /*
*/ gmmstyle(L.(d_at), lag (1 1) collapse) /*
*/ gmmstyle((tam), lag (0 2) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
 
 ***MODEL WITH SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==0, /*
*/ gmmstyle(L.(inv_atL1 SQinv_atL1), lag (1 .) collapse)/*
*/ gmmstyle((fc_at), lag (1 1) collapse) /*
*/ gmmstyle((cv), lag (1 1) collapse) /*
*/ gmmstyle(L.(d_at), lag (1 1) collapse) /*
*/ gmmstyle((tam), lag (0 2) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

*** working table

************ [KZ CONSTRAINED SAMPLE]************

***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==1, /*
*/ gmmstyle((inv_atL1), lag (1 1) collapse)/*
*/ gmmstyle((fc_at), lag (2 2) collapse) /*
*/ gmmstyle((cv), lag (2 2) collapse) /*
*/ gmmstyle((d_at), lag (1 1) collapse) /*
*/ gmmstyle((tam), lag (2 2) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
 
 ***MODEL WITH SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==1, /*
*/ gmmstyle(L.(inv_atL1), lag (1 3) collapse)/*
*/ gmmstyle(L.(SQinv_atL1), lag (1 1) collapse)/*
*/ gmmstyle((fc_at), lag (1 1) collapse) /*
*/ gmmstyle((cv), lag (1 1) collapse) /*
*/ gmmstyle(L.(d_at), lag (1 1)) /*
*/ gmmstyle((tam), lag (0 2) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)


*** working table


************ TABULATE RESULTS************
outreg2 [full_sample1 full_sample2 constrained1 constrained2 unconstrained1 unconstrained2] using prelim_results.doc, /*
*/ se bdec(3) e(ar1p ar2p hansenp sarganp) word replace dec(4) symbol (***,**,*) alpha(0.01, 0.05, 0.1) /*
*/ keep(l.inv_at c.L.inv_at#c.L.inv_at fc_at l.d_at d_at cv ln_pu l.tam tam)



********************************************************************************
********************************************************************************
********************************************************************************


