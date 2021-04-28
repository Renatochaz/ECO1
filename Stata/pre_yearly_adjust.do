** Open
. import delimited "G:\Meu Drive\GIT\ECO1\pre_yearly_full_sample.csv"

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
*/ gmmstyle(L.(inv_atL1), lag (2 .) collapse)/*
*/ gmmstyle(L.(fc_at), lag (2 .) collapse) /*
*/ gmmstyle((cv), lag (2 .) collapse) /*
*/ gmmstyle(L.(d_at), lag (0 3) collapse) /*
*/ gmmstyle((tam), lag (0 0) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store gmm1

 ***MODEL WITH SQUARED INVESTMENT***
** Working model 
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle2_gmm, /*
*/ gmmstyle(L.(inv_atL1 SQinv_atL1), lag (2 3) collapse)/*
*/ gmmstyle(L.(fc_at), lag (2 3) collapse) /*
*/ gmmstyle((cv), lag (2 3) collapse) /*
*/ gmmstyle(L.(d_at), lag (0 3) collapse) /*
*/ gmmstyle((tam), lag (0 0) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store gmm2

*** working table

************ [KZ UNCONSTRAINED SAMPLE]************

***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==0, /*
*/ gmmstyle(L.(inv_atL1), lag (0 .) collapse)/*
*/ gmmstyle(L.(fc_at), lag (3 .)) /*
*/ gmmstyle((cv), lag (0 .) collapse) /*
*/ gmmstyle(L.(d_at), lag (4 .) collapse) /*
*/ gmmstyle((tam), lag (2 4)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store gmm3
 
 ***MODEL WITH SQUARED INVESTMENT*** CV(0 .) L.FC(2 .) (L.FC (3 .) no collapse)
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==0, /*
*/ gmmstyle(L.(inv_atL1 SQinv_atL1), lag (0 .) collapse)/*
*/ gmmstyle(L.(fc_at), lag (3 .)) /*
*/ gmmstyle((cv), lag (0 .) collapse) /*
*/ gmmstyle(L.(d_at), lag (4 .) collapse) /*
*/ gmmstyle((tam), lag (2 4)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store gmm4

*** working table


*** Saved working model

************ [KZ CONSTRAINED SAMPLE]************

 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==1, /*
*/ gmmstyle(L.(inv_atL1), lag (1 3))/*
*/ gmmstyle(L.(fc_at), lag (2 .) collapse) /*
*/ gmmstyle((cv), lag (2 .) collapse) /*
*/ gmmstyle(L.(d_at), lag (1 .) collapse) /*
*/ gmmstyle((tam), lag (0 0) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store gmm5
 
 ***MODEL WITH SQUARED INVESTMENT*** L.FC (2~3 . COLLAPSE, (D 2 . COLLAPSE) (l.T 3 .) COLLAPSE) (L.CV 3 . collapse) (D 1 . collapse OR L.0 1)
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle2_gmm if kz_dum==1, /*
*/ gmmstyle(L.(inv_atL1 SQinv_atL1), lag (1 .))/*
*/ gmmstyle((fc_at), lag (3 .) collapse) /*
*/ gmmstyle(L.(cv), lag (3 .) collapse) /*
*/ gmmstyle(L.(d_at), lag (0 3)collapse ) /*
*/ gmmstyle(L.(tam), lag (3 .) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store gmm6

*** working table

************ TABULATE RESULTS************
outreg2 [gmm1 gmm2 gmm3 gmm4 gmm5 gmm6] using results.doc, /*
*/ se bdec(3) e(ar1p ar2p hansenp sarganp) word replace dec(4) symbol (***,**,*) alpha(0.01, 0.05, 0.1) /*
*/ keep(inv_atL1 SQinv_atL1 ln_pu fc_at d_at cv tam)



********************************************************************************
********************************************************************************
********************************************************************************
