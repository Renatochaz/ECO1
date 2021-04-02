********************************************************************************
**PRE PROCESSING
********************************************************************************

** Open
. import delimited "G:\Meu Drive\GIT\ECO1\full_sample.csv"

** Set sector and quarter dummies
. tabulate setor_economatica, gen(dum_setor)
. tabulate quarter_index, gen (dum_quarter)

* Only if needed ssc install outreg2

* set panel identifiers
encode cdigo, generate(firm_id)
xtset firm_id quarter
. cd "G:\Meu Drive\GIT\ECO1\Stata"

*********Only if needed*********************************************************
******** set lagged squared dependent variable and interactive variable*********
*sort firm_id quarter
*by firm_id: gen inv_atL1 = inv_at[_n-1] if quarter==quarter[_n-1]+1
*sort firm_id quarter
*by firm_id: gen SQinv_atL1 = inv_atL1^2
*Only if needed gen pu_fc = ln_pu * fc_at
********************************************************************************









********************************************************************************
********************************************************************************
**THIS SECTION CONTAINS MODELS THAT PASS ALL TESTS AND ARE WITHIN EXPECTATIONS
**OF HOW POLICY UNCERTAINTY SHOULD AFFECT DIFFERENT SAMPLES, BUT THE 
**DYNAMIC INVESTMENT IS NOT WELL ADJUSTED, AND THE MODELS ARE NOT ROBUST TO CHANGE
********************************************************************************
********************************************************************************

global controle_gmm l.d_at cv l.tam dum_setor* 
 
 ************ [FULL SAMPLE] ************
 
 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 l(0/1).inv_at fc_at ln_pu $controle_gmm, /*
*/ gmmstyle(L.(inv_at cv) fc_at d_at tam , lag (2 2) collapse)/* 
*/ ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3)
est store full_sample1

 ***MODEL WITH SQUARED INVESTMENT***
 xtabond2 l(0/1).inv_at c.L.inv_at#c.L.inv_at fc_at ln_pu $controle_gmm, /*
 */ gmmstyle(L.(inv_at c.L.inv_at#c.L.inv_at cv fc_at d_at tam), lag (2 2) collapse) /*
 */ ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3) orthogonal
est store full_sample2


 
 ************ [KZ CONSTRAINED SAMPLE]************
 
 ***MODEL WITHOUT SQUARED INVESTMENT***
 xtabond2 l(0/1).inv_at fc_at ln_pu $controle_gmm if kz_dum==1,/*
 */ gmmstyle(L4.(inv_at) L.(fc_at cv tam) d_at , lag (2 2) collapse) /*
 */ ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3) orthogonal
est store constrained1

 ***MODEL WITH SQUARED INVESTMENT***
 xtabond2 l(0/1).inv_at c.L.inv_at#c.L.inv_at fc_at ln_pu $controle_gmm if kz_dum==1, /*
 */ gmmstyle(L.(inv_at c.L.inv_at#c.L.inv_at tam) L3.(fc_at cv) d_at, lag (2 2) collapse) /*
*/ ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3) 
est store constrained2



************ [KZ UNCONSTRAINED SAMPLE]************

 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 l(0/1).inv_at fc_at ln_pu $controle_gmm if kz_dum==0, /*
*/ gmmstyle(L4.(inv_at tam) L2.(d_at cv) fc_at, lag (2 2) collapse) /*
*/ ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3)
est store unconstrained1

 ***MODEL WITH SQUARED INVESTMENT***
 xtabond2 l(0/1).inv_at c.L.inv_at#c.L.inv_at fc_at ln_pu $controle_gmm if kz_dum==0, /*
*/ gmmstyle(L4.(inv_at c.L.inv_at#c.L.inv_at tam) L3.(d_at cv) fc_at, lag (2 2) collapse) /*
*/ ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3) 
est store unconstrained2


************ TABULATE RESULTS************
outreg2 [full_sample1 full_sample2 constrained1 constrained2 unconstrained1 unconstrained2] using prelim_results.doc, /*
*/ se bdec(3) e(ar1p ar2p hansenp sarganp) word replace dec(4) symbol (***,**,*) alpha(0.01, 0.05, 0.1) /*
*/ keep(l.inv_at c.L.inv_at#c.L.inv_at fc_at l.d_at d_at cv ln_pu l.tam tam)



********************************************************************************
********************************************************************************
********************************************************************************









********************************************************************************
********************************************************************************
**THIS SECTION CONTAINS MODELS THAT PASS AR2 AND ONLY HANSEN TEST

********************************************************************************
********************************************************************************

global controle2_gmm d_at cv tam dum_setor* 
 
 ************ [FULL SAMPLE] ************
 
 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at L.inv_at ln_pu fc_at $controle2_gmm, /*
*/ gmmstyle(L.(inv_at cv), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store adj_full_sample1

 ***MODEL WITH SQUARED INVESTMENT***
xtabond2 inv_at L.inv_at c.L.inv_at#c.L.inv_at ln_pu fc_at $controle2_gmm, /*
*/ gmmstyle(L.(inv_at c.L.inv_at#c.L.inv_at cv), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store adj_full_sample2
 
 ************ [KZ CONSTRAINED SAMPLE]************
 
 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at L.inv_at ln_pu fc_at $controle2_gmm if kz_dum==1, /*
*/ gmmstyle(L.(inv_at cv), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store adj_constrained1

 ***MODEL WITH SQUARED INVESTMENT***
xtabond2 inv_at L.inv_at c.L.inv_at#c.L.inv_at ln_pu fc_at $controle2_gmm if kz_dum==1, /*
*/ gmmstyle(L.(inv_at c.L.inv_at#c.L.inv_at cv), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
est store adj_constrained2


************ [KZ UNCONSTRAINED SAMPLE]************

 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at L.inv_at ln_pu fc_at $controle2_gmm if kz_dum==0, /*
*/ gmmstyle(L.(inv_at cv), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(3) orthogonal
est store adj_unconstrained1

 ***MODEL WITH SQUARED INVESTMENT***
xtabond2 inv_at L.inv_at c.L.inv_at#c.L.inv_at ln_pu fc_at $controle2_gmm if kz_dum==0, /*
*/ gmmstyle(L.(inv_at c.L.inv_at#c.L.inv_at cv), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(3) orthogonal
est store adj_unconstrained2


************ TABULATE RESULTS************
outreg2 [adj_full_sample1 adj_full_sample2 adj_constrained1 adj_constrained2 adj_unconstrained1 adj_unconstrained2] using adjusted_results.doc, /*
*/ se bdec(3) e(ar1p ar2p hansenp) word replace dec(4) symbol (***,**,*) alpha(0.01, 0.05, 0.1) /*
*/ keep(l.inv_at c.L.inv_at#c.L.inv_at fc_at l.d_at d_at cv ln_pu l.tam tam)



********************************************************************************
********************************************************************************
********************************************************************************
