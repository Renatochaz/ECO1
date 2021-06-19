** Open
. import delimited "G:\Meu Drive\GIT\ECO1\v2_wins_quint_yearly_full_sample.csv"

*. drop if kz_quintile == 2
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


global controle_gmm d_at cv tam dum_setor* 

 ************ [FULL SAMPLE] ************

 ***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle_gmm, gmmstyle(L.(inv_atL1), lag (0 0) collapse) gmmstyle(L.(fc_at), lag (1 .)) gmmstyle((cv), lag (4 4) collapse) gmmstyle(L.(d_at), lag (2 2) collapse) gmmstyle((tam), lag (2 2) collapse) robust small twostep h(2)
est store Amostra_completa1
 ***MODEL WITH SQUARED INVESTMENT***
** Working model 
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle_gmm, gmmstyle(L.(inv_atL1 SQinv_atL1), lag (3 .) collapse) gmmstyle(L.(fc_at), lag (1 .)) gmmstyle((cv), lag (3 3) collapse) gmmstyle(L.(d_at), lag (2 2) collapse) gmmstyle((tam), lag (2 2) collapse) robust small twostep h(2)
est store Amostra_completa2


************ [KZ UNCONSTRAINED SAMPLE]************

***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle_gmm if kz_quintile==0, gmmstyle(L.(inv_atL1), lag (0 .) collapse) gmmstyle(L.(fc_at), lag (1 .)) gmmstyle((cv), lag (4 .) collapse) gmmstyle(L.(d_at), lag (4 .) collapse) gmmstyle((tam), lag (3 .) collapse) robust small twostep h(2)
est store Não_restritas1

 ***MODEL WITH SQUARED INVESTMENT*** CV(0 .) L.FC(2 .) (L.FC (3 .) no collapse)
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle_gmm if kz_quintile==0, gmmstyle(L.(inv_atL1), lag (0 .) collapse) gmmstyle(L.(SQinv_atL1), lag (0 .) collapse)  gmmstyle(L.(fc_at), lag (2 .)) gmmstyle((cv), lag (3 3) collapse) gmmstyle(L.(d_at), lag (2 2) collapse) gmmstyle((tam), lag (2 2) collapse) robust small twostep h(2)
 est store Não_restritas2



************ [KZ CONSTRAINED SAMPLE]************

***MODEL WITHOUT SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 ln_pu fc_at $controle_gmm if kz_quintile==1, gmmstyle(L.(inv_atL1), lag (1 .) collapse) gmmstyle(L.(fc_at), lag (2 3) ) gmmstyle((cv), lag (4 5) collapse) gmmstyle(L.(d_at), lag (0 .) collapse) gmmstyle((tam), lag (0 0) collapse) robust small twostep h(2)
est store Restritas1

xtabond2 inv_at inv_atL1 ln_pu fc_at $controle_gmm if kz_quintile==1, gmmstyle(L.(inv_atL1), lag (3 4) collapse) gmmstyle(L.(fc_at), lag (2 2) ) gmmstyle((cv), lag (0 0)) gmmstyle(L.(d_at), lag (0 .) collapse) gmmstyle((tam), lag (0 0) collapse) robust small twostep h(2)
***MODEL WITH SQUARED INVESTMENT***
xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at $controle_gmm if kz_quintile==1, gmmstyle(L.(inv_atL1), lag (1 .) collapse) gmmstyle (L.(SQinv_atL1), lag (0 .) collapse) gmmstyle(L.(fc_at), lag (2 3)) gmmstyle((cv), lag (4 4) collapse) gmmstyle(L.(d_at), lag (0 .) collapse) gmmstyle((tam), lag (0 0) collapse) robust small twostep h(2)
est store Restritas2



outreg2 [Amostra_completa1 Amostra_completa2 Não_restritas1 Não_restritas2] using results9.doc, /*
*/ se bdec(3) e(ar1p ar2p hansenp sarganp) word replace dec(4) symbol (***,**,*) alpha(0.01, 0.05, 0.1) /*
*/ keep(inv_atL1 SQinv_atL1 ln_pu fc_at d_at cv tam)
