*** Models for the full model, without samples subsetted by financial constrain ***
global controle_gmm d_at cv tam dum_quarter* dum_setor2*

global controle3_gmm l.d_at cv l.tam dum_setor* 

* Working full model
xtabond2 l(0/1).inv_at fc_at ln_pu $controle3_gmm, gmmstyle(L.(inv_at) L.(cv ) fc_at d_at tam  , lag (2 2) collapse)/* 
*/ ivstyle(ln_pu dum_setor*) robust twostep small h(3)

xtabond2 l(0/1).inv_at inv_at_l1_quad fc_at ln_pu $controle3_gmm, gmmstyle(L.(inv_at) L.(inv_at_l1_quad) L.(cv ) L.(fc_at) L.(d_at) L.(tam) , lag (2 2) collapse)/*
 */ ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3) orthogonal


* Constrained model
xtabond2 l(0/1).inv_at inv_at_l1_quad fc_at ln_pu $controle3_gmm if kz_dum==1, /*
*/gmmstyle(L.(inv_at) L.(inv_at_l1_quad) L3.(fc_at) L3.(cv) L.(tam) d_at , lag (2 2) collapse) ivstyle(ln_pu dum_setor*, eq(level)) robust twostep small h(3) 


******* WORKING TABLE ***********
xtabond2 l(0/1).inv_at fc_at ln_pu $controle_gmm, gmmstyle(L1.(inv_at) cv fc_at d_at tam  , lag (0 1) collapse)/* 
*/ ivstyle(ln_pu dum_setor* dum_quarter*) robust twostep small h(2)
