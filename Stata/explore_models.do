************** Experience Session **************

xtabond2 inv_at L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmm(L.inv_at) iv(ln_pu fc_at d_at cv tam dum_setor*) h(1) nolevel small

xtabond2 inv_at L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmm(L.inv_at) iv(ln_pu fc_at d_at cv tam dum_setor*) nolevel robust

xtabond2 inv_at L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmm(L.(inv_at fc_at d_at cv tam)) iv(ln_pu dum_setor*) nolevel robust small

xtabond2 inv_at inv_atL1 SQinv_atL1 ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_atL1 SQinv_atL1) fc_at d_at cv tam, laglimits (1 3)) iv(ln_pu dum_setor*) robust small twostep h(2)
***********************************************************************************************************************

************** Second step estimation **************

************** Full model without squared **************

*** Working model, but FC is not significative
xtabond2 inv_at L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_at cv) fc_at d_at tam, lag (2 2) collapse)/*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

*Works with hansen and m2, but not sargan, and div is high
xtabond2 inv_at L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(cv, lag (2 2) collapse) /*
*/ gmmstyle(L.(tam), lag (0 0) ) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

*work table
xtabond2 inv_at L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(cv, lag (2 2) collapse) /*
*/ gmmstyle(L.(tam), lag (0 0) ) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

*******************************************************************

************** Full model with squared **************

*** Working model, but div is high
xtabond2 inv_at L.inv_at c.L.inv_at#c.L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(c.L.inv_at#c.L.inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle((cv), lag (2 2) collapse) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

*work table
*(2 2) structure for inv and d_at/inv level without collapse
xtabond2 inv_at L.inv_at c.L.inv_at#c.L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(c.L.inv_at#c.L.inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (0 0) ) /*
*/ gmmstyle(L.(d_at), lag (2 2)) /*
*/ gmmstyle((cv), lag (2 2) collapse) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

**don't pass ar test
xtabond2 inv_at L.inv_at c.L.inv_at#c.L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_at), lag (2 2))/*
*/ gmmstyle(L.(c.L.inv_at#c.L.inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle((d_at), lag (2 2)) /*
*/ gmmstyle(L.(cv), lag (1 3) collapse) /*
*/ gmmstyle(L.(tam), lag (1 3) collapse) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)

**interesting result
xtabond2 inv_at L.inv_at c.L.inv_at#c.L.inv_at ln_pu fc_at d_at cv tam dum_setor*, /*
*/ gmmstyle(L.(inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(c.L.inv_at#c.L.inv_at), lag (2 2) collapse)/*
*/ gmmstyle(L.(fc_at), lag (1 3) collapse) /*
*/ gmmstyle(L.(d_at), lag (2 2)) /*
*/ gmmstyle((cv), lag (2 2) collapse) /*
*/ gmmstyle(L.(tam), lag (0 0)) /*
*/ iv(ln_pu dum_setor*, equation(level)) robust small twostep h(2)
*******************************************************************
