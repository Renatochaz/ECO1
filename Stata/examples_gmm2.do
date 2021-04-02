. regress inv_at inv_atL1 ln_pu fc_at d_at cv tam dum_setor* dum_quarter*

. regress inv_at inv_atL1 SQinv_atL1 ln_pu fc_at d_at cv tam dum_setor* dum_quarter*

xtreg inv_at inv_atL1 ln_pu fc_at d_at cv tam dum_setor* dum_quarter*, fe

xtreg inv_at inv_atL1 SQinv_atL1 ln_pu fc_at d_at cv tam dum_setor* dum_quarter*, fe


