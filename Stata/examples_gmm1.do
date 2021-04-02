. webuse abdata
. regress n nL1 nL2 w wL1 k kL1 kL2 ys ysL1 ysL2 yr*

. xtreg n nL1 nL2 w wL1 k kL1 kL2 ys ysL1 ysL2 yr*, fe

. ivregress 2sls D.n (D.nL1= nL2) D.(nL2 w wL1 k kL1 kL2 ys ysL1 ysL2 yr1979 /*
*/ yr1980 yr1981 yr1982 yr1983)

. xtabond2 n L.n L2.n w L.w L(0/2).(k ys) yr*, gmm(L.n)/*
*/ iv(w L.w L(0/2).(k ys) yr*) h(1) nolevel small

. xtabond2 n L.n L2.n w L.w L(0/2).(k ys) yr*, gmm(L.n)/*
*/ iv(w L.w L(0/2).(k ys) yr*) nolevel robust

. xtabond2 n L.n L2.n w L.w L(0/2).(k ys) yr*, gmm(L.(n w k))/*
*/ iv(L(0/2).ys yr*) nolevel robust small

. xtabond2 n L.n L(0/1).(w k) yr*, gmmstyle(L.(n w k))/*
*/ ivstyle(yr*, equation(level)) robust small
