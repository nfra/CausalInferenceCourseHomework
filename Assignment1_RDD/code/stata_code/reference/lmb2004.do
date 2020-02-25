* Lee, Moretti and Butler (2004) QJE replication from mixtape
clear

cd "/Users/scott_cunningham/Dropbox/Workshop/Do"

* scuse lmb-data
use ../mixtape_datafiles/data/lmb-data.dta, replace

reg score lagdemocrat if lagdemvoteshare>.48 & lagdemvoteshare<.52, cluster(id)
reg score democrat if lagdemvoteshare>.48 & lagdemvoteshare<.52, cluster(id)
reg democrat lagdemocrat if lagdemvoteshare>.48 & lagdemvoteshare<.52, cluster(id)

reg score democrat, cluster(id2)

gen demvoteshare_c = demvoteshare - 0.5
reg score democrat demvoteshare_c, cluster(id2)

xi: reg score i.democrat*demvoteshare_c, cluster(id2)
xi: reg score i.democrat*demvoteshare_c if demvoteshare>.45 & demvoteshare<.55, cluster(id2)

gen x_c = demvoteshare - 0.5
gen x_c2 = x_c^2
reg score democrat##(c.x_c c.x_c2)

reg score democrat##(c.x_c c.x_c2) if demvoteshare>0.4 & demvoteshare<0.6

* ssc install cmogram

cmogram score lagdemvoteshare, cut(0.5) scatter line(0.5) lfit
cmogram score lagdemvoteshare, cut(0.5) scatter line(0.5) lfitci
cmogram score lagdemvoteshare, cut(0.5) scatter line(0.5) qfitci
cmogram score lagdemvoteshare, cut(0.5) scatter line(0.5) lowess

* Note kernel-weighted local polynomial regression is a smoothing method.
lpoly score demvoteshare if democrat == 0, nograph kernel(triangle) gen(x0 sdem0) bwidth(0.1)
lpoly score demvoteshare if democrat == 1, nograph kernel(triangle) gen(x1 sdem1) bwidth(0.1)
scatter sdem1 x1, color(red) msize(small) || scatter sdem0 x0, msize(small) color(red) xline(0.5,lstyle(dot)) legend(off) xtitle("Democratic vote share") ytitle("ADA score")

* ssc install rdrobust, replace
rdrobust score demvoteshare, c(0.5)

* McCrary density test

DCdensity demvoteshare_c if (demvoteshare_c>-0.5 & demvoteshare_c<0.5), breakpoint(0) generate(Xj Yj r0 fhat se_fhat)
