library(tidyverse)
rnf = read.csv("redneckedfalcon.csv")
nh = read.csv("rnfnestheight.csv")
rnf = left_join(rnf,nh)
nestperiod = 72

#rnf = rnf %>% filter(year >= 2017)

## reduce exposure to 40% for failed nests 

rnf$fact = 1
rnf$fact[rnf$stat == "F"] = 0.4
rnf$expo = rnf$exp*rnf$fact

## calculate daily survival probability (Mayfield)

totalexp = sum(na.omit(rnf$expo))
fail = length(rnf$stat[rnf$stat == "F"])

dsr = (totalexp - fail)/totalexp

## calculate variance and SE for dsr (Johnson 1979)

var = -(1/(-(totalexp^3)/((totalexp-fail)*fail)))
dsrSE = sqrt(var)
dsrCI = dsrSE*1.96

################ therefore DSR = 0.998539 +- 0.00103 SE or 0.998539 +- 0.002 95%CI

## calculate nesting success (Mayfield) 

ns = dsr^nestperiod
nsSE = ns*sqrt(nestperiod*((dsrSE/dsr)^2))
nsCI = nsSE*1.96

################ therefore NS = 0.900 +- 0.00789 SE or 0.900 +- 0.0155 95%CI (range 0.88 to 0.92)

## calculate productivity (Steenhof and Newton)

broodsize = mean(na.omit(rnf$c2[rnf$stat == "S"]))
broodsizeSE = sd(na.omit(rnf$c2[rnf$stat == "S"]))

prod = ns*broodsize
prodSE = prod*sqrt((nsSE/ns)^2+(broodsizeSE/broodsize)^2)
prodCI = prodSE*1.96

################ therefore PRODUCTIVITY = 2.48 +- 0.72 SE or 2.48 +- 1.4 95%CI (range 1.1 to 3.9)



## comparing nest heights

t1 = rnf$height[rnf$type == "Pylon"]
t2 = rnf$height[rnf$type == "Tree"]

t.test(t1,t2)
