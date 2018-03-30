#Read the files
library(data.table)


#First, read the fam files
Mother = fread("~/SFARI/mothersdata.txt", header= T)
Father = fread("~/SFARI/mothersdata.txt", header= T)
Cases = fread("~/SFARI/cases.txt", header= T)
Siblings = fread("~/SFARI/siblings.txt", header= T)

#Next, read the prs scores
mv3 = fread("~/ALSPAC/PRSice2results/results_SSC_Mv3/SSC_1Mv3_friendshipmtagprsice.all.score", header = TRUE)
mv1 = fread("~/ALSPAC/PRSice2results/results_SSC_Mv3/SSC_1Mv1_friendshipmtagprsice.all.score", header = TRUE)
Omni = fread("~/ALSPAC/PRSice2results/results_SSC_Omni/SSC_Omni_friendshipmtagprsice.all.score", header = TRUE)

merged = rbind(Omni, mv1)
merged = rbind(merged, mv3)

fatherpgs = merge(Father, merged)
motherpgs = merge(Mother, merged)
parentpgs = merge(motherpgs, fatherpgs, by = "FID")
parentpgs$midparent8 = (parentpgs$`1.000000.x` + parentpgs$`1.000000.y`)/2
parentpgs$midparent7 = (parentpgs$`0.750000.x` + parentpgs$`0.750000.y`)/2
parentpgs$midparent6 = (parentpgs$`0.500000.x` + parentpgs$`0.500000.y`)/2
parentpgs$midparent5 = (parentpgs$`0.250000.x` + parentpgs$`0.250000.y`)/2
parentpgs$midparent4 = (parentpgs$`0.100000.x` + parentpgs$`0.100000.y`)/2
parentpgs$midparent3 = (parentpgs$`0.010000.x` + parentpgs$`0.010000.y`)/2
parentpgs$midparent2 = (parentpgs$`0.001000.x` + parentpgs$`0.001000.y`)/2
parentpgs$midparent1 = (parentpgs$`0.000100.x` + parentpgs$`0.000100.y`)/2

casespgs = merge(Cases, merged)
triopgs = merge(parentpgs, casespgs, by = "FID")


Sd1 = sd(triopgs$midparent1)
Sd2 = sd(triopgs$midparent2)
Sd3 = sd(triopgs$midparent3)
Sd4 = sd(triopgs$midparent4)
Sd5 = sd(triopgs$midparent5)
Sd6 = sd(triopgs$midparent6)
Sd7 = sd(triopgs$midparent7)
Sd8 = sd(triopgs$midparent8)

triopgs$diff8 = (triopgs$`1.000000` - triopgs$midparent8)/Sd8
triopgs$diff7 = (triopgs$`0.750000` - triopgs$midparent7)/Sd7
triopgs$diff6 = (triopgs$`0.500000` - triopgs$midparent6)/Sd6
triopgs$diff5 = (triopgs$`0.250000` - triopgs$midparent5)/Sd5
triopgs$diff4 = (triopgs$`0.100000` - triopgs$midparent4)/Sd4
triopgs$diff3 = (triopgs$`0.010000` - triopgs$midparent3)/Sd3
triopgs$diff2 = (triopgs$`0.001000` - triopgs$midparent2)/Sd2
triopgs$diff1 = (triopgs$`0.000100` - triopgs$midparent1)/Sd1

One = mean(triopgs$diff1)/(sd(triopgs$diff1)/47.2334627145)
Two = mean(triopgs$diff2)/(sd(triopgs$diff2)/47.2334627145)
Three = mean(triopgs$diff3)/(sd(triopgs$diff3)/47.2334627145)
Four = mean(triopgs$diff4)/(sd(triopgs$diff4)/47.2334627145)
Five = mean(triopgs$diff5)/(sd(triopgs$diff5)/47.2334627145)
Six = mean(triopgs$diff6)/(sd(triopgs$diff6)/47.2334627145)
Seven = mean(triopgs$diff7)/(sd(triopgs$diff7)/47.2334627145)
Eight = mean(triopgs$diff8)/(sd(triopgs$diff8)/47.2334627145)

One
Two
Three
Four
Five
Six
Seven
Eight