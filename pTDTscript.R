### PTDT script
### Author: Varun Warrier


### Paper: https://www.nature.com/articles/ng.3863#methods

###Formulas
###PGSmidparent = (PGSfather + PGSmother)/2 ; 
###pTDT deviation is (PGSchild - PGSmidparent)/SD(PGSmidparent);
###tPDT = mean (pTDT deviation)/ (SD(pTDT deviation)/sqrt(N))


## STEP 1: Read files and merge

library(data.table)

#First, read the fam files
Mother = fread("~/SFARI/mothersdata.txt", header= T)
Father = read.table("~/SFARI/fathersdata.txt", header= T)
Cases = fread("~/SFARI/cases.txt", header= T)
Siblings = fread("~/SFARI/siblings.txt", header= T)

#Next, read the prs scores
PRS = fread("~/ALSPAC/PRSice2results/Sfarimergedfriendshipmtagprsice.all.score", header = TRUE)

#Create merged files
fatherpgs = merge(Father, PRS, by = "IID")
motherpgs = merge(Mother, PRS, by = "IID")
casespgs = merge(Cases, PRS, by = "IID")

##Step 2: Calculate mid-parent PGS and SD of midparent PGS

parentpgs = merge(motherpgs, fatherpgs, by = "FID.x")
parentpgs$midparent8 = (parentpgs$`1.000000.x` + parentpgs$`1.000000.y`)/2
parentpgs$midparent7 = (parentpgs$`0.750000.x` + parentpgs$`0.750000.y`)/2
parentpgs$midparent6 = (parentpgs$`0.500000.x` + parentpgs$`0.500000.y`)/2
parentpgs$midparent5 = (parentpgs$`0.250000.x` + parentpgs$`0.250000.y`)/2
parentpgs$midparent4 = (parentpgs$`0.100000.x` + parentpgs$`0.100000.y`)/2
parentpgs$midparent3 = (parentpgs$`0.010000.x` + parentpgs$`0.010000.y`)/2
parentpgs$midparent2 = (parentpgs$`0.001000.x` + parentpgs$`0.001000.y`)/2
parentpgs$midparent1 = (parentpgs$`0.000100.x` + parentpgs$`0.000100.y`)/2

triopgs = merge(parentpgs, casespgs, by = "FID.x")

Sd1 = sd(triopgs$midparent1)
Sd2 = sd(triopgs$midparent2)
Sd3 = sd(triopgs$midparent3)
Sd4 = sd(triopgs$midparent4)
Sd5 = sd(triopgs$midparent5)
Sd6 = sd(triopgs$midparent6)
Sd7 = sd(triopgs$midparent7)
Sd8 = sd(triopgs$midparent8)

## Step 3: Calculate pTDT deviation
triopgs$diff8 = (triopgs$`1.000000` - triopgs$midparent8)/Sd8
triopgs$diff7 = (triopgs$`0.750000` - triopgs$midparent7)/Sd7
triopgs$diff6 = (triopgs$`0.500000` - triopgs$midparent6)/Sd6
triopgs$diff5 = (triopgs$`0.250000` - triopgs$midparent5)/Sd5
triopgs$diff4 = (triopgs$`0.100000` - triopgs$midparent4)/Sd4
triopgs$diff3 = (triopgs$`0.010000` - triopgs$midparent3)/Sd3
triopgs$diff2 = (triopgs$`0.001000` - triopgs$midparent2)/Sd2
triopgs$diff1 = (triopgs$`0.000100` - triopgs$midparent1)/Sd1


## Step 4: Calculate the T score for pTDT deviation
N = sqrt(nrow(triopgs))

One = mean(triopgs$diff1)/(sd(triopgs$diff1)/N)
Two = mean(triopgs$diff2)/(sd(triopgs$diff2)/N)
Three = mean(triopgs$diff3)/(sd(triopgs$diff3)/N)
Four = mean(triopgs$diff4)/(sd(triopgs$diff4)/N)
Five = mean(triopgs$diff5)/(sd(triopgs$diff5)/N)
Six = mean(triopgs$diff6)/(sd(triopgs$diff6)/N)
Seven = mean(triopgs$diff7)/(sd(triopgs$diff7)/N)
Eight = mean(triopgs$diff8)/(sd(triopgs$diff8)/N)

One
Two
Three
Four
Five
Six
Seven
Eight
