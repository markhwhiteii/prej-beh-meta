#### PREP ####
library(metafor)
library(magrittr)
library(polycor)

# writing function to turn variance of r_bis, r_tet, r_poly to variance of fisher's z, using delta method
zvar <- function(yi, vi) {
  vi/(1 - yi^2)^2
}

#### BRANNON ET AL., 1973 ####
# effect 1011
y1011 <- escalc(measure="RTET", ai=66, bi=19, ci=23, di=53, n1i=85, n2i=76)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v1011 <- escalc(measure="RTET", ai=66, bi=19, ci=23, di=53, n1i=85, n2i=76)[2] %>% # get r_tet
  zvar(y1011, .) # transform variance

# effect 1012
y1012 <- escalc(measure="RTET", ai=77, bi=33, ci=23, di=20, n1i=110, n2i=43)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v1012 <- escalc(measure="RTET", ai=77, bi=33, ci=23, di=20, n1i=110, n2i=43)[2] %>% # get r_tet
  zvar(y1012, .) # transform variance

# effect 1013
y1013 <- escalc(measure="RTET", ai=42, bi=10, ci=25, di=24, n1i=52, n2i=49)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v1013 <- escalc(measure="RTET", ai=42, bi=10, ci=25, di=24, n1i=52, n2i=49)[2] %>% # get r_tet
  zvar(y1013, .) # transform variance

#### BRIEF ET AL., 2000 ####
# means and sds, collapsed, from Table 1
mhi <- ((1.87*23)+(0.53*19))/(23+19)
sdhi <- ((.55*23)+(.61*19))/(23+19)
mlow <- (1.81+1.10)/2
sdlow <- (.40+.77)/2

# effect 1311, need to flip the sign
y1311 <- escalc(measure="RBIS", m1i=mhi, sd1i=sdhi, m2i=mlow, sd2i=sdlow, n1i=42, n2i=42)[1] %>% # get r_bis
  transf.rtoz() %>% # transform to z
  prod(-1) # flip sign
v1311 <- escalc(measure="RBIS", m1i=mhi, sd1i=sdhi, m2i=mlow, sd2i=sdlow, n1i=42, n2i=42)[2] %>% # get r_bis
  zvar(y1311, .) # transform variance

#### DEFLEUR & WESTIE, 1958 ####
# effect 1911
y1911 <- escalc(measure="RTET", ai=18, bi=9, ci=5, di=14, n1i=27, n2i=19)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v1911 <- escalc(measure="RTET", ai=18, bi=9, ci=5, di=14, n1i=27, n2i=19)[2] %>% # get r_tet
  zvar(y1911, .) # transform variance

#### FENDRICH, 1976A ####
# effect 2711
y2711 <- escalc(measure="RTET", ai=28, bi=68, ci=6, di=87, n1i=96, n2i=93)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v2711 <- escalc(measure="RTET", ai=28, bi=68, ci=6, di=87, n1i=96, n2i=93)[2] %>% # get r_tet
  zvar(y2711, .) # transform variance

#### FENDRICH, 1976B ####
# recreating entire dataset
p <- c(1:46)
att <- c(131,127,138,118,106,113,113,118,149,131,
         118,143,137,123,117,146,137,127,124,149,
         124,132,118,118,108,141,127,131,110,144,
         144,132,159,126,116,139,124,136,129,133,
         128,136,113,123,111,123)
commit <- c(5,7,2,6,3,6,4,4,8,10,
            6,8,7,6,10,9,7,8,5,10,
            6,3,4,4,0,8,5,10,4,8,
            10,6,10,5,3,6,6,6,8,6,
            10,10,7,5,7,7)
overt <- c(3,2,1,1,0,1,0,0,0,3,
           1,1,0,0,0,1,0,1,0,0,
           0,0,0,0,0,2,1,1,1,3,
           3,1,2,0,0,1,0,2,0,1,
           1,2,1,0,1,1)
fendrichb <- as.data.frame(cbind(p,att,commit,overt))

# effects 2811 and 2812
round(cor(fendrichb[,-1]),2)

#### GREEN, 1972 ####
# effect 3511
y3511 <- escalc(measure="RBIS", m1i=sqrt(8.1088)*sqrt(2)/sqrt(22), m2i=0, sd1i=1, sd2i=1, n1i=22, n2i=22)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v3511 <- escalc(measure="RBIS", m1i=sqrt(8.1088)*sqrt(2)/sqrt(22), m2i=0, sd1i=1, sd2i=1, n1i=22, n2i=22)[2] %>%
  zvar(y3511, .) # transform variance

#### HIMELSTEIN & MOORE, 1963 ####
# effect 3911
y3911 <- escalc(measure="RTET", ai=16, bi=10, ci=17, di=8, n1i=26, n2i=23)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v3911 <- escalc(measure="RTET", ai=16, bi=10, ci=17, di=8, n1i=26, n2i=23)[2] %>% # get r_tet
  zvar(y3911, .) # transform variance

#### HOWITT & MCCABE, 1978 ####
# effect 4311
y4311 <- escalc(measure="RBIS", m1i=4.2*sqrt(2)/sqrt(14), m2i=0, sd1i=1, sd2i=1, n1i=14, n2i=18)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v4311 <- escalc(measure="RBIS", m1i=4.2*sqrt(2)/sqrt(14), m2i=0, sd1i=1, sd2i=1, n1i=14, n2i=18)[2] %>%
  zvar(y4311, .) # transform variance

#### KATZ, COHEN, & GLASS, 1975 ####
# effect 5011
y5011 <- escalc(measure="RBIS", m1i=sqrt(5.07)*sqrt(2)/sqrt(77), m2i=0, sd1i=1, sd2i=1, n1i=77, n2i=77)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v5011 <- escalc(measure="RBIS", m1i=sqrt(5.07)*sqrt(2)/sqrt(77), m2i=0, sd1i=1, sd2i=1, n1i=77, n2i=77)[2] %>%
  zvar(y5011, .) # transform variance

#### LINN, 1965 ####
# Creating dataset from the table
# 3x3 table (chi-square with df=4) complicated to turn into r...
# ...and formulas assume the categories are NOT ordered...
# ...but they are in this case, so I'll recreate 10,000 datasets...
# ...then calculate an r for each, and see the average r
# Table 2
nreps <- 10000
linn2rs <- matrix(data=NA,nrow=nreps,ncol=1)
rownames(linn2rs) <- 1:nreps
colnames(linn2rs) <- "r"

set.seed(1839) # setting seed as alma mater's founding date (Mizzou), as always
for (i in 1:nreps) {
  linn2 <- matrix(data=NA, nrow=34, ncol=2)
  rownames(linn2) <- 1:34 # naming rows
  colnames(linn2) <- c("va","ob") # naming columns for verbal attitude, overt behavior
  # Row 1 of table 2
  linn2[1:7,"va"] <- sample(6:7, 7, replace=TRUE) # cell 1,1 of the table
  linn2[1:7,"ob"] <- sample(6:7, 7, replace=TRUE) # cell 1,1 of the table
  linn2[8:10,"va"] <- sample(6:7, 3, replace=TRUE) # cell 1,2 of the table
  linn2[8:10,"ob"] <- sample(3:5, 3, replace=TRUE) # cell 1,2 of the table
  linn2[11:17,"va"] <- sample(6:7, 7, replace=TRUE) # cell 1,3 of the table
  linn2[11:17,"ob"] <- sample(0:2, 7, replace=TRUE) # cell 1,3 of the table
  # Row 2 of table 2
  linn2[18,"va"] <- sample(3:5, 1, replace=TRUE) # cell 2,1 of the table
  linn2[18,"ob"] <- sample(6:7, 1, replace=TRUE) # cell 2,1 of the table
  linn2[19:22,"va"] <- sample(3:5, 4, replace=TRUE) # cell 2,2 of the table
  linn2[19:22,"ob"] <- sample(3:5, 4, replace=TRUE) # cell 2,2 of the table
  linn2[23:28,"va"] <- sample(3:5, 6, replace=TRUE) # cell 2,3 of the table
  linn2[23:28,"ob"] <- sample(0:2, 6, replace=TRUE) # cell 2,3 of the table
  # Row 3 of table 2
  # cell 3,1 is empty
  linn2[29,"va"] <- sample(0:2, 1, replace=TRUE) # cell 3,2 of the table
  linn2[29,"ob"] <- sample(3:5, 1, replace=TRUE) # cell 3,2 of the table
  linn2[30:34,"va"] <- sample(0:2, 5, replace=TRUE) # cell 3,3 of the table
  linn2[30:34,"ob"] <- sample(0:2, 5, replace=TRUE) # cell 3,3 of the table
  linn2 <- as.data.frame(linn2)
  linn2rs[i,1] <- cor.test(linn2$va,linn2$ob)$est
}
linn2rs <- as.data.frame(linn2rs)

## Table 3
nreps <- 10000
linn3rs <- matrix(data=NA,nrow=nreps,ncol=1)
rownames(linn3rs) <- 1:nreps
colnames(linn3rs) <- "r"

set.seed(1839) # setting seed as alma mater's founding date (Mizzou), as always
for (i in 1:nreps) {
  linn3 <- matrix(data=NA, nrow=34, ncol=2)
  rownames(linn3) <- 1:34 # naming rows
  colnames(linn3) <- c("va","ob") # naming columns for verbal attitude, overt behavior
  # Row 1 of table 2
  linn3[1:7,"va"] <- sample(5:7, 7, replace=TRUE) # cell 1,1 of the table
  linn3[1:7,"ob"] <- sample(6:7, 7, replace=TRUE) # cell 1,1 of the table
  linn3[8:13,"va"] <- sample(5:7, 6, replace=TRUE) # cell 1,2 of the table
  linn3[8:13,"ob"] <- sample(3:5, 6, replace=TRUE) # cell 1,2 of the table
  linn3[14:23,"va"] <- sample(5:7, 10, replace=TRUE) # cell 1,3 of the table
  linn3[14:23,"ob"] <- sample(0:2, 10, replace=TRUE) # cell 1,3 of the table
  # Row 2 of table 2
  linn3[24,"va"] <- sample(1:4, 1, replace=TRUE) # cell 2,1 of the table
  linn3[24,"ob"] <- sample(6:7, 1, replace=TRUE) # cell 2,1 of the table
  linn3[25:26,"va"] <- sample(1:4, 2, replace=TRUE) # cell 2,2 of the table
  linn3[25:26,"ob"] <- sample(3:5, 2, replace=TRUE) # cell 2,2 of the table
  linn3[27:34,"va"] <- sample(1:4, 8, replace=TRUE) # cell 2,3 of the table
  linn3[27:34,"ob"] <- sample(0:2, 8, replace=TRUE) # cell 2,3 of the table
  linn3 <- as.data.frame(linn3)
  linn3rs[i,1] <- cor.test(linn3$va,linn3$ob)$est
}
linn3rs <- as.data.frame(linn3rs)

# effects 5511 and 5512, respectively
round(mean(linn2rs$r),2) # behavioral intention
round(mean(linn3rs$r),2) # prejudice

#### MALOF & LOTT, 1962 ####
# effect 5711
y5711 <- escalc(measure="RTET", ai=12, bi=3, ci=7, di=8, n1i=15, n2i=15)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v5711 <- escalc(measure="RTET", ai=12, bi=3, ci=7, di=8, n1i=15, n2i=15)[2] %>% # get r_tet
  zvar(y5711, .) # transform variance

#### MONTGOMERY & ENZIE, 1973 ####
# effect 6011
y6011 <- escalc(measure="RBIS", m1i=-0.42*sqrt(2)/sqrt(20), m2i=0, sd1i=1, sd2i=1, n1i=20, n2i=20)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v6011 <- escalc(measure="RBIS", m1i=-0.42*sqrt(2)/sqrt(20), m2i=0, sd1i=1, sd2i=1, n1i=20, n2i=20)[2] %>%
  zvar(y6011, .) # transform variance

#### RADEN, 1973 ####
# getting m and sd for each of the three levels of prejudice
# collapsing across male and females
phim <- ((5.02*6)+(4*2.09))/10
phisd <- ((2.72*6)+(4*0.95))/10
pmedm <- (4.48+2.53)/2
pmedsd <- (1.53+1.43)/2 
plowm <- (4.18+2.61)/2
plowsd <- (1.45+0.57)/2

# to get polyserial correlation, need to get raw data
# will generate data from these ms and sds
# I'll do it 10,000 times, just to be safe
yitemp <- c()
vitemp <- c()
set.seed(1839)
for (i in 1:10000) {
  # generating data
  hi <- scale(rnorm(10)) * phisd + phim
  med <- scale(rnorm(12)) * pmedsd + pmedm
  low <- scale(rnorm(12)) * plowsd + plowm
  
  # making data frame
  x <- c(hi, med, low)
  y <- factor(c(rep("hi",10),rep("med",12),rep("low",12)))
  
  # getting correlation
  yitemp[i] <- polyserial(x, y, ML=TRUE, std.err=TRUE, control=list(reltol=1e12))$rho
  vitemp[i] <- polyserial(x, y, ML=TRUE, std.err=TRUE, control=list(reltol=1e12))$var[1,1]
}

# effect 6911
y6911 <- transf.rtoz(mean(yitemp))*-1 # transform to z, flip sign
v6911 <- zvar(y6911, mean(vitemp)) # transform variance

#### SAENGER & GILBERT, 1950 ####
# recreating dataset, will use prejudice to predict white (0) or black (1) clerk
# then just correlate an r
# not looking at the "street sample"
N <- 61 + 53
sgdat <- matrix(NA,nrow=N,ncol=2)
rownames(sgdat) <- 1:N
colnames(sgdat) <- c("prej","clerkrace")

# starting with column 1 in table 3
nblack <- 61 # number of people who went to black clerk
nwhite <- 53 # number of people who went to white clerk
sgdat[1:nblack,1] <- c(rep(1,round(.38*61,0)),rep(2,round(.23*61,0)),rep(3,round(.18*61,0)),rep(4,round(.18*61,0)),rep(5,round(.03*61,0)))
sgdat[(nblack+1):N,1] <- c(rep(1,round(.28*53,0)),rep(2,round(.34*53,0)),rep(3,round(.15*53,0)),rep(4,round(.17*53,0)),rep(5,round(.06*53,0)))
sgdat[1:nblack,2] <- 1
sgdat[(nblack+1):N,2] <- 0
sgdat <- as.data.frame(sgdat)

# effect 7111, need to flip sign
round(cor.test(sgdat$prej,sgdat$clerkrace)$est,2)*-1

#### SAUCIER & MILLER, 2003 ####
# N = 97 were reached
# let's say about half of these, n = 48, were in the race condition
# which means n = 24 were low in prejudice and n = 24 were high in prejudice
# about 79% of low responded--let's round this to 19 (see figure 1)
# about 61% high responded--let's round this to 15 (see figure 1)
# effect 7211
y7211 <- escalc(measure="RTET", ai=19, bi=5, ci=15, di=9, n1i=24, n2i=24)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v7211 <- escalc(measure="RTET", ai=19, bi=5, ci=15, di=9, n1i=24, n2i=24)[2] %>% # get r_tet
  zvar(y7211, .) # transform variance

#### SCHNAKE, 1998 ####
# df indicates there were 35 ps in the black partner condition
# I will assume that half were in each, 17.5. Unrealistic, but just an estimate
# effect 7311
y7311 <- escalc(measure="RBIS", m1i=sqrt(8.04)*sqrt(2)/sqrt(17.5), m2i=0, sd1i=1, sd2i=1, n1i=17.5, n2i=17.5)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v7311 <- escalc(measure="RBIS", m1i=sqrt(8.04)*sqrt(2)/sqrt(17.5), m2i=0, sd1i=1, sd2i=1, n1i=17.5, n2i=17.5)[2] %>%
  zvar(y7311, .) # transform variance

# effect 7312
y7312 <- escalc(measure="RBIS", m1i=sqrt(4.74)*sqrt(2)/sqrt(17.5), m2i=0, sd1i=1, sd2i=1, n1i=17.5, n2i=17.5)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v7312 <- escalc(measure="RBIS", m1i=sqrt(4.74)*sqrt(2)/sqrt(17.5), m2i=0, sd1i=1, sd2i=1, n1i=17.5, n2i=17.5)[2] %>%
  zvar(y7312, .) # transform variance

# effect 7313
y7313 <- escalc(measure="RBIS", m1i=sqrt(7.28)*sqrt(2)/sqrt(17.5), m2i=0, sd1i=1, sd2i=1, n1i=17.5, n2i=17.5)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v7313 <- escalc(measure="RBIS", m1i=sqrt(7.28)*sqrt(2)/sqrt(17.5), m2i=0, sd1i=1, sd2i=1, n1i=17.5, n2i=17.5)[2] %>%
  zvar(y7313, .) # transform variance

#### SECHRIST, 2000 ####
# effect 7421
y7421 <- escalc(measure="RBIS", m1i=sqrt(46.05)*sqrt(2)/sqrt(27), m2i=0, sd1i=1, sd2i=1, n1i=27, n2i=27)[1] %>% # ger r_bis
  transf.rtoz() # transform to z
v7421 <- escalc(measure="RBIS", m1i=sqrt(46.05)*sqrt(2)/sqrt(27), m2i=0, sd1i=1, sd2i=1, n1i=27, n2i=27)[2] %>%
  zvar(y7421, .) # transform variance

#### SILVERMAN & COCHRANE, 1971 ####
# effects 7711 and 7712
y7711 <- escalc(measure="RTET", ai=18, bi=4, ci=8, di=13, n1i=22, n2i=21)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v7711 <- escalc(measure="RTET", ai=18, bi=4, ci=8, di=13, n1i=22, n2i=21)[2] %>% # get r_tet
  zvar(y7711, .) # transform variance

y7712 <- escalc(measure="RTET", ai=18, bi=12, ci=4, di=9, n1i=30, n2i=13)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v7712 <- escalc(measure="RTET", ai=18, bi=12, ci=4, di=9, n1i=30, n2i=13)[2] %>% # get r_tet
  zvar(y7712, .) # transform variance

#### SUTTON, 2014 ####
# effect 8011, need to reverse sign of effect size
y8011 <- escalc(measure="RBIS", m1i=35.84, sd1i=20.76, n1i=125, m2i=45.13, sd2i=25.70, n2i=125)[1] %>% # get r_bis
  transf.rtoz() %>% # transform to z
  prod(-1) # flip sign
v8011 <- escalc(measure="RBIS", m1i=35.84, sd1i=20.76, n1i=125, m2i=45.13, sd2i=25.70, n2i=125)[2] %>% # get r_bis
  zvar(y8011, .) # transform variance

#### WARNER & DEFLEUR, 1969 ####
# effect 8711
y8711 <- escalc(measure="RTET", ai=38, bi=217, ci=18, di=197, n1i=255, n2i=215)[1] %>% # get r_tet 
  transf.rtoz() # transform to z
v8711 <- escalc(measure="RTET", ai=38, bi=217, ci=18, di=197, n1i=255, n2i=215)[2] %>% # get r_tet
  zvar(y8711, .) # transform variance

# effect 8712, need to reverse sign
y8712 <- escalc(measure="RTET", ai=28, bi=217, ci=39, di=197, n1i=245, n2i=236)[1] %>% # get r_tet 
  transf.rtoz() %>% # transform to z
  prod(-1) # flip sign
v8712 <- escalc(measure="RTET", ai=28, bi=217, ci=39, di=197, n1i=245, n2i=236)[2] %>% # get r_tet
  zvar(y8712, .) # transform variance
