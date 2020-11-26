# Unequal sample sizes, missing data and number of cases
## Tensile strength in paper manufacturing
Y <- c(30,35,37,36,34,41,38,42,29,26,33,36,28,32,40,41,31,36,42,40,31,30,32,40,31,37,41,40,35,40,39,44,32,34,39,45)
block <- gl(3, 12, 36) # Three blocks
A <- gl(3, 4, 36) # Three pulp preparation methods
B <- gl(4,1,36) # Four different temperatures
Dat <- data.frame(Y, block, A, B)

Dat = Dat[-c(1:3), ] # make data unbalanced

summary(aov(Y ~ A*B, data = Dat)) # type I sum of square
Anova(mod <- lm(Y ~ A*B, data = Dat), type = "II") # library(carData) library(car) 
Anova(mod <- lm(Y ~ A*B, data = Dat), type = "III")


# R code – plot data
#ANCOVA
#Human footprint index, elevation, and landcover
ibis <- read.csv('d:/ibis.csv',header=T) #34 nests
ibis$Landcover[ibis$Landcover == 11] <- 1 #Forest
ibis$Landcover[ibis$Landcover == 16] <- 2 #Shrub
ibis$Landcover <- as.factor(ibis$Landcover)

Elev = ibis$Elev
Foot = ibis$Foot
Landcover = ibis$Landcover
plot(Elev, Foot, pch=16+as.numeric(Landcover), col=c('blue', 'red')[as.numeric(Landcover)],cex=1.5)
abline(lm(Foot[Landcover==1]~Elev[Landcover==1]),lty=1, col='blue')
abline(lm(Foot[Landcover==2]~Elev[Landcover==2]),lty=1, col='red')

# Compare means
options(digits=3)
tapply(Foot, Landcover, mean)
t.test(Foot ~ Landcover)

# summary(ancova)
anova1 <- lm(Foot~Landcover)
summary(anova1)
ancova <- lm(Foot~Landcover*Elev)
summary(ancova)

# ANOVA table
anova(ancova)

# Update model
ancova2 = update(ancova, ~. -Landcover:Elev)
anova(ancova, ancova2)

# Compare with ANOVA
ancova3 = update(ancova2, ~. -Elev)
anova(ancova2, ancova3)

# Model selection
step(ancova)

# Model check
ancova.final <- step(ancova)
plot(ancova.final)

ancova = lm(post.score ~ class.type * pre.score * IQ)

# Results
scores$class = factor(scores$class)
ancova = lm(post ~ class * pre * IQ, data = scores)
summary(ancova)

# Model selection
ancova2 = update(ancova, ~. -class : pre : IQ) summary(ancova2)
ancova3 = update(ancova2, ~. -class : pre) summary(ancova3)
ancova4 = update(ancova3, ~. -class : IQ) summary(ancova4)
ancova5 = update(ancova4, ~. -pre : IQ) summary(ancova5)

# Model selection
step(ancova)

# Type III SS
library(car)
sample.data <- data.frame(IV = factor(rep(1:4, each = 20)), DV= rep(c(-3,-3,1,3), each = 20) + rnorm(80))
Anova(lm1 <- lm(DV ~ IV, data = sample.data, contrasts = list(IV = contr.poly)), type = "III")


# Childhood sexual abuse
# Book: Linear models with R (Faraway 2009) 
# Effects of childhood sexual abuse on adult females reported in 
# Rodriguez et al. (1997):45 women treated at a clinic, 
# who reported childhood sexual abuse (csa), were measured for 
# post-traumatic stress disorder (ptsd) and
# childhood physical abuse (cpa)
library(faraway)
data(sexab)
by(sexab, sexab$csa, summary)
plot(ptsd ~ csa, sexab)
plot(ptsd ~ cpa, pch = as.numeric(sexab$csa), col = as.numeric(sexab$csa), sexab)
m1 <- lm (ptsd ~ cpa+csa+cpa:csa, sexab)
summary (m1)
model.matrix (m1)
m2 <- lm (ptsd ~ cpa+csa, sexab)
summary (m2)


# Childhood sexual abuse
plot(ptsd~cpa, pch=as.numeric(sexab$csa), col=as.numeric(sexab$csa), sexab)
abline (3.9753, 0.5506, col = 'red' ) # not abused
abline (10.248, 0.5506) # abused, 10.248 = 3.9753 + 6.2728
plot (fitted (m2), residuals (m2), pch=as.numeric(sexab$csa), xlab= "Fitted", ylab="Residuals")

# change the reference level
sexab$csa <- relevel (sexab$csa, ref="NotAbused") # ref="Abused"
m3 <- lm (ptsd ~ cpa+csa, sexab)
summary (m3)



# Sexual activity and the life span of male fruitflies
# The data for this example come from a study on the sexual activity and the life span of 
# male fruitflies by Partridge and Farquhar (1981):125 fruitflies were divided randomly 
# into five groups of 25 each. The response was the longevity of the fruitfly in days. One 
# group was kept solitary, while another was kept individually with a virgin female each 
# day. Another group was given eight virgin females per day. As an additional control, the 
# fourth and fifth groups were kept with one or eight pregnant females per day. Pregnant # fruitflies will not mate. The thorax length of each male was measured as this was known 
# to affect longevity. The five groups are labeled many, isolated, one, low and high 
# respectively. The purpose of the analysis is to determine the difference between the five
# groups if any.
library(faraway)
data (fruitfly)
plot (longevity ~ thorax, fruitfly, pch=unclass (activity))
legend (0.63, 100, levels (fruitfly$activity), pch=1:5)
g <- lm (longevity ~ thorax*activity, fruitfly)
summary (g)
model.matrix(g)
anova(g)
gb <- lm (longevity ~ thorax+activity, fruitfly)
drop1 (gb, test="F") # drop one term, using F test


# Missing data
library(faraway)
data(chmiss) # Chicago insurance dataset
head(chmiss)
model <- lm(involact ~ ., chmiss)
summary(model)

# Replacing missing data with mean
cmeans <- apply (chmiss, 2, mean, na.rm=T)
cmeans

mchm <- chmiss
for (i in c(1, 2, 3, 4, 6)) mchm[is.na (chmiss[,i]), i] <- cmeans[i]

model <- lm(involact ~ ., mchm)
summary(model)


# The random intercept model
library(nlme)
RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach, data = RIKZ)
summary(Mlme1)

# The random intercept and slope model
library(nlme)
RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme2 <- lme(Richness ~ NAP, random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)


# R code for figures
# The Random Intercept and/or slope Model
RIKZ = read.table('D:/softwares/R/library/AED/data/RIKZ.txt',header=T)
library(nlme)
RIKZ$fBeach <- factor(RIKZ$Beach)
Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach, data = RIKZ)
Mlme1 <- lme(Richness ~ NAP, random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme1)

# plot regression lines
F0 <- fitted(Mlme1, level = 0) # fitted values obtained by the population model
F1 <- fitted(Mlme1, level = 1) # fitted values obtained by within-beach model
I <- order(RIKZ$NAP); NAPs <- sort(RIKZ$NAP)
plot(NAPs, F0[I], lwd = 4, type = "l", ylim = c(0, 22), ylab = "Richness", xlab = "NAP")
for (i in 1:9){ 
  x1 <- RIKZ$NAP[RIKZ$Beach == i] 
  y1 <- F1[RIKZ$Beach == i] 
  K <- order(x1) 
  lines(sort(x1), y1[K])
}
text(RIKZ$NAP, RIKZ$Richness, RIKZ$Beach, cex = 0.9)


# model = lm(Richness ~ NAP * fBeach, data = RIKZ)
model = lm(Richness ~ NAP * fBeach, data = RIKZ)
anova(model)
pred = predict(model, RIKZ[, c('NAP', 'fBeach')])
plot(Dat$NAP, Dat$Richness, xlab='NAP', ylab='Richness', col='white') Dat = cbind(RIKZ, pred)
Dat = cbind(RIKZ, pred)
for (i in 1:9) { 
  Data = Dat[Dat$Beach==i, ] 
  lines(Data$NAP, Data$pred, col=i) 
  points(Data$NAP, Data$Richness, col=i, pch=16)
}

lme(Richness ~ NAP, random = ~1 + NAP | fBeach, data = RIKZ)
lm(Richness ~ NAP * fBeach, data = RIKZ)


# ANCOVA with autocorrelated data
head(ABirds) # data
AP <- c(ABirds$ArrivalAP, ABirds$LayingAP)
SOI2 <- c(ABirds$SOI, ABirds$SOI)
Y2 <- c(ABirds$Year, ABirds$Year)
ID <- factor(rep(c("Arrival", "Laying"), each = 55))
library(nlme)
vf2 <- varIdent(form =~ 1 | ID)
M1 <- gls(AP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit)
M2 <- gls(AP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, correlation = corAR1(form =~Y2 | ID))
anova(M1, M2)


# Model selection
# to compare two models with the same random structure, but with different fixed effect, 
# we need to use the maximum likelihood estimation method instead of REML 
# (P.356 in Zuur et al. 2009) 
M3 <- gls(AP ~ SOI2 + ID + SOI2:ID, weights = vf2, na.action = na.omit, method = "ML", correlation = corAR1(form =~Y2 | ID))
M4 <- gls(AP ~ SOI2 + ID, weights = vf2, na.action = na.omit, method = "ML", correlation = corAR1(form =~Y2 | ID))
M5 <- gls(AP ~ ID, weights = vf2, na.action = na.omit, method = "ML", correlation = corAR1(form =~Y2 | ID))
anova(M3, M4, M5)


# Plot
plot(ABirds$SOI, ABirds$ArrivalAP, ylim = c(195, 260), type = "n", ylab = "Arrival & laying dates", xlab='SOI')
points(ABirds$SOI, ABirds$ArrivalAP, pch = 1) 
points(ABirds$SOI, ABirds$LayingAP, pch = 2) 
MyX <- data.frame(SOI2 = seq(from = min(ABirds$SOI), to = max(ABirds$SOI), length = 20), ID = "Arrival")
Pred1 <- predict(M3, newdata = MyX) 
lines(MyX$SOI2, Pred1) 
MyX <- data.frame(SOI2 = seq(from = min(ABirds$SOI), to = max(ABirds$SOI), length = 20), ID = "Laying")
Pred2 <- predict(M3, newdata = MyX)
lines(MyX$SOI2, Pred2)


# Functions
ancova = lm(Y~X1*X2)
coef(ancova)
fitted(ancova)
predict(ancova)
resid(ancova)






