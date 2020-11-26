# Generalized linear models
# R code
# Logistic regression
# Risk of developing coronary heart disease (CD) by age (<60 and >60 years old)
coronary1 <- data.frame(CD = rep(1, 28), age = 'old') 
coronary2 <- data.frame(CD = rep(0, 11), age = 'old') 
coronary3 <- data.frame(CD = rep(1, 23), age = 'young') 
coronary4 <- data.frame(CD = rep(0, 72), age = 'young') 
coronary <- rbind(coronary1, coronary2, coronary3, coronary4) 
coronary <- rbind(coronary3, coronary4, coronary1, coronary2) 
fit <- glm(CD ~ age, data = coronary, family = binomial())
summary(fit)

# Interpretation of the coefficients in terms of the odds ratio – An Example
pre1 <- data.frame(prey = c(10:12), predator = rep(0, 3)) 
pre2 <- data.frame(prey = rep(c(10:12), c(2, 4, 8)), predator = rep(1, 14)) 
pre <- rbind(pre1, pre2) 
fit <- glm(predator ~ prey, data = pre, family = binomial())
summary(fit)

# R code for logistic regression - glm
#Logistic regression curve 
logit = function(p) log(p/(1 - p)) 
p = seq(0.01, 0.99, by = 0.01) 
plot(logit(p),p, type = "l", main = "Logistic curve") 
abline(h = 0, lty = 2)
abline(v = 0, lty = 2) 
#Logistic regression example 
head(trees)
use=sample(0:1,length(trees$Girth), rep=TRUE) 
data1=cbind(trees, use) 
names(data1)
table(use)
summary(data1$Girth)
sd(data1$Girth) 
fit = glm(use~Girth+Height+Volume,data=data1,family=binomial()) 
summary(fit) # display results 
confint(fit) # 95% CI for the coefficients 
exp(coef(fit)) # exponentiated coefficients 
exp(confint(fit)) # 95% CI for exponentiated coefficients 
pred = predict(fit, type="response") # predicted values 
res = residuals(fit, type="deviance") # residuals 
x11()
plot(pred, res) 
plot(data1$Girth, pred)
op = par(mfrow = c(2, 2), pty = "s")
plot(fit)


# Maximum likelihood function for a normal distribution n(0,2)
# Courtesy of Yihui Xie 
mu=seq(-2, 2, len=100) 
sigma=seq(1,10, len=100) 
x=rnorm(1000, mean=0, sd=2) 
loglik=matrix(apply(expand.grid(mu, sigma), 1, function(z) sum(dnorm(x, mean=z[1], sd=z[2],log=TRUE))), nrow=length(mu), ncol=length(sigma))


# R code: maximum likelihood estimation (MLE )
# Negative log likelihood 
negLL = function(psi) (-1)*dbinom(z, size=n, prob=psi, log=TRUE)
n = 5 # 5 independent coin tosses 
z = 1 # 1 heads
# General-purpose optimization based on Nelder–Mead, quasi-Newton and 
# conjugate-gradient algorithms. It includes an option for box-constrained
# optimization and simulated annealing 
# Method "BFGS" is a quasi-Newton method (also known as a variable metric algorithm), specifically that published simultaneously in 1970 by 
# Broyden, Fletcher, Goldfarb and Shanno. This uses function values and gradients to build up a picture of the surface to be optimized.
fit = optim(0.5, negLL, method='BFGS')
list(logLikelihood = fit$value, mle = fit$par)


# Log likelihood
ibis <- "ID Rice Nest
1 2.5 1
2 1.3 1
3 3.6 1
4 4.2 1
5 0.8 0
6 2.1 0
7 0.9 0
8 1.5 0
9 1.1 0
10 0.3 0"
nests <- read.table(con <- textConnection(ibis), header=TRUE)
close(con)

fit.0 = glm(Nest~1,data=nests,family = binomial) # null model
fit.1 = glm(Nest~Rice,data=nests,family = binomial) # null model
summary(fit.1)
y.hat.0 = predict(fit.0,nests,type = "response")# null model
y.hat.1 = predict(fit.1,nests,type = "response")# full model

library(lmtest)
library(zoo)
require(lmtest)
lrtest(fit.1)
lrtest(fit.1, fit.0)
logLik(fit.0) # -6.7301
log(0.4^4 * 0.6^6) # -6.7301
log(prod(y.hat.1[1:4]) * prod(1 - y.hat.1[6:10])) # -3.3926


# Goodness of fit - Analogous R2
# R code
library(Hmisc)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(SparseM)
library(rms) # required for lrm()
fit2 <- lrm(y ~ x1 + x2, data = data1)
fit2[[3]][10] # R square


# Hosmer-Lemeshow goodness of fit
library(ResourceSelection)
model <- glm(y ~ x, family=binomial)
hoslem.test(model$y, fitted(model))


# Goodness of fit - Wald Test
library(survival)
library(aod)
wald.test(b = coef(model), Sigma = vcov(model),Terms = 2:3)
step(fit) # Stepwise logistic regression based on AIC

fit <- glm(y ~ x1 + x2, data = data2, family = gaussian) # General Linear Model
fit <- glm(y ~ x1 + x2, data = data2, family = binomial()) # Logistic Regression

library(MASS)
fit <- glm.nb(y ~ x1 + x2, data = data2)# Negative Binomial Distribution

fit <- glm(y ~ x1 + x2, data = data1, family = poisson()) # Poisson Regression


# Habitat use of shortnose sturgeon - gill net capture
# Kolmogorov-Smirnov test for Poisson distribution
ks.test(x, rpois(length(x), mean(x)))


# GLM for habitat use of shortnose sturgeon
Fish count data (gill net capture) 
glm (count.gillnet ~ depth + temperature + salinity + substrate + velocity, data = data.count.gillnet , family = poisson())

Fish count data (underwater video survey) 
glm.nb (count.video ~ depth + substrate, data = data.count.video)

Fish tracking data (sonar telemetry) 
glm (present.tracking ~ depth + temperature + salinity + substrate + velocity, data = data.tracking, family = binomial())


# Quantify spatial autocorrelation
# spatial aotocorrelation 

library(nlme)
f1 <- formula(seedling ~ magpie + bulbul + site)
B1.gls <- gls(f1, data = yew) 
Vario.gls <- Variogram(B1.gls, form =~ x + y, robust = TRUE, maxDist = 200, resType = "pearson")
plot(Vario.gls, smooth = T)
yew$x = yew$x + rnorm(nrow(yew),0,0.01) # avoid zero distance 
yew$y = yew$y + rnorm(nrow(yew),0,0.01) 
B1A <- gls(f1, correlation = corSpher(form =~ x + y, nugget = TRUE), data = yew) 
B1B <- gls(f1, correlation = corLin(form =~ x + y, nugget = TRUE), data = yew)
B1C <- gls(f1, correlation = corRatio (form =~ x + y, nugget = TRUE), data = yew) 
B1D <- gls(f1, correlation = corGaus (form =~ x + y, nugget = TRUE), data = yew) 
B1E <- gls(f1, correlation = corExp (form =~ x + y, nugget = TRUE), data = yew) 
AIC(B1A, B1B, B1C, B1D, B1E)
summary(B1D) # best model


# Generalized linear mixed model (GLMM)
library(Matrix)
library(lme4) 
model <- glmer(seedling ~ magpie + bulbul + (1 | year) + (1 | site) + (1 | site:year), data = yew, family = poisson)
summary(model)
anova(model)


# R code
# Zuur et al. 2009. Mixed Effect Models page 268 
Snakes = read.table('D:/softwares/R/library/AED/data/Snakes.txt', header=T) 
library(MASS) 
M2A <- glm.nb(N_days ~ PDayRain + Tot_Rain + Road_Loc + PDayRain:Tot_Rain, data = Snakes)
library(stats4)
library(splines)
library(VGAM) 
M2B <- vglm(N_days ~ PDayRain + Tot_Rain + Road_Loc + PDayRain:Tot_Rain, family = negbinomial, data = Snakes)
# Zero-truncated GLM 
M3A <- vglm(N_days ~ PDayRain + Tot_Rain + Road_Loc + PDayRain:Tot_Rain, family = posnegbinomial,control = vglm.control(maxit = 100), data = Snakes)


# Compare zero-truncated and untruncated GLMs
# Zuur et al. 2009. Mixed Effect Models page 269 
Z <- cbind(coef(M2A), coef(M3A)[-2]) 
ZSE <- cbind(sqrt(diag(vcov(M2A))), sqrt(diag(vcov(M3A))[-1])) 
Comp <- cbind(Z[,1], Z[,2], ZSE[,1], ZSE[,2]) 
Comb <- round(Comp, digits = 3) 
colnames(Comb) <- c("NB", "Trunc.NB", "SE NB", "SE Trunc.NB")
Comb


# ZIP model
# R code
# Zuur et al. 2009. Mixed Effect Models page 270 
ParasiteCod = read.table('D:/softwares/R/library/AED/data/ParasiteCod.txt', header=T)
ParasiteCod$fArea <- factor(ParasiteCod$Area) 
ParasiteCod$fYear <- factor(ParasiteCod$Year) 
I1 <- is.na(ParasiteCod$Intensity) | is.na(ParasiteCod$fArea) | is.na(ParasiteCod$fYear) | is.na(ParasiteCod$Length)
ParasiteCod2 <- ParasiteCod[!I1, ] 
plot(table(ParasiteCod2$Intensity), ylab = "Frequencies", xlab = "Observed intensity values") #Figure to the right

library(pscl) 
f1 <- formula(Intensity ~ fArea*fYear + Length | fArea * fYear + Length) 
Zip1 <- zeroinfl(f1, dist = "poisson", link = "logit", data = ParasiteCod2)
summary(Zip1)


# R code and results
library(survival)
fit = clogit(Attempt ~ F_tot * F_ava * Rank + strata(Stratum), data=D) 
summary(fit)


# Multinomial logistic regression
head(mtcars) 
mtcars$cyl = as.factor(mtcars$cyl) 
mtcars$gear = as.factor(mtcars$gear)
# Re-leveling data
mtcars$cyl <- relevel(mtcars$cyl, ref = "4") 
table(mtcars$cyl)
table(mtcars$gear)

options(digits=4) 
library("nnet") 
test <- multinom(gear ~ wt + cyl, data = mtcars) 
summary(test) # Coefficients and SE

# significance 
z <- summary(test)$coefficients / summary(test)$standard.errors 
z
p <- (1 - pnorm(abs(z), 0, 1))*2; p
exp(coef(test))


# prediction 
head(fitted(test)) # fitted values 
expanded = expand.grid(cyl = c("4", "6", "8"), wt = c(1.5, 2, 2.5, 3, 3.5, 4, 4.5))
predicted = predict(test,expanded, type = "probs") 
bpp = cbind(expanded, predicted)
# “melts” data with the purpose of each row # being a unique id-variable combination 
library("reshape2") 
bpp2 = melt(bpp, id.vars = c("cyl", "wt"), value.name = "probablity") 
head(bpp2)
library("ggplot2") 
ggplot(bpp2, aes(x = wt, y = probablity, colour = cyl)) + geom_line() + facet_grid(variable ~ ., scales="free")


# Ordered logistic regression
require(foreign)
require(ggplot2)
require(MASS) 
require(Hmisc)
require(reshape2)
mtcars$gear = ordered(mtcars$gear, levels = c(3, 4, 5))
mtcars$gear
str(mtcars)
m <- polr(gear ~ wt + cyl, data = mtcars, Hess=TRUE) 
summary(m)

ctable <- coef(summary(m)) 
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
p 
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(m)
ci # confidence intervals 
exp(coef(m)) 
exp(cbind(OR = coef(m), ci)) ## OR and CI 
# enhance this model to obtain better prediction estimates 
summary(update(m, method = "probit", Hess = TRUE), digits = 3) 
summary(update(m, method = "logistic", Hess = TRUE), digits = 3)
summary(update(m, method = "cloglog", Hess = TRUE), digits = 3)

# mixed effect model 
library(VGAM)
library(lme4)
library(nlme)
library(ordinal) 
fmm1 <- clmm(cyl ~ wt + (1|gear), data = mtcars)
summary(fmm1) 
fmm2 <- clmm(cyl ~ wt + (1|gear), data = mtcars, link = "probit", threshold = "equidistant")
summary(fmm2)


# Complete or quasi-complete separation in logistic regression
Y <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1) 
X1 <- c(1, 1, 2, 3, 3, 5, 6, 7, 9, 9) 
X2 <- c(1, 2, 1, 1, 5, 4, 1, 0, 3, 6)
fit <- glm(Y ~ X1 + X2, family = binomial)


# biomod2
library(biomod2) 
# species occurrences 
DataSpecies <- read.csv(system.file ("external/species/mammals_table.csv", package="biomod2"))
# the name of studied species 
myRespName <- 'GuloGulo'
# the presence/absences data for our species 
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data 
myRespXY <- DataSpecies[,c("X_WGS84","Y_WGS84")]
# Environmental variables extracted from BIOCLIM (bio_3, bio_4, bio_7, bio_11 & bio_12) 
myExpl = stack( system.file( "external/bioclim/current/bio3.grd", package="biomod2"),system.file( "external/bioclim/current/bio4.grd",package="biomod2"),system.file( "external/bioclim/current/bio7.grd", package="biomod2"),system.file( "external/bioclim/current/bio11.grd", package="biomod2"),system.file( "external/bioclim/current/bio12.grd", package="biomod2"))
# 1. Formatting Data
myBiomodData <- BIOMOD_FormatingData(resp.var = myResp, expl.var = myExpl, resp.xy = myRespXY, resp.name = myRespName)
# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()
# 3. Doing Modelisation 
myBiomodModelOut <- BIOMOD_Modeling( myBiomodData, models = c('SRE','RF'), models.options = myBiomodOption, NbRunEval=2, DataSplit=80, VarImport=0, models.eval.meth = c('TSS','ROC'), do.full.models=TRUE, modeling.id="test")
## print a summary of modeling stuff
myBiomodModelOut


# Model performance: Cohen’s kappa
library(VGAM)
library(Hmisc)
library(ggplot2)
library(psych) # for for Cohen's Kappa 
# "obs" has observed 0/1 values, "pred" has predicted 0/1 values
Kappa = cohen.kappa(data[, c("obs", "pred")])$kappa


# ROC curve
library(stats)
library(pROC) # for ROC 
# use has the 0/1 values, prb has the predicted p values 
roc1 = roc(use, prb , percent = T, auc = T, plot = T) 
roc1$auc # the AUC value
# specificity, sensitivity, and threshold 
SST = coords(roc1, 'best',ret = c('spec', 'sens', 'threshold'))

library(pROC) # for ROC 
par(mfrow=c(2,2))
# perfect prediction 
prb = runif(1000) 
use = round(prb) 
roc1 = roc(use, prb , percent = T, auc = T, plot = T)
# half perfect, half random 
use1 = round(prb)[1:500] 
use2 = sample(c(0,1), 500, rep=T) 
use = c(use1, use2) 
roc1 = roc(use, prb , percent = T, auc = T, plot = T)
# random prediction 
use = sample(c(0,1), 1000, rep=T) 
roc1 = roc(use, prb , percent = T, auc = T, plot = T)
# high presence 
use = sample(c(0,1), 1000, rep=T, prob=c(0.05, 0.95))
roc1 = roc(use, prb , percent = T, auc = T, plot = T)


# Prediction
# predicted probability of presence 
# random forest 
p <- predict(model, new.data, type = 'response', predict.all = F)
# GAM 
p <- predict.gam(model, type="link", newdata = new.data)
# some random data 
ID = 1:200 
use = sample(c(0,1), 200, replace=T, prob=c(0.75,0.25)) 
p = use / 4 + rnorm(200, mean=0.3, sd = 0.2) # p = use / 1.5 + rnorm(200, mean=0.3, sd = 0.1)
occ = data.frame(ID, use, p)


# Distribution of the probability of presence
plot(density(p, from=0, to =1), xlab="P", main="")


# Model evaluation parameters
modEva <- function(data) { 
  library(pROC) # for ROC 
  library(psych) # for Cohen's Kappa 
  use = data[,"use"]
  prb = data[,"p"] 
  roc1 = roc(use, prb , percent = T, auc = T, plot = T) # for AUC 
  SST = coords(roc1, 'best', ret = c('spec', 'sens', 'threshold')) # for specificity, sensitivity, threshold 
  ACC = (SST[1]*sum(use) + SST[2]*(nrow(data) - sum(use))) / nrow(data) # for accuracy 
  names(ACC) = "accuracy(%)" 
  AUC = roc1$auc # AUC 
  names(AUC) = "AUC(%)" 
  Pred <- ifelse(prb >= SST[3], 1, 0) # classification based on best threshold 
  data = cbind(data, Pred) 
  kappa = cohen.kappa(data[, c("use", "Pred")])$kappa 
  names(kappa)="kappa" 
  L = c(AUC, ACC, kappa , SST)
  L = round(L, 3) 
  return(L)
}
modEva(occ)
TSS = sensitivity + specificity-1 # true skill statistic


# R code for logistic regression Biostatistics Xinhai Li– an example for species habitat prediction in future climate conditions
#Generate a dataset of species occurrences and control sites 
x <- seq(116, 120, by = 0.1) #longitude 
y <- seq(36,40, by = 0.1) #latitude
Geo <- expand.grid(x, y) #switch a grid to a two-column table 
names(Geo) = c('Lon', 'Lat') 
use <- as.factor(sample(0:1, length(Geo$Lat), rep = TRUE) )#present/absent 
elev <- rnorm(length(Geo$Lat), 1000, 200) #elevation 
aspect <- sample(1:100, length(Geo$Lat), rep = TRUE)/100 #slope aspect of nest tree 
temperature <- rnorm(length(Geo$Lat), 20, 5)*1000/elev #temperature negatively associated with elevation 
distr <- cbind(Geo, use, elev, aspect, temperature) #the database 
head(distr) #show the structure of database 
fit <- glm(use ~ elev + aspect + temperature, data = distr, family = binomial())
summary(fit) 
# fit <- step(glm(use ~ elev + aspect + temperature, data = distr, family = binomial()))
summary(fit) 
p.current <- predict(fit, newdata = distr, type='response') # predicted p values 
hist(p.current)
coplot(p.current ~ elev | Lon*Lat, data = distr, overlap = 0, number = c(8, 8)) 
result <- cbind(distr, p.current)
head(result) 
#plot current distribution 
attach(distr)
x11()
plot(116:120, 36:40, type = "n", xlab = 'Longitude', ylab = 'Latitude') 
for (i in 1:length(distr$Lat)){ 
  if(p.current[i]>0.5) points(Lon[i], Lat[i], col = "red", cex = 1.5, pch = 19)
  if(p.current[i]<0.5) points(Lon[i], Lat[i], col = "blue", cex = 1.5, pch = 19)}

#predict future distribution 
distr.future <- distr 
distr.future$temperature <- distr.future$temperature + 2 # a global warming scenario 
p.future <- predict(fit , type = c("response"), newdata = distr.future)

#plot future distribution 
x11()
plot(116:120, 36:40, type = "n", xlab = 'Longitude', ylab = 'Latitude') 
for (i in 1:length(distr.future$Lat)){ 
  if(p.future[i]>0.5) points(Lon[i], Lat[i], col = "red", cex = 1.5, pch = 19)
  if(p.future[i]<0.5) points(Lon[i], Lat[i], col = "blue", cex = 1.5, pch = 19)}

coplot(pred.current ~ elev | Lon*Lat, data=distr, overlap = 0, number = c(8,8))


