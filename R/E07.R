# Correlation matrix and variance-covariance matrix
A <- matrix(c(1,2,2,3,2,2,2,3,4,3,4,2,0,2,2,2,0,0),6,3)
SSCP <- t(A) %*% A
cor(A) # correlation matrix
A.dev = A - rep(apply(A, 2, mean), each = length(A[,1])) # deviance
t(A.dev) %*% A.dev / (length(A[,1])-1) # variance-covariance matrix
var(A) # variance-covariance matrix
library(MASS)
ginv(SSCP) # inverse matrix
ginv(ginv(SSCP))
ginv(A) %*% A


# Parameter estimation
Abund = c(41, 22, 31, 9, 39, 11)
Cover = c(80, 48, 40, 24, 64, 8)
Elev = c(4835, 3216, 5012, 2818, 5201, 3678)
fit = lm(Abund ~ Cover + Elev)
summary(fit)


# Checking independence and linearity
library(carData)
library(car)
fit = lm(mpg ~ ., data=mtcars)
durbinWatsonTest(fit) #Durbin-Watson Test for Autocorrelated Errors
crPlots(fit) #Component+Residual (Partial Residual) Plots


# Model and parameter significance
model = lm(log(trees$Volume)~log(trees$Girth)+log(trees$Height))
summary(model)


# R code – VIF (variance inflation factor)
library(carData)
library(car)
vif(lm(mpg ~ ., data = mtcars))


# Power function
# Johannes Kepler's third law of planetary motion
planets = read.table(header = T, row.name = 1, text = " 
planet distance period 
Mercury 57.9 87.98 
Venus 108.2 224.70 
Earth 149.6 365.26 
Mars 228.0 686.98 
Ceres 413.8 1680.50 
Jupiter 778.3 4332.00 
Saturn 1427.0 10761.00 
Uranus 2869.0 30685.00 
Neptune 4498.0 60191.00
Pluto 5900.0 90742.00")
# units: million km, earth day

# standarized by earth
planets$distance = planets$dist / 149.6
planets$period = planets$period / 365.26
plot(planets$distance, planets$period)
abline(lm(planets$period~planets$distance))
par(mfrow=c(1,2))
with(planets, scatter.smooth(log(period) ~ distance, las=1))
title(main="exponential")
with(planets, scatter.smooth(log(period) ~ log(distance), las=1))
title(main="power")
summary(lm(log(period) ~ log(distance), data=planets))


# Exponential function
out = nls(Nest ~ exp(b1*(b0+Year)), data=D, start=list(b0=-1981, b1=1),trace = TRUE)
plot(D$Year, D$Nests)
lines(D$Year, fitted(out), col=2)


# Logistic function
# Logistic growth
time <- c(seq(0,10),seq(0,10),seq(0,10))
plant <- c(rep(1,11),rep(2,11),rep(3,11))
weight <- c( 42,51,59,64,76,93,106,125,149,171,199, 40,49,58,72,84,103,122,138,162,187,209, 41,49,57,71,89,112,146,174,218,250,288)/288
D <- data.frame(cbind(time, plant, weight))

## Plot weight versus time
plot( D$time, D$weight, xlab="Time", ylab="weight", type="n")
text( D$time, D$weight, D$plant)
title(main="Graph of weight vs time")
IN = getInitial( weight ~ SSlogis(time, alpha, xmid, scale), data = D)
## Using the initial parameters above,fit the data with a logistic curve.
para0.st <- c( 
  alpha = IN[1],
  beta = IN[2]/IN[3], # beta is xmid/scale 
  gamma= 1/IN[3] # gamma (or r) is 1/scale
  )
names(para0.st) = c('alpha', 'beta', 'gamma')
fit0 <- nls( 
  weight ~ alpha/(1+exp(beta-gamma*time)), 
  D,
  start = para0.st,
  trace = T
)
curve( 2.21/(1 + exp(2.74 - 0.22*x)), from = time[1], to = time[11], add = TRUE)


# Perspective plots for interaction models
x1 <- seq(0, 10, length= 100)
x2 <- x1
f <- function(x1, x2) { r <- x1+x2+x1*x2}
y <- outer(x1, x2, f)
op <- par(bg = "white", mfrow=c(1,2))
persp(x1, x2, y, theta = 30, phi = 30, expand = 0.5, col = "lightblue", main='y=x1+x2+x1*x2')

# Contour plots for models with linear terms
x1 = x2 <- seq(0, 10, length= 100)
f <- function(x1, x2) { r <- x1+x2 }
y <- outer(x1, x2, f)
filled.contour(x1, x2, y, main="y = x1 + x2", color = terrain.colors)

# Contour plots for high order models
filled.contour(x, y, z, main="z = x + 10y + xy", color = terrain.colors)

# Contour plots for interaction models
x1 = x2 <- seq(0, 10, length= 100)
f <- function(x1, x2) { r <- x1+x2+x1*x2*x2 }
y <- outer(x1, x2, f)
filled.contour(x1, x2, y, main=expression(paste("Y ~ ", X[1], " + ", X[2], " + ", X[1], X[2]^2)), color = terrain.colors) # [] subscript; ^ superscript


# R code - multiple linear regression
ibis = read.csv('D:/database/ibisdata/ibis2010.csv', header=T)
head(ibis)
ibis.pre = ibis[ibis$use==1,c(3:6,8,9,11,12)]
head(ibis.pre)
# Multiple Linear Regression Example (only include linear terms)
fit <- lm(pop ~ latitude+elevation+footprint+year+GDP+slope, data=ibis.pre)
summary(fit) # show results


# R code - Multiple Linear Regression
# Other useful functions
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters

# diagnostic plots
plot(fit$fitted, fit$resid)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs
plot(fit)


# R code - Multiple Linear Regression
# Stepwise Regression
library(MASS)
fit <- lm(pop ~ y+elevation+footprint+year+GDP+slope, data=ibis.pre)
step <- stepAIC(fit, direction="both")
step$anova # display results


# Use the full model as a start
attach(trees)
fit = lm(Volume ~ Girth * Height + I(Girth^2) + I(Height^2), data=trees)
fit = step(fit)
summary(fit)


# Model selection
library(MuMIn)
head(Cement) # Example from Burnham and Anderson (2002), page 100:
Cement = rbind(Cement, (Cement + rnorm(dim(Cement)[1]*dim(Cement)[2], 0, 1))) # enlarge the dataset
Cement = rbind(Cement, (Cement + rnorm(dim(Cement)[1]*dim(Cement)[2], 0, 1))) # enlarge the dataset
# Full model
fit <- lm(y ~ (X1 + X2 + X3 + X4)^2 + I(X1^2) + I(X2^2) + I(X3^2) + I(X4^2), data = Cement, na.action = na.fail)
fit.all <- dredge(fit) # model comparison
fit.all[1:6, ] # the top 6 models

#or as a 95% confidence set:
avgmod.95p <- model.avg(fit.all, cumsum(weight) <= .95)
confint(avgmod.95p)

# The same result, but re-fitting the models via 'get.models'
confset.95p <- get.models(fit.all, cumsum(weight) <= .95)
model.avg(confset.95p)

# Force re-fitting the component models
model.avg(fit.all, cumsum(weight) <= .95, fit = TRUE)
# Models are also fitted if additional arguments are given
model.avg(fit.all, cumsum(weight) <= .95, rank = "AIC")

# using BIC (Schwarz's Bayesian criterion) to rank the models
BIC <- function(x) AIC(x, k = log(length(residuals(x))))
model.avg(confset.95p, rank = BIC)

# Model average
#models with delta.aicc < 4
summary(model.avg(fit.all, subset = delta < 4))


# Path analysis
library(plspm)
D = read.csv('d:/data.csv', header=T)
D 
# path matrix (inner model realtionships) 
Y0 = c(0, 0, 0, 0) 
Y1 = c(1, 0, 0, 0) 
Electro = c(1, 1, 0, 0) 
SR = c(0,1,1,0) 
saker_path = rbind(Y0, Y1, Electro, SR) 
# add optional column names 
colnames(saker_path) = rownames(saker_path) 
# plot the path matrix 
innerplot(saker_path) 
# list indicating what variables are associated with what latent variables 
saker_blocks = list(c(1,2), c(3,4),c(1,2,3,4),c(5,6)) 
# all latent variables are measured in a reflective way 
saker_modes = rep("A", 4) 
# run plspm analysis 
saker_pls = plspm(D, saker_path, saker_blocks, modes = saker_modes) 
# what's in saker_pls? 
saker_pls 
# path coefficients 
saker_pls$path_coefs 
# inner model 
saker_pls$inner_model 
# summarized results 
summary(saker_pls) 
# plot the results (inner model) 
plot(saker_pls) 
# plot the loadings of the outer model 
plot(saker_pls, what = "loadings", arr.width = 0.2) 
# plot the weights of the outer model
plot(saker_pls, what = "weights", arr.width = 0.1)


# 3-D bar-plot
library(lattice)
library(latticeExtra) 
library(faraway)
data(abrasion) 
abrasion$position = as.factor(abrasion$position)
cloud(wear ~ position + material, abrasion, panel.3d.cloud = panel.3dbars, col.facet = colorRampPalette(c('green','yellow','pink','blue'))(4)[abrasion$position], xbase=0.2, ybase=0.2, scales=list(arrows=F, col=1),par.settings = list(axis.line = list(col = "transparent")))


# Fractional-power interaction regression (FPIR)
# The R package interactionFPIR can be installed from GitHub
library(devtools)
library(remotes)
install_github("Xinhai-Li/interaction")
library(interactionFPIR)
attach(trees)
results = FPIR1twoway(trees$Volume, trees$Girth, trees$Height)
results2 = FPIR1twowaytune(trees$Volume, trees$Girth, trees$Height, 1.35, 0.3)
#1.175 0.2

library(interactionFPIR)
FPIR1threeway (y, x1, x2, x3)


# Pairwise correlation
# R code – correlation plot
## put (absolute) correlations on the upper panels, ## with size proportional to the correlations. 
panel.cor <- function(x, y, digits = 2,prefix = "", cex.cor, ...) {
usr <- par("usr")
on.exit(par(usr)) 
par(usr = c(0, 1, 0, 1)) # ranges for x-axis and y-axis in plots 
r <- abs(cor(x, y)) 
txt <- format(c(r, 0.123456789), digits = digits)[1] 
txt <- paste(prefix, txt, sep = "") 
if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(mtcars, lower.panel = panel.smooth,upper.panel = panel.cor)


# Partial correlation example
y <- c(3, 2, 1, 6, 5, 4, 9, 8, 7)
x1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x2 <- c(2, 2, 2, 4, 4, 4, 6, 6, 6)
# multiple regression
fit = summary(lm(y ~ x1 + x2))
interception = fit[[4]][1,1]
coef_x1= fit[[4]][2,1]
coef_x2= fit[[4]][3,1]
x.1 <- seq(min(x1) -1, max(x1) + 1, length= 100)
x.2 <- seq(min(x2) -2, max(x2) + 1, length= 100)
f <- function(x.1, x.2) { r <- interception + coef_x1*x.1 + coef_x2*x.2 } 
y.pred <- outer(x.1, x.2, f)
filled.contour(x.1, x.2, y.pred, main="", color = terrain.colors, 
               xlab=expression(paste(X[1])),
               ylab=expression(paste(X[2])),
               ylim=c(1, 7))
points(x1/1.3, x2, pch=16, cex=y)
library(ggm)
D = cbind(y, x1, x2)
D = jitter(D, factor = .01)
pcor(c("y", "x1"), var(D)) # 0.8
pcor(c("y", "x1","x2"), var(D)) # -0.9999


# R code - partial correlation
# partial correlation 
library(ggm)
## The marginal correlation between analysis and statistics 
pcor(c("footprint", "GDP"), var(ibis.pre)) 
cor(ibis.pre$footprint, ibis.pre$GDP) 
0.528
## The correlation between footprint and GDP given elevation 
pcor(c("footprint", "GDP","elevation"), var(ibis.pre)) 
0.507
## The correlation between footprint and GDP given elevation and latitude 
pcor(c("footprint", "GDP","elevation","latitude"), var(ibis.pre))
0.5


# Remove the effect of X2
plot(x1, y, ylim=c(-2,10), pch=1, cex=3, xlab=expression(paste(X[1])), ylab="Y")
points(x1, resid.y, col=adjustcolor( "red", alpha.f = 0.5), pch=17, cex=2)
points(x1, resid.x1, col=adjustcolor( "blue", alpha.f = 0.5), pch=16, cex=2)
legend("topleft", c("Y", "Residual of lm(Y ~ X2)", "Residual of lm(X1 ~ X2)"),pch=c(1, 17, 16), col=c(1,2,4))


# R code for contribution, fraction, partial R2 and variance partitioning
mtcars
mtcars.st = scale(mtcars)
apply(mtcars.st, 2, var) 
mtcars.st = as.data.frame(mtcars.st)
fit = lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data=mtcars.st)
# Contribution 
Contribution = coef(fit) * cor(mtcars.st)[1,] 
fit2 = step(fit) 
Contribution = coef(fit2)[-1] * cor(mtcars.st)[1, c(6,7,9)]
# Fraction 
f.wt = lm(wt ~ qsec + am, data=mtcars.st) 
res.wt = resid(f.wt) 
Fraction.a = summary(lm(mtcars.st$mpg ~ res.wt))$r.squared
# Partial.R2 
f.mpg = lm(mpg ~ qsec + am, data=mtcars.st) 
res.mpg = resid(f.mpg) 
Partial.R2 = summary(lm(res.mpg ~ res.wt))$r.squared 
# Variance partition 
fit = lm(mpg ~ wt + qsec + am, data=mtcars.st)
anova(fit)[[2]] / sum(anova(fit)[[2]])


# One example of CCA
# http://www.ats.ucla.edu/stat/r/dae/canonical.htm 
require(ggplot2) 
require(GGally) 
require(CCA)
# Example 1. A researcher has collected data on three psychological variables, four academic variables (standardized test scores) 
# and gender for 600 college freshman. She is interested in how the set of psychological variables relates to the academic 
# variables and gender. In particular, the researcher is interested in how many dimensions (canonical variables) are necessary to 
# understand the association between the two sets of variables.
library(plspm)
library(ggplot2)
library(GGally)
library(CCA)
mm <- read.csv("http://www.ats.ucla.edu/stat/data/mmreg.csv") 
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", "Science", "Sex") 
summary(mm)
head(mm)
psych <- mm[, 1:3] 
acad <- mm[, 4:8]
ggpairs(psych) 
ggpairs(acad)
# correlations within and between the two sets of variables
matcor(psych, acad) #CCA package


# One example of CCA
# Canonical Correlation Analysis 
cc1 <- cc(psych, acad)
summary(cc1)

# display the canonical correlations 
cc1$cor

# raw canonical coefficients 
cc1[3:4]

# compute canonical loadings 
cc2 <- comput(psych, acad, cc1)

# display canonical loadings 
cc2[3:6]


# One example of CCA
# tests of canonical dimensions 
ev <- (1 - cc1$cor^2)
n <- dim(psych)[1] 
p <- length(psych) 
q <- length(acad) 
k <- min(p, q) 
m <- n - 3/2 - (p + q)/2 
w <- rev(cumprod(rev(ev)))
# initialize 
d1 <- d2 <- f <- vector("numeric", k)
for (i in 1:k) { 
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s 
  d1[i] <- p * q 
  d2[i] <- m * s - p * q/2 + 1 
  r <- (1 - w[i]^si)/w[i]^si 
  f[i] <- r * d2[i]/d1[i] 
  p <- p - 1 
  q <- q - 1
}
pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))







