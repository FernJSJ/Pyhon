# The famous five sums
plot(trees$Girth, trees$Height)
abline(lm(trees$Height~trees$Girth))
X = trees$Girth
Y = trees$Height

sum(X) # 410.7
sum(X^2) # 5736.55
sum(Y) # 2356
sum(Y^2) # 180274
sum(X*Y) # 31524.7

# matrix multiplication
XY <- cbind(1,X,Y)
t(XY) %*% XY


# Sums of squares and sums of products
SSX = sum((X -mean(X))^2)
SSX # 295.4374
SSY = sum((Y -mean(Y))^2)
SSY # 1218
SSXY = sum((Y -mean(Y))*(X-mean(X)))
SXY # 311.5

# The alternative way using the 5 sums
SSX = sum(X^2)-sum(X)^2/length(X)
SSY = sum(Y^2)-sum(Y)^2/length(Y)
SSXY = sum(X*Y)-sum(X)*sum(Y)/length(X)


# Model (Y=a+bX) coefficients
b = SSXY/SSX
b # 1.054369
a = mean(Y)-b*mean(X)
a # 62.03131
lm(Y~X)


# Analysis of variance in regression
anova(lm(Y~X)) # data: trees
qf(0.95,1,29) # 4.18
1-pf(10.707,1,29) # 0.002757909


# Unreliability estimates for the parameters
summary(lm(Y~X))
confint(lm(Y~X))


# Degree of scatter
SSY = deviance(lm(Y~1))
SSY # 1218
SSE = deviance(lm(Y~X))
SSE # 889.5641
rsq = (SSY-SSE)/SSY
# R square 0.2696518
summary(lm(Y~X))[[8]] # 0.2696518


# R^2 computer output
summary(lm(Volume~Girth, trees))$r.squared # 0.9353199
summary(lm(Volume~Girth, trees))$adj.r.squared# 0.9330895


# Standard errors of regression coefficients
summary(lm(Y~X))[[4]][4] # The standard error of the slope 0.3222233
summary(lm(Y~X))[[4]]


# Prediction using the fitted model
model <- lm(Y~X)
predict(model, list(X = c(14,15,16)))


# Plot estimation CI
ci.lines<-function(model){ 
  xm <- mean(model[[12]][,2]) 
  n <- length(model[[12]][[2]])
  ssx<- sum(model[[12]][2]^2)- sum(model[[12]][2])^2/n 
  s.t<- qt(0.975,(n-2)) 
  xv <- seq(min(model[[12]][2]),max(model[[12]][2]), (max(model[[12]][2])-min(model[[12]][2]))/100)
  yv <- coef(model)[1]+coef(model)[2]*xv 
  se <- sqrt(summary(model)[[6]]^2*(1/n+(xv-xm)^2/ssx))
  ci <- s.t * se 
  uyv<- yv + ci 
  lyv<- yv - ci 
  lines(xv, uyv, lty=2) 
  lines(xv, lyv, lty=2)
} 
plot(X, Y, pch = 16) 
abline(model)
ci.lines(model)

# Another method
X = trees$Girth
Y = trees$Height 
model <- lm(Y~X) 
plot(X, Y, pch = 16, ylim=c(60,95)) 
xv <- seq(8,22,1) 
y.c <- predict(model,list(X=xv),int="c") # “c”: 95% CI 
y.p <- predict(model,list(X=xv),int="p") # “p”: prediction 
matlines(xv, y.c, lty=c(1,2,2), lwd=2, col="black")
matlines(xv, y.p, lty=c(1,2,2), lwd=1, col=c("black","grey","grey"))


# Error bars (for categorical levels)
x1 <- rep( 0:1, each=500 )
x2 <- rep( 0:1, each=250, length=1000 ) 
y <- 10 + 5*x1 + 10*x2 - 3*x1*x2 + rnorm(1000,0,2) #values 
fit1 <- lm( y ~ x1*x2 ) 
newdat <- expand.grid( x1=0:1, x2=0:1 ) 
pred.lm.ci <- predict(fit1, newdat, interval='confidence') 
pred.lm.pi <- predict(fit1, newdat, interval='prediction') 
pred.lm.ci
pred.lm.pi
# function for plotting error bars from http://monkeysuncle.stanford.edu/?p=485 
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){ 
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper)) 
    stop("vectors must be same length") 
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
barx <- barplot(pred.lm.ci[,1], names.arg=1:4, col="blue", axis.lty=1, ylim=c(0,28), xlab="Levels", ylab="Values")

# Error bar for confidence interval
error.bar(barx, pred.lm.ci[,1], pred.lm.ci[,2]-pred.lm.ci[,1],pred.lm.ci[,1]-pred.lm.ci[,3])
# Error bar for prediction interval
error.bar(barx, pred.lm.pi[,1], pred.lm.pi[,2]-pred.lm.pi[,1],pred.lm.pi[,1]-pred.lm.pi[,3],col='red')


# Model checking
par(mfrow=c(2,2))
plot(model)


# Leverage
leverage <- hat(model.matrix(model))


# Cook’s distance
cooks.distance(model)


# Model update
model2 <- update(model,subset=(X != 15))
summary(model2)
# Slope
coef(model2)[2] # 1.054369 
model2$coefficients[2] # 1.054369 


# R code and results
reg.tree <- lm(Volume~Height, data=trees)
reg.tree
summary(reg.tree)

# Check the model
par
plot(lm(Volume~Height, data=trees))


# Check the model
library(carData)
library(car)
fit = lm(Girth ~ Height, data = trees)
# Computes residual autocorrelations and generalized Durbin-Watson statistics 
# and their bootstrapped p-values
durbinWatsonTest(fit) # check independence
# P < 0.05, autocorrelation exists

# component + residual plots (also called partial-residual plots) for linear
# and generalized linear models
crPlots(fit) # check linearity
# the red line (regression) and green line (residual) match well, the linearity is good

# Score Test for Non-Constant Error Variance
ncvTest(fit)
# p = 0.15, error variance is homogeneous


# Pearson Product Moment Coefficient of Correlation, r
# correlation coefficient and p value
cor(X, Y, use = 'pairwise.complete.obs') # 0.5192801
cor.test(X, Y, alternative = c("two.sided"), method = c("pearson"))$p.value # 0.002757815

# correlation coefficient
r = SSXY/(SSX*SSY)^.5 # 0.5192801


# Correlations with significance levels
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
# mtcars is a dataframe
head(mtcars)
rcorr(as.matrix(mtcars),type = "pearson") # parametric
rcorr(as.matrix(mtcars),type = "spearman") # nonparametric


# R
head(trees)
reg.tree = lm(Volume~Height, data=trees)
summary(reg.tree)
plot(trees$Height, trees$Volume) # x-y plot
plot(reg.tree$fitted, reg.tree$resid) # check homogeneity
shapiro.test(reg.tree$resid) # check normality
summary(lm(Volume~Height, trees))$r.squared # R square 0.3579026


# R code for producing a correlation scatter-plot matrix
## put (absolute) correlations on the upper panels
## with size proportional to the correlations
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) # ranges of coordinates 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(mtcars[,1:7], lower.panel = panel.smooth,
      upper.panel = panel.cor)

# R code for producing a correlation scatter-plot matrix for ordered-categorical data
panel.cor.ordered.categorical <- function(x, y, digits=2, prefix="", cex.cor) { 
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman")) # notive we use spearman, non parametric correlation here 
  r.no.abs <- cor(x, y, method = "spearman")
  txt <- format(c(r.no.abs , 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  test <- cor.test(x,y, method = "spearman") 
  # borrowed from printCoefmat 
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2)
}

panel.smooth.ordered.categorical <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, iter = 3, point.size.rescale = 1.5, ...)
  
{
  #require(colorspace) 
  require(reshape) 
  z <- merge(data.frame(x,y), melt(table(x ,y)),sort =F)$value 
  #the.col <- heat_hcl(length(x))[z] 
  z <- point.size.rescale*z/ (length(x)) # notice how we rescale the dots accourding to the maximum z could have gotten
  symbols(x, y,circles = z,#rep(0.1, length(x)), #sample(1:2, length(x), replace = T) , inches=F, bg= "grey",#the.col , g = bg, add = T)
  #points(x, y, pch = pch, col = col, bg = bg, cex = cex) 
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth, ...)
} 

panel.hist <- function(x, ...) { 
  usr <- par("usr")
  on.exit(par(usr)) 
  par(usr = c(usr[1:2], 0, 1.5) ) 
  h <- hist(x, plot = FALSE, br = 20) 
  breaks <- h$breaks
  nB <- length(breaks) 
  y <- h$counts
  y <- y/max(y) 
  rect(breaks[-nB], 0, breaks[-1], y, col="orange", ...)
}

pairs.ordered.categorical <- function(xx,...) { 
  pairs(xx , diag.panel = panel.hist , lower.panel=panel.smooth.ordered.categorical, upper.panel=panel.cor.ordered.categorical, cex.labels = 1.5, ...)
}

# Example
library(reshape)
set.seed(666) 
a1 <- sample(1:5, 100, replace = T) 
a2 <- sample(1:5, 100, replace = T) 
a3 <- round(jitter(a2, 7) ) 
a3[a3 < 1 | a3 > 5] <- 3
a4 <- 6-round(jitter(a1, 7) ) 
a4[a4 < 1 | a4 > 5] <- 3
aa <- data.frame(a1,a2,a3, a4) 
require(reshape) 
# plotting :)
pairs.ordered.categorical(aa)