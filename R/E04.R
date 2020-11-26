# F distribution
df.1 <- 10
df.2 <- 1
f.d <- df(seq(0.1,15,by=0.1),df.1,df.2)
plot(f.d,xlab = 'X',ylab = 'p',type = 'l',xlim = c(0,15),ylim = c(0,1))
for (df.2 in 1:10) {
  f.d <- df(seq(0.1,15,by=0.1),10,df.2)
  lines(f.d,type = 'l',col=rainbow(10)[df.2])
  legend(df.2*1.3-1,1,paste('df=',df.2,sep = ' '),text.col = rainbow(10)[df.2],box.lty = 0,cex = 1)
}


# Input data and check data
# One way ANOVA
# Rodent weight at 7 sites
site1 <- c(9.4,8.7,13.3,13.6,15,15.2,17.7,18.6,22.2)
site2 <- c(16.8,30.8,33.6,40.5,48.9)
site3 <- c(27.0,28.9,32,32.7,35.5,45.6)
site4 <- c(21.0,23.4,27.5,27.5,30.5,31.9,32.5,33.8,33.8)
site5 <- c(24.3,29.7,19.9)
site6 <- c(17.7,19.7,21.5,27.9,34.8,40.2)
site7 <- c(16.5,20.7,23.5,26.4,26.7,29.5,29.8,31.9,35.5)

rodent.survey <- data.frame(weight = c(site1,site2,site3,site4,site5,site6,site7),site=factor(c(rep("1",9),rep("2",5),rep("3",6),rep("4",9),rep("5",3),rep("6",6),rep("7",9))))

options(digits = 3) # default value = 7
tapply(rodent.survey$weight,rodent.survey$site,mean)

tapply(rodent.survey$weight,rodent.survey$site,var)

tapply(rodent.survey$weight,rodent.survey$site,sum)

boxplot(weight~site,data = rodent.survey,xlab = 'Sites',ylab = 'Weight')

rodent.survey


# Model and results
# One way ANOVA(completely randomized design)
fit <- aov(weight~site,data=rodent.survey)
summary(fit)

#fit = lm(weight~site,data = rodent.survey)
#anova(fit)

# report the means and the number of subjects/cell
print(model.tables(fit,"means"),digits = 3)


# Predicted values and residuals
# ANOVA fit
fit[[1]] # coefficients
fit[[2]] # residuals
fit[[5]] # predicted (fitted.values)
all <- cbind(rodent.survey,predicted=fit[[5]],residual=fit[[2]])
head(all)

# Model performance
par(mfrow=c(2,2))
plot(fit)


# Residuals normal?
shapiro.test(residuals) # fit[[2]]
# ±¨´í
shapiro.test(fit[[2]])


# Bartlett's test-example
# Bartlett test of homogeneity of variances(parametric)
bartlett.test(weight~site,data = rodent.survey)

# Figner-kileen test of homogeneity of variances(non-parametric)
fligner.test(weight~site,data = rodent.survey)


# Post-ANOVA comparisons
# Tukey's multiple comparisons tests(Honestly significantly diffierent(HSD) test)
TukeyHSD(fit)$site


# Model-with Tukey test
fit <- aov(Width~Host,data = tick)
TukeyHSD(fit)$Host


# R script
datafilename <- "http://personality-project.org/R/datasets/R.appendix1.data" #tell where the data come from
data.ex1 <- read.table(datafilename, header = T) #read the data into a table
aov.ex1 <- aov(Alertness ~ Dosage, data = data.ex1) #do the analysis of variance
print(model.tables(aov.ex1,"means"), digits = 3) #report the means and the number of subjects/cell
boxplot(Alertness ~ Dosage, data = data.ex1) #graphical summary
summary(aov.ex1) #show the results


# R script-randomized block design
#Randomized Block Design
#Carbon dioxygen density at 8 incubators and 4 treatments
CO2 <- data.frame(ID=1:32, group=NA, treat=NA, density=NA)
n <- 0
for(i in 1:8){ 
  for(j in c('A','B','C','D')){ 
    n <- n+1 
    CO2$group[n] = i 
    CO2$treat[n] = j
  }}
CO2$group <- factor(CO2$group)
CO2$treat <- factor(CO2$treat)
CO2$density <- c(5.27,5.27,5.94,5.53,5.27,5.22,4.88,4.96,5.88,5.83, 5.38,5.53,5.44, 5.38,5.27,5.32,5.66, 5.44,5.38,4.88,6.22,6.22,5.61,5.92,5.83,5.72,5.38,4.88,5.27,5.11,5.12,4.44)

fit1 <- aov(density ~ treat, data = CO2) # one way ANOVA
fit2 <- aov(density ~ group + treat, data = CO2) # Randomized Block Design

summary(fit1)
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)


# Reshape data
head(CO2)
library(reshape2)
Tab = melt(CO2,id=c("group","treat"),na.rm = TRUE)
Mat = acast(Tab,group~treat~variable)
dim(Mat)
Mat[,,2]


# R script for two-way interaction plot
# Two-way interaction plot
attach(mtcars)
gears <- factor(gear)
cyl <- factor(cyl)
interaction.plot(cyl,gear,mpg,type = 'b',col = c(1:3),leg.bty = 'o',leg.bg = 'beige',lwd=2,pch = c(18,24,22),xlab = 'Number of cylinders',ylab = 'Mean miles per gallon',main='Interaction plot')

# leg.bty:legend boundary type(0,n,1)
# leg.bg:legend background


# Check homogeneity of variance
# two explanatory variables
head(mtcars)
str(mtcars)
mtcars$cyl = as.factor(mtcars$cyl)
mtcars$gear = as.factor(mtcars$gear)

library(carData)
library(car)
leveneTest(mpg~cyl*gear,data=mtcars)


# R-two way ANOVA
# Two way ANOVA
weight.gain <- data.frame(ID=1:60,amount=NA,food=NA,gain=NA)
n <- 0
for (i in c('heigh','low')) {
  for (j in c('beef','cereal','port')) {
    for (k in 1:10) {
      n <- n+1
      weight.gain$amount[n] = i
      weight.gain$food[n] = j
    }
  }
}
weight.gain$gain <- c(73,102,118,104,81,107,100,87,117,111, 98,74,56,111,95,88,82,77,86,92, 94,79,96,98,102,102,108,91,120,105, 90,76,90,64,86,51,72,90,95,78, 107,95,97,80,98,74,74,67,89,58,49,82,73,86,81,97,106,70,61,82)

fit <- aov(gain~amount + food + amount:food,data = weight.gain)
fit <- aov(gain~amount*food,data = weight.gain) # same thing

summary(fit)
par(mfrow=c(2,2))
plot(fit)

# Because the interaction yerm is not significant,the final model is:
fit <- aov(gain~amount+food,data = weight.gain)


# Quick R(http://www.statmethods.net/)
# One Way Anova (Completely Randomized Design)
attach(mtcars)
head(mtcars)
fit1 <- aov(mpg~cyl, data=mtcars)
B=cyl
A=gear
x=wt
# Randomized Block Design (B is the blocking factor)
fit2 <- aov(mpg ~ A + B, data=mtcars)
# Two Way Factorial Design
fit3 <- aov(mpg ~ A + B + A:B, data=mtcars)
fit4 <- aov(mpg ~ A*B, data=mtcars) # same thing
# Analysis of Covariance
fit5 <- aov(mpg ~ A + x, data=mtcars)
summary(fit1) # display Type I ANOVA table
drop1(fit1,~.,test="F") # type III SS and F Tests


# How to check independence
par(mfrow=c(1,2))
D = rnorm(30)
plot(D[1:29],D[2:30])
D.order = sort(D)
plot(D.order[1:29],D.order[2:30])

#Model performance
par(mfrow=c(2,2))
plot(fit)


# Two way ANOVA without replication
fit <- aov(W~plot+tpye,data = mydata)


# Dunnett's test
Group <- factor(c("A","A","B","B","B","C","C","C","D","D","D","E","E","F","F","F"))
Value <- c(5,5.09,4.63,4.58,4.72,5,5.08,4.24,5.09,5.19,4.58,6.16,6.85,7.68,7.07,6.48)
data <- data.frame(Group, Value)
aov <- aov(Value ~ Group, data)
library(mvtnorm)
library(survival)
library(TH.data)
library(MASS)
library(multcomp)
summary(glht(aov, linfct=mcp(Group="Dunnett")))


# Quiz
data(mtcars)
nrow(mtcars) #32
mtcars$cyl = as.factor(mtcars$cyl)
levels(mtcars$cyl) # 4,6,8
model=aov(mpg~cyl,data=mtcars)
summary(model)


# R script
# Three way ANOVA
Dat = read.table('d:/ioz/statistics/2015/3way.ANOVA.txt', sep=' ', header=T)
Dat$species <- as.factor(Dat$species)
model <- aov(rate ~ species * temp* sex, data=Dat)
summary(model)
summary.lm(model)


# R script for another example
# Book ¡°Linear Models with R¡± by Faraway
library(car)
library(faraway)
data(abrasion)
lines <-
"id run position material wear 
1 1 1 C 235 
2 1 2 D 236 
3 1 3 B 218 
4 1 4 A 268 
5 2 1 A 251 
6 2 2 B 241 
7 2 3 D 227 
8 2 4 C 229 
9 3 1 D 234 
10 3 2 C 273 
11 3 3 A 274 
12 3 4 B 226 
13 4 1 B 195 
14 4 2 A 270 
15 4 3 C 230
16 4 4 D 225"

abrasion.data <- read.table(con <-textConnection(lines), header=TRUE)
close(con)

matrix(abrasion.data$material, 4, 4)
abrasion.data$run = as.factor(abrasion.data$run)
abrasion.data$position = as.factor(abrasion.data$position)
fit1 = aov(wear~run + position + material,abrasion.data)
fit2 = lm(wear~run + position + material,abrasion.data)
summary(fit1)
summary(fit2)


# R code
lines <-
"Length Mosquito Cage 
58.5 1 1
59.5 1 1
77.8 2 1
80.9 2 1
84.0 3 1
83.6 3 1
70.1 4 1
68.3 4 1
69.8 1 2
69.8 1 2
56.0 2 2
54.5 2 2
50.7 3 2
49.3 3 2
63.8 4 2
65.8 4 2
56.6 1 3
57.5 1 3
77.8 2 3
79.2 2 3
69.9 3 3
69.2 3 3
62.1 4 3
64.5 4 3"
dat <- read.table(con<-textConnection(lines),header=TRUE)
close(con)
dat$Mosquito = as.factor(dat$Mosquito)
dat$Cage = as.factor(dat$Cage)
# two way ANOVA
summary(aov(Length ~ Cage * Mosquito, dat))
# nested ANOVA
summary(aov(Length ~ Cage / Mosquito, dat))


# R results
# Manually computation of the F-statistic and p value is needed.
fit = summary(aov(Length ~ Cage / Mosquito, dat))
F.value = fit[[1]] [1, 3] / fit[[1]] [2, 3]
p = pf(F.value, fit[[1]] [1, 1], fit[[1]] [2, 1], lower=FALSE)
p # 0.23

# Another version: nested ANOVA
summary(aov(Length ~ Cage + Error(Cage / Mosquito),dat))


# R script
# Split-plot
# Tensile strength in paper manufacturing
Y <- c(30,35,37,36,34,41,38,42,29,26,33,36, 28,32,40,41,31,36,42,40,31,30,32,40,31,37,41,40,35,40,39,44,32,34,39,45)
block <- gl(3,12,36) # Three blocks
A <- gl(3,4,36) # The pulp preparation methods
B <- gl(4,1,36) # Four different temperatures
Dat <- cbind(Y, block, A, B)
fit <- aov(Y ~ A*B + Error(block/A))
summary(fit)

# Compare regular ANOVA
summary(aov(Y ~ A*B + block))


# 
model <- aov(yield ~ irrigation * density * fertilizer + Error(block / irrigation / density))
summary(model)



#Sphericity
mauchly.test()

# mauchly.test()
# data
Group <- c("A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B", "C","C","C","C","C","C","C","C")
Value <- c(1,2,4,1,1,2,2,3,3,4,4,2,3,4,4,3,4,5,3,5,5,3,4,6)
Participant <- c("1","2","3","4","5","6","7","8","1","2","3","4","5","6","7","8", "1","2","3","4","5","6","7","8")
data <- data.frame(Participant, Group, Value)
# make a matrix such that the rows are the within-subject factor (Participant)
# and the columns are the groups to compare (Group)
matrix <- with(data, cbind(Value[Group == "A"], Value[Group == "B"], Value[Group == "C"]))
# build a multivariate linear model with the matrix you've just created# build a multivariate linear model with the matrix you've just created
model <- lm(matrix ~ 1)

# 
# define the design of the study, make a list of the independent variable
design <- factor(c("A", "B", "C"))
# load car package, which has Anova() function including Mauchly's test
library(car)
options(contrasts=c("contr.sum", "contr.poly"))
aov <- Anova(model, idata = data.frame(design), idesign = ~design, type = "III")
summary(aov, multivariate = F)


# R ¨C repeated ANOVA
# Repeated measures ANOVA
face = read.table("d:/ioz/statistics/repeated_ANOVA/face.csv", header = T, sep = ",")
face$aspect <- as.factor(face$aspect)
face$id <- as.factor(face$id)
# id / aspect (aspect within id)
face.aov = aov(time ~ aspect + Error(id / aspect), data = face)
face.aov = aov(time ~ aspect + Error(id), data = face) # same
summary(face.aov)
# pairwise comparison
with(face, pairwise.t.test(time, aspect, p.adjust.method="holm", paired=T))


# R script - randomized block design
# Randomized Block Design
# Carbon dioxygen density, 8 incubators and 4 treatments
CO2 <- data.frame(ID=1:32, group=NA, treat=NA, density=NA)
n <- 0
for(i in 1:8){ 
  for(j in c('A','B','C','D')){ 
    n <- n+1 
    CO2$group[n] = i 
    CO2$treat[n] = j
  }}
CO2$group <- factor(CO2$group)
CO2$treat <- factor(CO2$treat)
CO2$density <- c(5.27,5.27,5.94,5.53,5.27,5.22,4.88,4.96,5.88,5.83, 5.38,5.53,5.44, 5.38,5.27,5.32,5.66, 5.44,5.38,4.88,6.22,6.22,5.61,5.92,5.83,5.72,5.38,4.88,5.27,5.11,5.12,4.44)
fit1 <- aov (density ~ treat,data = CO2)# one way ANOVA
fit2 <- aov (density ~ group + treat, data = CO2) # Randomized Block Design 
library(Matrix)
library(lme4)
fit3 <- lmer(density ~ treat + (1|group),CO2) # mixed effect model
summary(fit3) 
# Randomized block design:compare with results from mixed-effect model
summary(fit2) # fit2 <- aov (density ~ group + treat, data = CO2)
summary(fit3) # lmer(density ~ treat + (1|group), CO2)
anova(fit3) # lmer(density ~ treat + (1|group), CO2)



# R script ¨C split plot
# Crop products
Y <- c(30,35,37,36,34,41,38,42,29,26,33,36, 28,32,40,41,31,36,42,40,31,30,32,40,31,37,41,40,35,40,39,44,32,34,39,45)
block <- gl(3,12,36) # Three blocks
A <- gl(3,4,36) # Three different fertilizers
B <- gl(4,1,36) # Four different pesticides
Dat <- data.frame(Y, block, A, B)
head(Dat)
model <- aov(Y ~ A*B + Error(block/A)) # split plot
# Mixed-effect models
library(lme4)
library(nlme)
model1 <- lme(Y ~ A*B, random=~1|block/A, data=Dat) 
summary(model1)
model2 <- lmer(Y ~ A*B+(1|block/A), data=Dat)
summary(model2)
anova(model2)


# Repeated measure design for time series data
# Crop products
Y <- c(30,35,37,36,34,41,38,42,29,26,33,36, 28,32,40,41,31,36,42,40,31,30,32,40,31,37,41,40,35,40,39,44,32,34,39,45)
block <- gl(3,12,36) # Three blocks
time <- gl(8,2,36)
time <- as.numeric(time) # 8 time periods
B <- gl(4,1,36) # Four different pesticides
Dat <- data.frame(Y, block, time, B)
head(Dat)

# Mixed-effect models
library(nlme)
model <- lme(Y ~ B, random = ~ time | block, data = Dat)
summary(model)


# Mixed effect models for spatially autocorrelated data
# plot 5 autocorrelation types in package nlme
library(nlme)
par(mfrow=c(2,3))
D <- seq(from = 0, to = 1, by = 0.1)
Mydata <- data.frame(D = D)
autocor <- corSpher(c(0.8, 0.1), form = ~ D, nugget = TRUE)
autocor <- Initialize(autocor, data = Mydata)
semivar <- Variogram(autocor)
plot(semivar[,2], semivar[,1], type = "l", col = 1, xlab = 'Distance', ylab = 'Semivariogram', main = 'corSpher')


# Mixed effect model (Zuur)
Boreality <- read.table('D:/softwares/R/library/AED/data/Boreality.txt', header=T)
head(Boreality)
# Mixed effect models (Zuur, Page 168)
library(nlme)
f1 <- formula(nBor ~ Wet)
B1.gls <- gls(f1, data = Boreality)
Vario.gls <- Variogram(B1.gls, form =~ x + y, robust = TRUE, maxDist = 2000,resType = "pearson")
plot(Vario.gls, smooth = TRUE)

B1A <- gls(f1, correlation = corSpher(form =~ x + y, nugget = TRUE), data = Boreality)
B1B <- gls(f1, correlation = corLin (form =~ x + y, nugget = TRUE), data = Boreality)
B1C <- gls(f1, correlation = corRatio (form =~ x + y, nugget = TRUE), data = Boreality)
B1D <- gls(f1, correlation = corGaus (form =~ x + y, nugget = TRUE), data = Boreality)
B1E <- gls(f1, correlation = corExp (form =~ x + y, nugget = TRUE), data = Boreality)
AIC(B1A, B1B, B1C, B1D, B1E)


# R script
# Bartlett Test of Homogeneity of Variances (parametric)
bartlett.test(split(Dat$y, list(Dat$x1, Dat$x2)))
bartlett.test(Dat$y ~ Dat$x1 * Dat$x2)
# Two Way ANOVA
weight.gain=data.frame(ID=1:60, amount=NA, food=NA, gain=NA)
n=0
for(i in c('high','low')){ 
  for(j in c('beef','cereal','port')){
    for(k in 1:10){ 
      n=n+1 
      weight.gain$amount[n]=i 
      weight.gain$food[n]=j 
      }
    }
  }
weight.gain$gain=c(73,102,118,104,81,107,100,87,117,111, 98,74,56,111,95,88,82,77,86,92, 94,79,96,98,102,102,108,91,120,105, 90,76,90,64,86,51,72,90,95,78, 107,95,97,80,98,74,74,67,89,58,49,82,73,86,81,97,106,70,61,82)
fit = aov(gain ~ amount + food + amount:food, data=weight.gain)
fit <- aov(gain ~ amount * food, data=weight.gain) # same thing
summary(fit)
par(mfrow=c(2,2))
plot(fit)