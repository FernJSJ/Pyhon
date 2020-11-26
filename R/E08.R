# The sample statistics 
A = read.table(header = T, text ="
Year Area1 Area2 Area3 
1 11.3 14.1 6.9
2 10.4 14 11.2
3 9.9 13 8.7
4 8.2 11.4 3.3
5 10.1 11.9 8.7
6 10.7 13.8 12.5
7 11 14.9 8.9
8 7.1 8.5 3.7
9 14.7 14.5 12.1
10 5.4 9 4.1
11 7.3 7.6 5.6
12 10.2 10.9 7.3
13 6.1 9.9 6.8
14 9.7 13.2 6.6
15 8.1 9.4 4
16 11.3 11.8 4.9
17 8.8 11.5 8.8
18 9.4 11.6 5.7
19 7.5 11.4 4.9
20 8.8 10.7 7.2
21 7.5 11.1 7
22 9.1 13.2 8.9
23 6.8 9.8 7.6")

A = A[, c(2:4)] # The covariance matrix
apply(A, 2, mean) # mean
# variance-covariance matrix
S = var(A)
cor(A) # correlation matrix
eigen(S)

# Principal Component Analysis
X = sample(1:100, 50, rep = TRUE) 
# Y = X + rnorm(50, 0, 20) 
Y = jitter(X, factor = 1, amount = 40)
plot(X, Y, pch = 21, cex = 3, col = "blue", bg = "darkgreen", lwd = 1, axes = FALSE, xlab = "", ylab = "")
abline(lm(Y~X), lwd=3)


# R code: PCA
head(mtcars)
############### PCA ###############
## vary by orders of magnitude, so scaling is appropriate
pca1 <- princomp(mtcars) # inappropriate
# use the correlation matrix NOT the covariance matrix 
# =^= prcomp(ibis.pre, scale=TRUE)
pca2 <- princomp(mtcars, cor = TRUE)
plot(pca2) # shows a scree plot
biplot(pca2)
summary(pca2)
pca2$loadings # in R princomp(), the loadings are coefficients (eigenvectors)
pca2$scores # principal component vector
eigen(var(scale(mtcars))) [[2]] # coefficients, eigenvectors


# R code: Factor Analysis
# Exploratory Factor Analysis (Maximum Likelihood) 
# extracting 3 factors, with varimax rotation 
fit <- factanal(scale(mtcars), 3, rotation = "varimax")
print(fit, digits = 2, cutoff = .01, sort = TRUE)


# R: Result
# plot factor 1 by factor 2 
load <- fit$loadings[, 1:2] 
plot(load, type = "n") # set up plot 
text(load, labels = names(mtcars), cex = 1) # add variable names
abline(h = -1:1, v = -1:1, col = "lightgray", lty=1)


# R code: Factor Analysis
library(MASS) 
COV = cov(mtcars) 
FA <- factanal(covmat = COV, factor = 3, rotation = "varimax") 
load <- loadings(FA)
rot <- FA$rot
#Pairs of factor loadings to plot 
ind <- combn(1:3,2) 
par(mfrow = c(2,2)) 
nms <- row.names(load) 
#Loop over pairs of factors and draw each plot 
for (i in 1:3){ 
  eqscplot(load[,ind[1,i]], load[,ind[2,i]], xlim = c(-1,1), 
           ylim = c(-0.5,1.5), type = "n", 
           xlab = paste("Factor", as.character(ind[1,i])), 
           ylab = paste("Factor", as.character(ind[2,i])))

  text(load[,ind[1,i]],load[,ind[2,i]], labels = nms) 
  arrows(c(0,0), c(0,0), rot[ind[,i], ind[,i]][,1],
rot[ind[,i], ind[,i]][,2], length = 0.1)}


# R code: Correspondence analysis
library(ca) # Package for Correspondence analysis
data(author)
ca(author)
plot(ca(author)) # One example
# Generate 5 species density data at 120 sites 
sp1 = round(rnorm(120, 20, 7))
sp1 = abs(sp1) 
sp2 = round(rnorm(120, 40, 7)) 
sp3 = round(rnorm(120, 60, 7)) 
sp4 = round(rnorm(120, 80, 7)) 
sp5 = round(rnorm(120, 100,7)) 
species = data.frame(sp1, sp2, sp3, sp4, sp5)
species = species * 0 
sp.1 = table(sp1) 
sp.2 = table(sp2) 
sp.3 = table(sp3) 
sp.4 = table(sp4) 
sp.5 = table(sp5)
sp.5 = sp.5[1:(nrow(sp.5)-2)] 
species$sp1[c(as.numeric(names(sp.1)))] = sp.1 
species$sp2[c(as.numeric(names(sp.2)))] = sp.2 
species$sp3[c(as.numeric(names(sp.3)))] = sp.3 
species$sp4[c(as.numeric(names(sp.4)))] = sp.4 
species$sp5[c(as.numeric(names(sp.5)))] = sp.5
#Generate a noise 
random = matrix(sample(c(0,1),600, rep=T), nrow=120, ncol=5) 
species = species + random 
rownames(species) = c(1:120) #define row names 
ca(species) #Correspondence analysis
plot(ca(species))


# horseshoe effect
species <- read.csv("C:/Users/14256/Documents/R_JSJ/PCA species environment gradient.csv", header = T) # see following table for data 
rownames(species) <- species$site 
species <- species[,-1] #remove the first column (row names) 
pca <- princomp(species) 
biplot(pca)

library(ca) # Package for CA 
ca(species) #Correspondence analysis
plot(ca(species))

# R code: Detrended correspondence analysis (DCA)
library(permute)
library(lattice)
library(vegan)
plot(decorana(species))


# R code
library(permute)
library(lattice)
library(vegan)
library(ape)
# data standarization 
cars = apply(mtcars, 2, scale, center=TRUE, scale=TRUE)
dis <- vegdist(cars, "bray") 
dis <- vegdist(cars, "euclidean") 
dis <- vegdist(cars, "manhattan") 
dis <- vegdist(cars, "jaccard")
res <- pcoa(dis) # library(ape)
biplot(res, cars)


# R code
NMDS <- metaMDS (mtcars) # vegan 
par (mfrow = c(1,2),mar=c(5,4,3,2)) 
stressplot (NMDS)
plot (NMDS)


# Plots
NMDS <- metaMDS (mtcars) # vegan 
ordiplot(NMDS, type = "n") 
orditorp(NMDS, display = "species", col = "red", air = 0.01, cex = 1.5) # column
orditorp(NMDS, display = "sites", cex = 1, air = 0.01) # row


# Fits Environmental variables onto an Ordination and Plot Smooth Surfaces of Variables
library(permute)
library(vegan) 
perf = mtcars[,c('mpg', 'hp', 'qsec')] 
status = mtcars[,c('cyl', 'disp', 'drat', 'wt', 'gear', 'carb')]
vare.mds<- metaMDS(status) 
# Fits an Environmental Vector or Factor onto an Ordination 
ef <- envfit(vare.mds, perf, permu = 999) 
plot(vare.mds, display = "sites") 
plot(ef, p.max = 0.05)
ef <- envfit(vare.mds ~ mpg + hp, perf) 
plot(vare.mds, display = "sites") 
plot(ef) # display mpg and hp 
# Fit and Plot Smooth Surfaces of Variables on Ordination 
tmp <- with(perf, ordisurf(vare.mds, mpg, add = TRUE)) 
with(perf, ordisurf(vare.mds, hp, add = TRUE, col = "green4"))


# Grouping species
library(vegan) 
mtcars = mtcars[order(mtcars$cyl), ] 
# mtcars = mtcars[, c('mpg','cyl','disp','hp','drat','wt', 'qsec','gear', 'carb¡¯)] # remove vs & am
table(mtcars$cyl) # 11, 6, 14
NMDS = metaMDS(mtcars, k=3) # cyl = 4, 6, 8 
treat = c(rep("Treatment1",11), rep("Treatment2",6), rep("Treatment3",14)) 
colors = c(rep("red",11), rep("blue",6), rep("green",14))
ordiplot(NMDS, type = "n") 
for(i in unique(treat)) { 
  ordihull(NMDS$point[grep(i, treat),], draw = "polygon", # Plot convex hulls with colors based on treatment 
           groups = treat[treat==i], col = colors[grep(i, treat)], label = F) }
orditorp(NMDS, display = "species", col = "black", air = 0.01, cex = 1.2) 
colors = c(rep("darkred",11), rep("darkblue",6), rep("darkgreen",14))
orditorp(NMDS, display = "sites", col = colors, air = 0.01, cex = 0.8)



# Redundancy analysis
# R code
library(vegan) # rda() is in this library 
X <- matrix(rnorm(120),ncol=5) # vegan reveres the X and Y. 
Y <- matrix(rnorm(120),ncol=5) 
colnames(X) = c('X1','X2','X3','X4','X5') 
X=data.frame(X, X6 = rep(1:3, each = 8)) 
colnames(Y) = c('Y1','Y2','Y3','Y4','Y5') 
rda.results <- rda(X,Y) # X = Community data matrix; Y = Constraining matrix, typically of environmental variables 
plot(rda(X,Y), scaling =1) # Distance triplot 
plot(rda(X,Y), scaling =2) # Correlation triplot 
# mtcars 
stat = scale(mtcars[, c('cyl','disp', 'drat', 'wt', 'vs', 'am', 'gear', 'carb')]) 
perf = scale(mtcars[, c('mpg','hp', 'qsec')]) 
RDA <- rda(perf, stat) 
stat = as.data.frame(stat)
perf = as.data.frame(perf) 
RDA <- rda(perf ~., data= stat) #same
plot(RDA, scaling =1, main="Scaling1") # Distance triplot 
plot(RDA, scaling =2, main="Scaling2") # Correlation triplot
plot(RDA) # as same as the correlation triplot

goodness(RDA)
coef(RDA)
RDA$CCA$u
RDA$CCA$v
RDA$CCA$wa
RDA$CCA$biplot
cor(RDA$CCA$u[,1], RDA$CCA$wa[,1]) # 0.96
spenvcor(RDA)
summary(RDA)
plot(RDA, scaling =1, main="Scaling1") # Distance triplot
plot(RDA,perf),scaling =2,main="Scaling2") # Correlation triplot
plot(RDA, scaling =2, main="Scaling2") # Correlation triplot 
points(RDA$CCA$u[, c(1,2)], col = "green", pch=16) # no match 
points(RDA$CCA$wa[, c(1,2)]*3, col = "purple", pch=16) # match well
points(RDA$CCA$biplot[, c(1,2)]*1.2, col = "red", pch=16) # match well


# Plotting
plot(RDA, scaling=2,display = c("bp", "cn"), main = "scale 2", xlim = c(-2, 2))
spe.sc <- scores(RDA, choices = 1:2, display = "sp") 
# Calculate importance 
R2 = goodness(RDA)[,2] 
#select 2 important species/perf 
spe.sc = spe.sc[order(-R2),][1:2,] 
arrows(0, 0, spe.sc[,1], spe.sc[,2], length = 0.1, lty = 1, col = "red")
text(spe.sc[,1], spe.sc[,2]+0.05, row.names(spe.sc))
text(scores(RDA)$sites,
     row.names(scores(RDA)$sites), cex = .5)


# Explained variance
(R2 <- RsquareAdj(RDA)$r.squared) # 0.872
(R2adj <- RsquareAdj(RDA)$adj.r.squared) # 0.828


# Variance partition
ST1 = stat[,1:4] 
ST2 = stat[,5:8] 
(spe.part <- varpart(perf, ST1, ST2))
plot(spe.part, digits=2)


# Significance test
anova.cca(RDA,by="axis",step=1000)
anova.cca(RDA, by="term", step=1000)


# Canonical correspondence analysis (CCA)
# R code
library(vegan) 
stat = mtcars[, c('cyl','disp', 'drat', 'wt', 'vs', 'am', 'gear', 'carb')] 
perf = mtcars[, c('mpg','hp', 'qsec')] 
cca.cars <- cca(perf, stat)
# the total variation (inertia) in the data is: 0.07 
round(cca.cars $tot.chi, 2)
# the sum of all canonical eigenvalues (Constrained inertia): 0.06 
round(cca.cars$CCA$tot.chi, 2)
# all explanatory variables explain 88% of the total variation in the data 
cat(round(cca.cars$CCA$tot.chi /cca.cars$tot.chi*100), "% of data", "\n")
# the first two (canonical) eigenvalues are: 0.06, 0.0 
round(cca.cars$CCA$eig[1:2], 2)
# so the first two canonical axes explain 100% of the variation with the used environmental variables 
cat(round(sum(cca.cars$CCA$eig[1:2]) /cca.cars$CCA$tot.chi * 100), "%", "\n")
# but this is (the first two canonical axes explain) 88% of the total variation in the data 
cat(round(sum(cca.cars$CCA$eig[1:2]) /cca.cars$tot.chi * 100), "%", "\n")

# Triplot
plot(cca.cars, scaling = 2, main="Scaling2") # status scores are scaled by eigenvalues 
plot(cca.cars, scaling = 1, main="Scaling1") # performance scores are scaled by eigenvalues


# 3D plot
library(vegan3d) 
data(dune, dune.env) 
ord <- cca(dune ~ A1 + Moisture, dune.env) ordiplot3d(ord) 
### A boxed 'pin' version 
ordiplot3d(ord, type = "h") 
### More user control 
pl <- ordiplot3d(ord, scaling = "symmetric", angle=15, type="n") 
points(pl, "points", pch=16, col="red", cex = 0.7) 
### identify(pl, "arrows", col="blue") would put labels in better positions 
text(pl, "arrows", col="blue", pos=3) 
text(pl, "centroids", col="blue", pos=1, cex = 1) 
### Add species using xyz.convert function returned by ordiplot3d 
sp <- scores(ord, choices=1:3, display="species", scaling="symmetric") 
text(pl$xyz.convert(sp), rownames(sp), cex=0.7, xpd=TRUE) 
### Two ways of adding fitted variables to ordination plots 
ord <- cca(dune) 
ef <- envfit(ord ~ Moisture + A1, dune.env, choices = 1:3) 
### 1. use argument 'envfit' 
ordiplot3d(ord, envfit = ef) 
### 2. use returned envfit.convert function for better user control 
pl3 <- ordiplot3d(ord) 
plot(pl3$envfit.convert(ef), at = pl3$origin) 
### envfit.convert() also handles different 'choices' of axes 
pl3 <- ordiplot3d(ord, choices = c(1,3,2)) 
plot(pl3$envfit.convert(ef), at = pl3$origin) 
### vegan::ordiXXXX functions can add items to the plot 
ord <- cca(dune) 
pl4 <- with(dune.env, ordiplot3d(ord, col = Management, pch=16)) 
with(dune.env, ordiellipse(pl4, Management, draw = "poly", col = 1:4, alpha = 60))
with(dune.env, ordispider(pl4, Management, col = 1:4, label = TRUE))


# Generalized Joint Attribute Modeling
# R code
library(gjam) 
D = read.csv('d:/database/ibisdata/watersheds6-year.csv', header=T) 
head(D)
dim(D) 
xdata <- D[,c(12:24)] 
ydata <- D[,c(2:11)] 
formula <- as.formula( ~ lat + lon + Area + Elevation + Population + GDP + Footprint + Temperature + Precipitation + Rice_paddy +Water_body + wet+ elevSD )
ml <- list(ng = 2500, burnin = 500, typeNames = 'DA') 
out <- gjam(formula, xdata = xdata, ydata = ydata, modelList = ml) 
summary(out) 
specNames <- colnames(ydata) 
specColor <- rep('black', ncol(ydata)) 
specColor[ c(grep('quer', specNames), grep('cary', specNames)) ] <- 'brown' 
specColor[ c(grep('acer', specNames), grep('frax', specNames)) ] <- 'darkgreen' 
specColor[ c(grep('abie', specNames), grep('pice', specNames)) ] <- 'blue'
pl <- list(SMALLPLOTS = F, GRIDPLOTS=T, specColor = specColor)
gjamPlot(output = out, plotPars = pl)

