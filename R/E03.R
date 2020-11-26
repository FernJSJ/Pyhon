# Example
1-pchisq(1.333,df=1) # 0.248

# Chi-square distribution
plot(table(round(rchisq(1000,df=1),1)),type = 'l',col='red',xlab = 'Chi-square value',ylab = 'Number of experiments')

hist(rchisq(1000,df=1),nclass = 30)

# PDF of Chi-square distribution
x <- seq(0.01,4,by=0.01)

plot(x,dchisq(x,1),type = 'l',xlab = 'x',ylab = 'p',xlim = c(0,4),ylim = c(0,1))

for (i in 1:5) {
  lines(x,dchisq(x,i),col=rainbow(5)[i],lwd=3)
  legend(3.2,.1-i/15,paste('k=',i,sep = ' '),lty=1,col=rainbow(5)[i],box.lty=0,cex=1)
}
# Ã»ÓÐÍ¼Àý

# Critical Chi-square
qchisq(0.95,df=1) # 3.84

# Chi square test
chisq.test(c(290,110),p=c(0.75,0.25)) # p=0.248

# Calculating the Chi-square value
chisq.test(c(315,101,108,32),p=c(9/16,3/16,3/16,1/16)) # p=0.9254

# D.F and critical value
x <- seq(0.01,10,by=0.01)
plot(x,dchisq(x,3),type = 'l',xlab = 'x',ylab = 'p',xlim = c(0,10),ylim = c(0,.3))
abline(0,0)
abline(v=0.47,lwd=3) # Mendel's result
abline(v=qchisq(.95,3),lwd=3) # 5%

# R for Fisher's exact test
TeaTasting <- matrix(c(8,2,3,7),nrow = 2,dimnames = list(Guess=c("Milk","Tea"),Truth=c("Milk","Tea")))

fisher.test(TeaTasting,alternative = "greater")

## A4 x 4 table Agresti(2002,p.57) Job Satisfaction
Job <- matrix(c(1,2,1,0,3,3,6,1,10,10,14,9,6,7,12,11),4,4,dimnames = list(income=c("<15k","15-25k","25-40k",">40k"),satisfaction=c("VeryD","LittleD","ModerateS","VeryS")))
fisher.test(Job)
fisher.test(Job,simulate.p.value = TRUE,B=1e5)

var.test()
bartlett.test()
fligner.test()

#Example
qchisq(0.01,df=29) # 14.257

# Example
qchisq(0.995,df=29) # 52.336

# Check variance
plot(count~spray,data = InsectSprays)

bartlett.test(InsectSprays$count,InsectSprays$spray)
var.test(InsectSprays$count,InsectSprays$spray)
fligner.test(InsectSprays$count,InsectSprays$spray)
fligner.test(count~spray,data = InsectSprays)

# R script
effect.size <- function(data.1, data.2){ 
  d <- (mean(data.1) - mean(data.2))/ 
    sqrt(((length(data.1) - 1) * var(data.1) + (length(data.2) - 1) * var(data.2))/ (length(data.1) + length(data.2)-2)) 
  names(d) <- "effect size d"
  return(d)
}

effect.size(rnorm(30),rnorm(50,2,1))

# demonstrate difference
plot.design(breaks ~ wool + tension, data = warpbreaks)

# Example
n=30 # sample size
sigma=120 # population standard deviation
sem=sigma/sqrt(n) # standard error
sem
alpha=.05 # significance level
mu0=10000 # hypothetical lower bound
q=qnorm(alpha,mean=mu0,sd=sem)
q # 99634

# Example
mu=9950 # assumed actual mean
pnorm(q,mean=mu,sd=sem,lower.tail = FALSE) # 0.26196


# Power
power.t.test (n = 20, delta = 1.5, sd = 2, sig.level = 0.05, type = "one.sample", alternative = "two.sided", strict = TRUE)
# where n is the sample size, delta is the effect size, and type indicates 
# a two-sample t-test, one-sample t-test or paired t-test
# power=0.8888478

# If you have unequal sample sizes, use
library (pwr) 
pwr.t2n.test (n1 = 30,n2 = 50,,d = -.5, sig.level = 0.05,alternative="less")

# sample size n and beta
qnorm(0.95,mean=100,sd=20/sqrt(30))
pnorm(106,mean=110,sd=20/sqrt(30),lower.tail = F)

# Power
# Quick-R http://www.statmethods.net/stats/power.html 
# Plot sample size curves for detecting correlations of 
# various sizes. 
library(pwr) 
# range of correlations 
r <- seq(.1,.5,.01) 
nr <- length(r) 
# power values 
p <- seq(.4,.9,.1) 
np <- length(p) 
# obtain sample sizes 
samsize <- array(numeric(nr*np), dim=c(nr,np)) 

for (i in 1:np){ 
  for (j in 1:nr){ 
    result <- pwr.r.test(n = NULL, r = r[j], sig.level = .05, power = p[i], alternative = "two.sided") 
    samsize[j,i] <- ceiling(result$n)
}}
samsize

# Plot the curves
# set up graph 
xrange <- range(r) 
yrange <- round(range(samsize)) 
colors <- rainbow(length(p)) 
plot(xrange, yrange, type="n", xlab = "Correlation Coefficient (r)", ylab = "Sample Size (n)" )
# add power curves 
for (i in 1:np){ lines(r, samsize[,i],type="l",lwd=2,col=colors[i])
}
# add annotation (grid lines, title, legend) 
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89") 
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,col="grey89") 
title("Sample Size Estimation for Correlation Studies\n Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p), fill=colors)

# Power of test
a <- 6
s <- 2 
n <- 20 
diff <- qt(0.975, df = n-1)*s/sqrt(n)
left <- a-diff 
right <- a+diff

left[1]# 5.063971
right[1] # 6.936029

assumed <- a + 1.5 
tleft <- (assumed - right)/(s/sqrt(n)) #1.261
p <- pt(-tleft, df = n-1)
p #0.1112583
1-p # 0.888

# P value and sample size
N = 10 * c(1:1000) 
P = numeric(1000)
for(i in 1:1000){ 
  t = t.test(sample(0:100, N[i], rep=T), sample(2:102, N[i], rep=T))
P[i] = t$p.value
}
plot(N, P)
lo = loess(P~N)
x1 = seq(min(N), max(N), (max(N) - min(N))/1000)
lines(x1, predict(lo,x1), col = 'red', lwd=2)

# R script
sample = rnorm(30)
m = mean(sample)
s = sd(sample)
n = length(sample)

diff <- qt(0.975, df = n-1)*s/sqrt(n)
left <- m-diff
right <- m+diff

assumed <- m + 2
tleft <- (assumed - right)/(s/sqrt(n))
p <- pt(-tleft, df = n-1)
power = 1-p

