hist(rbinom(100000, 100, .5), freq = F, main = "", xlab = 'Number of head', ylab = 'Probability density')
pbinom(25,100,.5)
qbinom(0.05,100,.5)

# t-test energy intake in kJ for 11 women
daily.intake = c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)

t.test(daily.intake,mu=7725)
# Nonparametric
wilcox.test(daily.intake,mu=7725)
# Two samples
x1 = rnorm(300,0,1)
x2 = sample(0:100,300,rep = T)
t.test(x1,x2)
# Check normality
plot(x1)
hist(x1)
qqnorm(x1)
shapiro.test(x1)

# Shapiro-Wilk normality test
shapiro.test(rnorm(5000,mean = 5,sd=100))
shapiro.test(runif(30,min = 2,max = 4))

# Quantile-Quantile plot
y <- rnorm(200,sd = 10)
qqnorm(y)
qqline(y,col=2)

# The central limit theorem
# Exponential distribution is a skewed distribution
y <- rexp(100)
hist(y,col = 'grey')

# Define a vector with length 200
sample.mean <- numeric(200)

# The mean of 200 samoles
for(i in 1:200) sample.mean[i] <- mean(rexp(100))

# A normal distrbution appearss
hist(sample.mean,col = 'grey')
shapiro.test(sample.mean)

# Z test statistic
pnorm(z)
t.test(x,mu=0,var.equal = T)

# T test statistic
t.test(x,mu=0,var.equal = F)

# Plot a normal distribution and t distributions
X <- seq(-4,4,len = 1000)
Z <- dnorm(X,0,1)
plot(X,Z,type = 'l',col='blue') # the normal curve
segments(1.96,-0.05,1.96,0.1,col='brown')
segments(-1.96,-0.05,-1.96,0.1,col = 'brown')
abline(0,0)
lines(X,dt(X,15),col='green')
lines(X,dt(X,4),col='red')

# Two-tailed test
t.test(x,mu=100,alt="two.sided")

# Right-tailed test
t.test(x,mu=100,alt="greater")

# Left-tailed test
t.test(x,mu=0,alt="less")

qt(0.01, 22) # -2.508

qnorm(0.01)

# one sample hypothesis tests:proportions
prop.test(x,n,p=null,alternative = c("two.sided","less","greater"),conf.level = 0.95,correct = TRUE)
prop.test(0.77*300,300,p=0.8)

# paired tests
# input data
lines <- 
  "ID x y 
1 208 197 
2 202 150 
3 203 255 
4 200 134 
5 205 266 
6 206 200 
7 207 189 
8 208 186 
9 203 215 
10 210 199"
score <- read.table(con <-textConnection(lines),header = TRUE)
close(con)
t.test(score$x,score$y,paired = T)

# Two samples:common variance,one population
t.test(x,y,var.equal = TRUE)

# Two samples:not common variance,one/two population
t.test(x,y,var.equal = FALSE)# Welch's t-test

# A test for equal proportion of one population
prop.test(12,20,p=0.5) # it is a chi square test

# Two population proportions
x = sample(c('A','B'),50,rep = T)
y = sample(c('F','M'),50,rep = T)
table(x,y)
prop.test(table(x,y),correct = TRUE)

pbinom(8-1, 20, .25, lower.tail = F)# 0.102

# A two-sample t test(two tails)
t.test(B,C,var.equal = TRUE)

# A two-sample t test (one tail)
t.test(present,newer,alt="less",var.equal = T)

# input data 
lines <-
  "ID x y
1 208 197
2 202 150
3 203 255
4 200 134
5 205 266
6 206 200
7 207 189
8 208 186
9 203 215
10 210 199"
weight<-read.table(con <- textConnection(lines),header = TRUE)
close(con)
# cheak data
weight
head(weight)

# t test
t.test(weight$x,weight$y,var.equal = F) # two samples
t.test(weight$x,mu = 200) # one sample
shapiro.test(weight$x) # check normal distribution
shapiro.test(weight$y)
hist(weight$y)