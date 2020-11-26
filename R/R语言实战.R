# 第一章
# 1-1 会话实例
age <- c(1,2,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
mean(weight)
sd(weight)
cor(age,weight)
plot(age,weight)

# 1-3 使用新的包
help.start()
install.packages("vcd")
help(package="vcd")
library(grid)
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)

# 第二章
# 2-1 创建矩阵
y <- matrix(1:20,nrow=5,ncol=4)
y
cells <- c(1,26,24,68)
rnames <- c("R1","R2")
cnames <- c("C1","C2")
mymatrix <- matrix(cells,nrow=2,ncol=2,byrow=TRUE,dimnames=list(rnames,cnames))
mymatrix
mymatrix <- matrix(cells,nrow=2,ncol=2,byrow=FALSE,dimnames=list(rnames,cnames))
mymatrix

# 第三章
# 3-1
attach(mtcars)
plot(wt,mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)

# 3-2
does <- c(20,30,40,45,60)
drugA <- c(16,20,27,40,60)
drugB <- c(15,18,25,31,40)
plot(does,drugA,type = "b")

opar <- par(no.readonly = TRUE)
par(lty=2,pch=17)
plot(does,drugA,type = "b")
par(opar)

# 3.3.2 颜色
n <- 10
mycolors <- rainbow(n)
pie(rep(1,n),labels=mycolors,col=mycolors)
mygrays <- gray(0:n/n)
pie(rep(1,n),labels=mygrays,col=mygrays)

# 3-1 使用图形参数控制图形外观
does <- c(20,30,40,45,60)
drugA <- c(16,20,27,40,60)
drugB <- c(15,18,25,31,40)
opar <- par(no.readonly = TRUE)
par(pin=c(2,3))
par(lwd=2,cex=1.5)
par(cex.axis=.75,font.axis=3)
plot(does,drugA,type = "b",pch=19,lty=2,col="red")
plot(does,drugB,type = "b",pch=23,lty=6,col="blue",bg="green")
par(opar)

# 3-2 自定义坐标轴
x <- c(1:10)
y <- x
z <- 10/x

opar <- par(no.readonly = TRUE)
par(mar=c(5,4,4,8)+0.1)
plot(x,y,type = "b",pch=21,col="red",yaxt="n",lty=3,ann=FALSE)

lines(x,z,type = "b",pch=22,col="blue",lty=2)
axis(2,at=x,labels=x,col.axis="red",las=2)
axis(4,at=z,labels=round(z,digit=2),col.axis="blue",las=2,cex.axis=0.7,tck=-.01)
mtext("y=1/x",side=4,line=3,cex.lab=1,las=2,col="blue")
title("An example of creative axes",xlab="X values",ylab="Y+X")
par(opar)

# 3-3 依剂量对比药物A和药物B的响应情况
does <- c(20,30,40,45,60)
drugA <- c(16,20,27,40,60)
drugB <- c(15,18,25,31,40)

opar <- par(no.readonly = TRUE)
par(lwd=2,cex=1.5,font.lab=2)
plot(does,drugA,type = "b",pch=15,lty=1,col="red",ylim=c(0,60),main="Drug A vs. Drug B",xlab="Drug Dosage",ylab="Drug Response")

lines(does,drugB,type="b",pch=17,lty=2,col="blue")
abline(h=c(30),lwd=1.5,lty=2,col="gray")
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
minor.tick(nx=3,ny=3,tick.ratio=0.5)
legend("topleft",inset=.05,title="Drug Type",c("A","B"),lty=c(1,2),pch=c(15,17),col=c("red","blue"))
par(opar)

# 3-4 多幅图形布局的精细控制
opar <- par(no.readonly = TRUE)
par(fig=c(0,0.8,0,0.8))
plot(mtcars$wt,mtcars$mpg,xlab="Miles per gallon",ylab="Car weight")

par(fig=c(0,0.8,0.55,1),new=TRUE)
boxplot(mtcars$wt,horizontal = TRUE,axes=FALSE)
par(fig=c(0.65,1,0,0.8),new=TRUE)
boxplot(mtcars$mpg,axes=FALSE)

mtext("Enhanced scatterplot",side=3,outer=TRUE,line=-3)
par(opar)

# 第四章
# 4-1 创建leadership数据框
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, gender, age, q1, q2, q3, q4, q5, stringsAsFactors = FALSE)

# the individual vectors are no longer needed
rm(manager, date, gender, age, q1, q2, q3, q4, q5)

# 4-2 创建新变量
mydata <- data.frame(x1 = c(2, 2, 6, 4), x2 = c(3, 4, 2, 8))
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2

attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)

mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2)/2)

# 4-3 使用is.na()函数
is.na(leadership[,6:10])

# 第五章
# 5-3 生产服从多元正态分布的数据
library(MASS)
options(digits=3)
set.seed(1234)

mean <- c(230.7, 146.7, 3.6)                                           
sigma <- matrix( c(15360.8, 6721.2, -47.1,                              
                   6721.2, 4700.9, -16.5,
                   -47.1,  -16.5,   0.3), nrow=3, ncol=3)

mydata <- mvrnorm(500, mean, sigma)                                     
mydata <- as.data.frame(mydata)                                         
names(mydata) <- c("y", "x1", "x2")                                       

dim(mydata)                                                             
head(mydata, n=10)   

# 第二部分 基本方法
# 第六章 基本图形
# 6-1 简单的条形图
library(grid)
library(vcd)
counts <- table(Arthritis$Improved)
counts

barplot(counts,main="Simple bar plot",xlab = "Improvement",ylab = "frequency")

barplot(counts,main = "Horizontal bar plot",xlab = "Frequency",ylab = "Improvemnet",horiz = TRUE)


# 6-2 堆砌条形图和分组条形图
library(grid)
library(vcd)
counts <- table(Arthritis$Improved,Arthritis$Treatment)
counts

barplot(counts,main = "Stacked bar plot",xlab = "Treatment",ylab = "Frequency",col = c("red","yellow","green"),legend=rownames(counts))

barplot(counts,main = "Grouped bar plot",xlab = "Treatment",ylab = "Frequency",col = c("red","yellow","green"),legend=rownames(counts),beside = TRUE)

# 6-3 排序后均值条形图
states <- data.frame(state.region,state.x77)
means <- aggregate(states$Illiteracy,by=list(state.region),FUN=mean)
means

means <- means[order(means$x),]
means

barplot(means$x,names.arg = means$Group.1)
title("Mean illiteracy rate")

# 6-4 为条形图搭配标签
par(mar=c(5,8,4,2))
par(las=2)
counts <- table(Arthritis$Improved)

barplot(counts,main = "Treatment utcome",horiz = TRUE,cex.names = 0.8,names.arg = c("No improvement","Some improvement","Marked improvement"))

# 棘状图
library(grid)
library(vcd)
attach(Arthritis)
counts <- table(Treatment,Improved)
spine(counts,main = "Spinogram example")
detach(Arthritis)

# 6-5 饼图
par(mfrow = c(2, 2))
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "Australia", "Germany", "France")

pie(slices, labels = lbls, main = "Simple Pie Chart")

pct <- round(slices/sum(slices) * 100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
pie(slices, labels = lbls2, col = rainbow(length(lbls)), 
    main = "Pie Chart with Percentages")

library(plotrix)
pie3D(slices, labels = lbls, explode = 0.1, main = "3D Pie Chart ")

mytable <- table(state.region)
lbls <- paste(names(mytable), "\n", mytable, sep = "")
pie(mytable, labels = lbls, 
    main = "Pie Chart from a Table\n (with sample sizes)")

