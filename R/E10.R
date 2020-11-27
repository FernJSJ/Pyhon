# °ëÍ¾¶ø·Ï
# MAchine learning 

# Multilayer perceptron (MLP)
require(monmlp)
# MLP Models 
# Generating New Data 
x <- as.matrix(seq(0, 10, length = 100)) 
y <- logistic(x) + rnorm(100, sd = 0.15)
# Fitting Model, require(monmlp) 
mlpModel <- monmlp.fit(x = x, y = y, hidden1 = 3, monotone = 1, n.ensemble = 100, bag = TRUE)
mlpModel <- monmlp.predict(x = x, weights = mlpModel)
# Plotting predicted value over actual values 
plot(x, y) 
for(i in 1:100){ lines(x, attr(mlpModel, "ensemble")[[i]], col = adjustcolor( "red", alpha.f = 0.1), lwd = 2) }
# one prediction 
mlpModel <- monmlp.fit(x = x, y = y, hidden1 = 3, monotone = 1, n.ensemble = 1, bag = TRUE)
mlpModel <- monmlp.predict(x = x, weights = mlpModel)
plot(x, mlpModel)

# Recurrent Neural Networks (RNNs) for time series patterns
require(rnn)
# Function for creating T and T+1 dataset
dataset <- function(data){ 
  x <- y <- c() 
  for (i in 1:(nrow(data)-2)){ 
    x <- append(x, data[i, 2]) 
    y <- append(y, data[i+1, 2])
}
  output <- cbind(x,y) 
  return(output[1:nrow(output)-1,])
}
# The classic Box & Jenkins airline data. Monthly totals of # international airline passengers, 1949 to 1960 
tt = AirPassengers # time series data 
dmn <- dimnames(.preformat.ts(tt)) 
data = as.data.frame(t(matrix(tt, 12, dimnames = dmn))) 
data = t(data); data = as.vector(data) 
data = cbind(Month=1:length(data), data)
# Plotting the Sequence 
plot(data[,2], main = "Air passengers", xlab = "Month", ylab = "Count (thousands)", lwd = 1.5,col = "darkgreen", type = "l")

# Recurrent Neural Networks (RNNs) for time series patterns
#Creating Test and Training Sets 
newData <- dataset(data = data) 
head(newData)
#Creating Test and Train Data 
rows <- sample(1:100, 100) 
trainingData <- newData[rows, ] # 1:100 in 144 
testData <- newData[-rows, ] #101:144 in 144
#Max-Min Scaling 
x <- trainingData[,1] 
y <- trainingData[,2] 
train_x <- (x - min(x))/(max(x)-min(x)) 
train_y <- (y - min(y))/(max(y)-min(y))
plot(train_x, train_y) #autocorrelation at lag 1

# Recurrent Neural Networks (RNNs) for time series patterns
#RNN Model 
RNN <- trainr(Y = as.matrix(train_y), X = as.matrix(train_x), learningrate = 0.05, momentum = 0.1, network_type = "rnn", numepochs = 300, hidden_dim = 3)
y_h <- predictr(RNN, as.matrix(train_x))
#Comparing Plots of Predicted Curve vs Actual Curve: Training Data 
plot(train_y, col = "blue", type = "l", lwd = 2, main = "Actual Curve (blue) vs Predicted Curve (red)")
lines(y_h, type = "l", col = "red", lwd = 1) 
cat("Train MSE: ", mse(y_h, train_y)) # 0.00677879

#Test Data 
testData <- newData[-rows, ] 
x <- testData[,1]
y <- testData[,2] 
test_x <- (x - min(x))/(max(x)-min(x)) 
test_y <- (y - min(y))/(max(y)-min(y)) 
y_h2 <- predictr(RNN, as.matrix(test_x)) 
#Comparing Plots of Predicted Curve vs Actual Curve: Test Data 
plot(test_y, col = "blue", type = "l", lwd = 2, main = "Actual Curve (blue) vs Predicted Curve (red)")
lines(y_h2, type = "l", col = "red", lwd = 1)
cat("Test MSE: ", mse(y_h2, test_y)) #0.060894

# Artificial Neural Networks (ANN)
library("neuralnet") #multi-layer perceptrons, backpropagation 
#Generate 50 random numbers uniformly distributed between 0 and 100 
# and store them as a dataframe 
traininginput <- as.data.frame(runif(50, min=0, max=100)) 
trainingoutput <- sqrt(traininginput)
#Column bind the data into one variable 
trainingdata <- cbind(traininginput, trainingoutput) 
colnames(trainingdata) <- c("Input", "Output")

#Train the neural network using 10 hidden layers 
#Threshold is a numeric value specifying the threshold for the partial 
#derivatives of the error function as stopping criteria. 
net.sqrt <- neuralnet(Output ~ Input, trainingdata, hidden=10, threshold=0.01)
net.sqrt <- neuralnet(Output ~ Input, trainingdata, hidden=c(5, 5), threshold=0.01, linear.output=T)
print(net.sqrt) #Plot the neural network
plot(net.sqrt)

# Artificial Neural Networks (ANN)
#Test the neural network on some training data 
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers 
net.results <- compute(net.sqrt, testdata) #Run them through the neural network
# Display results 
cleanoutput <- cbind(testdata, sqrt(testdata), as.data.frame(net.results$net.result)) 
colnames(cleanoutput) <- c("Input", "Expected_Output","Neural_Net_Output")
print(cleanoutput)

# tree
# Classification tree and regression tree
library(tree) 
# classification tree 
data(iris)
iris[1:3,]
TR1 = tree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, iris)
summary(TR1) 
plot(TR1)
text(TR1)
predict(TR1, iris[61:66,])

# regression tree 
data(mtcars) 
TR2 = tree(mpg ~ cyl + disp + hp + drat + wt + qsec, mtcars)
summary(TR2) 
plot(TR2)
text(TR2)
as.data.frame(predict(TR2, mtcars[1:10,]))

# Regression tree
library(tree) 
par(mfrow=c(1,2)) 
spe = gal[gal$species == "Hadoa texana", ] # plot regression tree (trend) 
spe = gal[gal$species == "Burbunga hillieri", ] # plot regression tree 
TREE <- tree(footprint ~ alt, data=spe) 
X <- seq(min(spe$alt),max(spe$alt), length.out = 100)
Y <- predict(TREE, list(alt=X)) 
plot(spe$alt, spe$footprint, pch=21, col="black",bg="gray", xlab = "Elevation (m)", ylab = 'HFI', main=spe$species[1])
lines(X, Y)


# Random forest does not give true partial effect
y <- c(3, 2, 1, 6, 5, 4, 9, 8, 7) 
x1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9) 
x2 <- c(2, 2, 2, 4, 4, 4, 6, 6, 6)
# multiple regression 
fit = summary(lm(y~x1+x2)) 
interception = fit[[4]][1,1] 
coef_x1 = fit[[4]][2,1] 
coef_x2 = fit[[4]][3,1] 
x.1 <- seq(min(x1)-1, max(x1)+1, length= 100) 
x.2 <- seq(min(x2)-2, max(x2)+1, length= 100) 
f <- function(x.1, x.2) { r <- interception + coef_x1*x.1 + coef_x2*x.2 }
y.pred <- outer(x.1, x.2, f) 
filled.contour(x.1, x.2, y.pred, main='', color = terrain.colors, xlab=expression(paste(X[1])), ylab=expression(paste(X[2])), ylim=c(0,7), xlim=c(0,10))
points(x1/1.3, x2, pch=16, cex=y)
library(ggm) # partial correlation 
D = cbind(y, x1, x2) 
D = jitter(D, factor = .01) 
pcor(c("y", "x1"), var(D)) # 0.8 
pcor(c("y", "x1","x2"), var(D)) # -0.9999
resid.y = residuals(lm(y ~ x2)) 
resid.x1 = residuals(lm(x1 ~ x2)) 
cor(resid.y, resid.x1) # -1
# Remove the effect of X2 
plot(x1, y, ylim=c(-2,10), pch=1, cex=3, xlab=expression(paste(X[1])), ylab="Y") 
points(x1, resid.y, col=adjustcolor( "red", alpha.f = 0.5), pch=17, cex=2) 
points(x1, resid.x1, col=adjustcolor( "blue", alpha.f = 0.5), pch=16, cex=2) 
legend("topleft", c("Y", "Residual of lm(Y ~ X2)", "Residual of lm(X1 ~ X2)"), pch=c(1, 17, 16), col=c(1,2,4))
# random forest 
RF = randomForest(y ~ x1 + x2, importance = TRUE, ntree = 1000)
x.1 <- seq(min(x1), max(x1), length = 100) 
x.2 <- seq(min(x2), max(x2), length = 100) 
data = expand.grid(x.1, x.2) 
names(data) = c('x1','x2') 
Y = predict(RF, newdata = data) 
Y = matrix(Y, nrow=100, ncol=100) 
filled.contour(x.1, x.2, Y, main=paste(''), color = terrain.colors, xlab=expression(paste(X[1])), ylab=expression(paste(X[2])))
# Plot partial effects 
op <- par(mfrow = c(1, 2),mar = c(4,4,2,2)) 
D <- cbind(x1, x2, y) 
partialPlot(RF, D, x1, xlab = expression(paste(X[1])), ylab='Y', main='')
partialPlot(RF, D, x2, xlab = expression(paste(X[2])), ylab='Y',main='')





























