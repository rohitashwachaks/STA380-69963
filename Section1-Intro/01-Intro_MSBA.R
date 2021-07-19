library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn) ## knn library

# Set Global Seed
set.seed(0)

# Adds Boston dataset to the Global Scope.(?)
attach(Boston)

# Take number of rows
n <- dim(Boston)[1]

#----Intro to KNN----

# Plotting %age of pop. from Lower economic status v/s Median Property Value
plot(lstat,medv,
     xlab = "%age Pop. from Lower economic status",
     ylab = "Median Property Value",
     main = "Relationship b/w Property Value & Econ. Status of Pop.")

# Both Test and Train identical?!!
# Test isn't really needed but for the sake of the function to work
# We are just showing how Knn works in this section
train <- data.frame(lstat,medv)
test <- data.frame(lstat,medv)

# Sorting test w.r.t Column 1
# Ordering so that the line we get is not zig-zig all over the screen
ind <- order(test[,1])
test <- test[ind,]

# Initialising MSE to NULL
# MSE Will later be populated as a column vector containing MSE
# value for each 'k' nearest neighbour
MSE <- NULL

# Trying out different neighbourhood size
kk <- c(2,10,50,100,150,200,250,300,400,505)

for(i in kk)
{
  # medv~lstat means predict medv using lstat
  near <- kknn(medv~lstat,train,test,k=i,kernel = "rectangular")
  # aux is the MSE for given 'i'
  aux <- mean((test[,2]-near$fitted)^2)
  
  # append aux to MSE
  MSE <- c(MSE,aux)
  
  # Plot lstat v/s medv
  # Add fitted line (K-nearest neightbour prediction for each point)
  plot(lstat,medv,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  
  cat ("Press [enter] to continue")
  line <- readline()
}

# Plot of RMSE v/s neighborhood size
plot(log(1/kk),sqrt(MSE),type="b",
     xlab="Complexity (log(1/k))",
     ylab="RMSE",
     main = "Plot of RMSE v/s Neighbourhood size(Model Complexity)",
     col="blue",lwd=2,cex.lab=1.2)
text(log(1/kk[1]),sqrt(MSE[1])+0.3,paste("k=",kk[1]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)

# Fitting Knn with K = 20 (arbitrary) 
near = kknn(medv~lstat,train,test,k=20,kernel = "rectangular")

# Showing neighborhoods of random points and highlighting
# their 20 closest neighbors
for(i in seq(1,505,by=100))
{
  ii = near$C[i,1:20]
  plot(lstat,medv,main=paste("k=",20),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  abline(v=test[i,1],col=2,lty=2)
  points(lstat[ii],medv[ii],pch=19,col="blue")
  
  cat ("Press [enter] to continue")
  line <- readline()
}

#----OUT-OF-SAMPLE Prediction----

# taking a sample of 70%
tr_size = floor(n*0.7)
tr <- sample(1:n, tr_size)

# Test train split
train <- data.frame(lstat,medv)[tr,]
test <- data.frame(lstat,medv)[-tr,]

# Initialising MSE Column Vec. to store MSE values
# of different neighborhood sizes
out_MSE <- NULL

# i = 1 is the point itself. Thus, useless
for(i in 2:tr_size)
{
  near = kknn(medv~lstat,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  out_MSE = c(out_MSE,aux)
}

# min(out_MSE) -> Minimum MSE Value
# which.min(out_MSE) -> Index of Minimum MSE Value
best = which.min(out_MSE)

# Plotting Out-of-Sample RMSE v/s Model Complexity
plot(log(1/(2:tr_size)),sqrt(out_MSE),
     xlab="Complexity (log(1/k))",
     ylab="Cross Validation RMSE",
     main = "Model Performance",
     col=4,lwd=2,type="l",cex.lab=1.2)

text(log(1/best),sqrt(out_MSE[best])+0.3,paste("k=",best+1),col=2,cex=1.2)
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)

# Plotting Prediction line for k = 37 neighbours
near = kknn(medv~lstat,train,test,k=best+1,kernel = "rectangular")
ind = order(test[,1])
plot(lstat,medv,main=paste("k=",best+1),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

#----leave-one-out cross validation (LOOCV)----

# Matrix of n*100
# n training model b/c Leaving one out every time
out_MSE <- matrix(0,n,100)

for(j in 1:n)
{
  # Leaving one record out and fitting the model
  train_i <- data.frame(lstat,medv)[-j,]
  test_i <- data.frame(lstat,medv)[j,]
  
  # i = 1 is the point itself. Thus, useless
  # Iterating over 1 so that we dont have to 'best+1' like in OOS-CV
  for(i in 1:100)
  {
    # medv~lstat: medv is Y, and parameter list is lstat
    near <- kknn(medv~lstat,train_i,test_i,k=i,kernel = "rectangular")
    aux <- mean((test_i[,2]-near$fitted)^2)
    
    out_MSE[j,i] <- aux
  }
  cat(j,'\n') # Print command
}

mMSE <- apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),
     xlab="Complexity (log(1/k))",
     ylab="Cross Validation RMSE",
     main = "LOOCV",
     col=4,lwd=2,type="l",cex.lab=1.2)

best = which.min(mMSE)

text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)

#----K-fold Cross-Validation----

# Arbitary 10-folds
kcv = 10

# Divide into groups of size n0
n0 = ceiling(n/kcv)

out_MSE = matrix(0,kcv,100)


used = NULL
set = 1:n

for(j in 1:kcv)
{
  if(n0<length(set)){
    val = sample(set,n0)
  }
  else{
    val=set
  }

  train_i = data.frame(lstat,medv)[-val,]
  test_i = data.frame(lstat,medv)[val,]

  for(i in 1:100)
  {
    near = kknn(medv~lstat,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,2]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}

mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2,main=paste("kfold(",kcv,")"))
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)


#----California knn----

library(maps)


ca <- read.csv("datasets/CAhousing.csv")
logMedVal <- log(ca$medianHouseValue)

n=dim(ca)[1]

ind = sample(1:n,1000)

Y = logMedVal[ind]
CAdata = ca[ind,]


train = data.frame(Y,CAdata$latitude,CAdata$longitude)
test = data.frame(Y,CAdata$latitude,CAdata$longitude)


near = kknn(Y~.,train,test,k=10,kernel = "rectangular")


res = Y - near$fitted
nclr = 10
plotclr = colorRampPalette(c("cyan","magenta"))(nclr)
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
predbreaks = seq(min(Y),max(Y),length=nclr)
residbreaks = seq(min(res),max(res),length=nclr) # borders of resid color bins
residmap <- function(e){
  return(plotclr[cut(drop(e), residbreaks)]) ## cut sorts into bins
}
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}


par(mfrow=c(1,2))
## preds
map('state', 'california')
mtext("fitted values (k=10)",cex=2) 
points(test[,3:2], col=predmap(near$fitted), pch=19, cex=1)
map('state', 'california')
mtext("Residuals (k=10)",cex=2) 
points(test[,3:2], col=residmap(res), pch=19, cex=1)

n = dim(CAdata)[1]

##----K-Fold CV on Cali Housing----

kcv = 10
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv)
{
  if(n0<length(set))
  {
    val = sample(set,n0)
  }
  
  if(n0>=length(set))
  {
    val=set
  }
  
  train_i = data.frame(Y,CAdata$latitude,CAdata$longitude)[-val,]
  test_i = data.frame(Y,CAdata$latitude,CAdata$longitude)[val,]
  
  for(i in 1:100)
  {
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}


mMSE <- apply(out_MSE,2,mean)
par(mfrow=c(1,1))

plot(log(1/(1:100)),sqrt(mMSE),
     type="l",
     col=4,lwd=2,
     ylab="out-of-sample RMSE",
     xlab="Complexity",
     main="California Housing (knn)")

best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")


Income = scale(CAdata$medianIncome)*sd(CAdata$latitude)

train = data.frame(Y,CAdata$latitude,CAdata$longitude,Income)
test = data.frame(Y,CAdata$latitude,CAdata$longitude,Income)


near = kknn(Y~.,train,test,k=best,kernel = "rectangular")


res = Y - near$fitted
nclr = 10
plotclr = colorRampPalette(c("cyan","magenta"))(nclr)
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
predbreaks = seq(min(Y),max(Y),length=nclr)
residbreaks = seq(min(res),max(res),length=nclr) # borders of resid color bins
residmap <- function(e){
  return(plotclr[cut(drop(e), residbreaks)]) ## cut sorts into bins
}
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}


par(mfrow=c(1,2))

## preds
map('state', 'california')
mtext("fitted values (k=9)",cex=2) 
points(test[,3:2], col=predmap(near$fitted), pch=19, cex=1)
map('state', 'california')
mtext("Residuals (k=9)",cex=2) 
points(test[,3:2], col=residmap(res), pch=19, cex=1)
