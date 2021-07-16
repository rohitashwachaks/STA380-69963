library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn) ## knn library

# Set Seed for replicability
set.seed(121)
# Adds Boston dataset to the Global Scope.(?)
attach(Boston)

n <- dim(Boston)[1]

# Plotting %age of pop. from Lower economic status v/s Median Property Value
plot(lstat,medv,
     xlab = "%age Pop. from Lower economic status",
     ylab = "Median Property Value",
     main = "Relationship b/w Property Value & Econ. Status of Pop.")

train <- data.frame(lstat,medv)
test <- data.frame(lstat,medv)

# Sorting test w.r.t Column 1
# Ordering so that the line we get is not zig-zig all over the screen
ind = order(test[,1])
test =test[ind,]

# Initialising MSE
MSE = NULL

# Trying out different neighbourhood size
kk = c(2,10,50,100,150,200,250,300,400,505)

for(i in kk)
{
  near = kknn(medv~lstat,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  MSE = c(MSE,aux)
  
  plot(lstat,medv,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  
  cat ("Press [enter] to continue")
  line <- readline()
}

# Plot
plot(log(1/kk),sqrt(MSE),type="b",xlab="Complexity (log(1/k))",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
text(log(1/kk[1]),sqrt(MSE[1])+0.3,paste("k=",kk[1]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)

near = kknn(medv~lstat,train,test,k=20,kernel = "rectangular")

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

#------OUT-OF-SAMPLE Prediction----------------

tr = sample(1:506,400)

train = data.frame(lstat,medv)[tr,]
test = data.frame(lstat,medv)[-tr,]

out_MSE = NULL

for(i in 2:350)
{
  near = kknn(medv~lstat,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  out_MSE = c(out_MSE,aux)
}

# min(out_MSE) -> Minimum MSE Value
# which.min(out_MSE) -> Index of Minimum MSE Value
best = which.min(out_MSE)

plot(log(1/(2:350)),sqrt(out_MSE),
     xlab="Complexity (log(1/k))",
     ylab="Cross Validation RMSE",
     main = "Model Performance",
     col=4,lwd=2,type="l",cex.lab=1.2)

text(log(1/best),sqrt(out_MSE[best])+0.3,paste("k=",best+1),col=2,cex=1.2)
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)


near = kknn(medv~lstat,train,test,k=best+1,kernel = "rectangular")

ind = order(test[,1])
plot(lstat,medv,main=paste("k=",best+1),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

#------leave-one-out cross validation (LOOCV)-------

# Adds Boston dataset to the Global Scope.(?)
#attach(Boston)
n <- dim(Boston)[1]

out_MSE <- matrix(0,n,100)

for(j in 1:n)
{
  train_i <- data.frame(lstat,medv)[-j,]
  test_i <- data.frame(lstat,medv)[j,]
  
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

#------K-fold Cross-Validation-------

kcv = 10

#attach(Boston)
n <- dim(Boston)[1]

# Divide into groups of size n0
n0 = ceiling(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv)
{
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}

  train_i = train[-val,]
  test_i = test[val,]

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


#------California knn-------

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
  
  train_i = train[-val,]
  test_i = test[val,]
  
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


mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="California Housing (knn)",xlab="Complexity")
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
