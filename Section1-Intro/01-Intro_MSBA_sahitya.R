library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn) ## knn library


attach(Boston) ## attach the database to include in variable search path
n = dim(Boston)[1] ## number of data points
 
plot(lstat,medv) ## plot lstat vs medv

train = data.frame(lstat,medv) ##Assign train and test datasets
test = data.frame(lstat,medv)
ind = order(test[,1]) ## create a permutation of indices and select the indice values for test dataset
test = test[ind,]

MSE = NULL

kk = c(2,10,50,100,150,200,250,300,400,505)

for(i in kk){

near = kknn(medv~lstat,train,test,k=i,kernel = "rectangular") ## medv is y and lstat is x
aux = mean((test[,2]-near$fitted)^2) ## calculate the RMSE value for the given k

MSE = c(MSE,aux) ## Add it to the MSE array

plot(lstat,medv,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
lines(test[,1],near$fitted,col=2,lwd=2)
cat ("Press [enter] to continue")
line <- readline()
}


plot(log(1/kk),sqrt(MSE),type="b",xlab="Complexity (log(1/k))",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
text(log(1/kk[1]),sqrt(MSE[1])+0.3,paste("k=",kk[1]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)

near = kknn(medv~lstat,train,test,k=20,kernel = "rectangular")

for(i in seq(1,505,by=100)){
ii = near$C[i,1:20]
plot(lstat,medv,main=paste("k=",20),pch=19,cex=0.8,col="darkgray")
lines(test[,1],near$fitted,col=2,lwd=2)
abline(v=test[i,1],col=2,lty=2)
points(lstat[ii],medv[ii],pch=19,col="blue")
cat ("Press [enter] to continue")
line <- readline()
}

######################################
## OUT-OF-SAMPLE Prediction
######################################

train = data.frame(lstat,medv)
test = data.frame(lstat,medv)

tr = sample(1:506,400)

train = train[tr,]
test = test[-tr,]

out_MSE = NULL

for(i in 2:350){

near = kknn(medv~lstat,train,test,k=i,kernel = "rectangular")
aux = mean((test[,2]-near$fitted)^2)

out_MSE = c(out_MSE,aux)
}


best = which.min(out_MSE)

plot(log(1/(2:350)),sqrt(out_MSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
text(log(1/best),sqrt(out_MSE[best])+0.3,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)


near = kknn(medv~lstat,train,test,k=42,kernel = "rectangular")

ind = order(test[,1])
plot(lstat,medv,main=paste("k=",42),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

#########################################
# leave-one-out cross validation (LOOCV)#
#########################################

train = data.frame(lstat,medv)
test = data.frame(lstat,medv)


out_MSE = matrix(0,n,100) ## n different iterations

for(j in 1:n){


train_i = train[-j,] ## Leave one observation out
test_i = test[j,] ## The left out observation is the test set

for(i in 1:100){ ## Run knn with different values of k = 1-100

near = kknn(medv~lstat,train_i,test_i,k=i,kernel = "rectangular") ## y=medv; x=lstat
aux = mean((test_i[,2]-near$fitted)^2)

out_MSE[j,i] = aux
}
cat(j,'\n')
}

mMSE = apply(out_MSE,2,mean) ## Take an average out_MSE-506*100, apply to the columns(2) - 100 values

## k=40 is the ideal value of k
plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)




#################################
# k-fold cross validation       #
#################################
kcv = 10  ## 10 folds , so 10 iterations
n0 = round(n/kcv,0) ## round it up. Size of 101

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){

if(n0<length(set)){val = sample(set,n0)} ## If you have 101 observations left from set, pick out n0 random values from set
if(n0>=length(set)){val=set} ## If you have less than 101, include the entire set as the val 

train_i = train[-val,] ## Set the training set as everything except the ones in val
test_i = test[val,] ## Set the ones left out as the test set

for(i in 1:100){ ## Run the knn algorithm for different k values from 1-100

near = kknn(medv~lstat,train_i,test_i,k=i,kernel = "rectangular")
aux = mean((test_i[,2]-near$fitted)^2) ## MSE test_i[,2] is the second column that is medv

out_MSE[j,i] = aux ## calculate MSE
}

used = union(used,val) ## Keep track of all the values that have been used already as a validation set
set = (1:n)[-used] ## set is now the values that have not been used for validation yet

cat(j,'\n')

}

mMSE = apply(out_MSE,2,mean) ## Again, calculate mean that will average out the columns - 1*100

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2,main=paste("kfold(",kcv,")"))
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)





##########################
#### California knn      #
##########################

library(maps)


ca <- read.csv("CAhousing.csv")
logMedVal <- log(ca$medianHouseValue) ## Take log of median house value to un stretch the varied values since they are in dollars

#hist()

n=dim(ca)[1] #20640 samples

ind = sample(1:n,1000) ## consider indices of only 1000 samples from the dataset

Y = logMedVal[ind]  ## Y variable modified for only the 1000 samples considered
CAdata = ca[ind,] ## X variable modified for only the 1000 samples considered


train = data.frame(Y,CAdata$latitude,CAdata$longitude) ## Consider only Y as a function of lat and long and split into train and test
test = data.frame(Y,CAdata$latitude,CAdata$longitude)


near = kknn(Y~.,train,test,k=10,kernel = "rectangular") ## Run knn for Y as a function of all other variables 'rectangular kernel' is basically euclidean distance calculation


res = Y - near$fitted ## Calculate the residual that is just the difference b/w actual and fitted Y
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

kcv = 10 ## 10 folds
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){

if(n0<length(set)){val = sample(set,n0)}
if(n0>=length(set)){val=set}

train_i = train[-val,]
test_i = test[val,]

for(i in 1:100){

near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
aux = mean((test_i[,1]-near$fitted)^2)

out_MSE[j,i] = aux
}

used = union(used,val)
set = (1:n)[-used]

cat(j,'\n')
}



## k = 6; mMSE = 0.1120724

mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="California Housing (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")



## Income as another feature that is used to predict median house value 
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



n = dim(CAdata)[1]

kcv = 10 ## 10 folds
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}



## k = 11; mMSE = 0.09925854

mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="California Housing (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")


## Income, rooms as another feature that is used to predict median house value 
Income = scale(CAdata$medianIncome)*sd(CAdata$latitude)
Rooms = scale(CAdata$totalRooms)*sd(CAdata$latitude)

train = data.frame(Y,CAdata$latitude,CAdata$longitude,Income,Rooms)
test = data.frame(Y,CAdata$latitude,CAdata$longitude,Income,Rooms)


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



n = dim(CAdata)[1]

kcv = 10 ## 10 folds
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}



## k = 9; mMSE = 0.1089018

mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="California Housing (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")





## Income, rooms, bedrooms as another feature that is used to predict median house value 
Income = scale(CAdata$medianIncome)*sd(CAdata$latitude)
Rooms = scale(CAdata$totalRooms)*sd(CAdata$latitude)
Bedrooms= scale(CAdata$totalBedrooms)*sd(CAdata$latitude)
  
train = data.frame(Y,CAdata$latitude,CAdata$longitude,Income,Rooms,Bedrooms)
test = data.frame(Y,CAdata$latitude,CAdata$longitude,Income,Rooms,Bedrooms)


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



n = dim(CAdata)[1]

kcv = 10 ## 10 folds
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}



## k = 7; mMSE = 0.1101364

mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="California Housing (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")


