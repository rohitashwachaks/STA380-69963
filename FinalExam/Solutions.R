set.seed(0)

#----Question 2----

rm(list = ls())
library(MASS)
attach(Boston)
?Boston

View(Boston)

  ## 2. a----

# How many rows, columns are there in the data?
cat("Rows: ", dim(Boston)[1], ", Columns: ", dim(Boston)[2])

# What do the Rows and Columns represent
colnames(Boston)

  ## 2.b----
pairs(Boston[,c("age","crim","lstat","rm","medv")])
# lstat is negatively corelated with medv
# very low medv usually has higher crime rate.
# But largely, crime rate is independent of locality/medv
# lstat independent of crime rate
# rm (#Rooms) +vely corelated to medv

  ## 2.c----
pairs(Boston)

  ## 2.e----
sum(Boston$chas == 1) # 35 suburbs are bound by the charles river

  ## 2.f----
median(Boston$ptratio) # Median -  19.05

#----Question 3.15----

rm(list = ls())
library(MASS)
library(corrplot)
attach(Boston)
?Boston

#View(Boston)
dim(Boston)

## 3.15.a----

mat <- matrix(0,length(colnames(Boston[,-1])),3)
i <- 0

significant <- NULL

for(cols in colnames(Boston[,-1]))
{
  i <- i+1
  cor(crim,Boston[cols])
  cat("\n")
  mdl <- lm(paste("crim~",cols), data = Boston)
  
  smry <- summary(mdl)
  coeff <- as.numeric(smry$coefficients[2,]["Estimate"])
  pval <- as.numeric(smry$coefficients[2,]["Pr(>|t|)"])
  rsqval <- as.numeric(smry$r.squared)
  
  plot(unlist(Boston[cols]),crim,pch=19,cex=1.5,xlab="Crime Rate",ylab = cols)
  abline(smry$coefficients[1,]["Estimate"],
         smry$coefficients[2,]["Estimate"],col=2,lwd=2)
  
  mat[i,1] <- coeff
  mat[i,2] <- pval
  mat[i,3] <- rsqval
  
  cat("For Predictor :",paste(cols),
      "\nCo-efficient:",paste(coeff),
      "\nP-value     :",paste(pval),
      "\nR^2         :",paste(rsqval),
      "\n")
  if(pval <= 0.05)
  {
    significant <- c(significant,cols)
  }
}

cat("\nStatistically Significant Variable are:\n",
    paste(significant),
    "\nbecause the P value for these are less than 0.05")

## 3.15.b----

mdl <- lm(crim~., data=Boston)
smry <- summary(mdl)

# Rejecting Coefficient = 0 hypothesis if p-value < 0.05
# This means Parameter is Significant
coeffs <- smry$coefficients[-1,4][smry$coefficients[-1,4] <0.05]

cat("Failing to reject NULL Hypothesis for: ")
for(x in colnames(t(coeffs))){
  cat(paste(x),"(",paste(coeffs[x]),")\n")
}
cat("Because Their P-Value is < 0.05")

## 3.15.c----

plot(x = mat[,1], xlab = "Simple Linear Regression Coefficients",
     y = smry$coefficients[-1,1], ylab = "Multiple Linear Regression Coefficients",
     main = "Parameter Coefficents Simple v/s Multiple"
)

## 3.15.d----

mat <- matrix(0,length(colnames(Boston[,-1])),3)
mat <- as.data.frame(mat)
colnames(mat) <- c("^1","^2","^3")
row.names(mat) <- colnames(Boston[,-1])
i <- 0

significant <- NULL

for(cols in colnames(Boston[,-1]))
{
  i <- i+1
  cor(crim,Boston[cols])
  # cat("\n")
  mdl <- lm(paste("crim~",cols,"+I(",cols,"^2)+I(",cols,"^3)"), data = Boston)
  
  smry <- summary(mdl)
  for(j in 1:3){
    mat[i,] <- t(smry$coefficients[-1,4])
  }
  # plot(unlist(Boston[cols]),crim,pch=19,cex=1.5,xlab="Crime Rate",ylab = cols)
  # lines(sort(crim), fitted(mdl)[order(crim)], col='red', type='l') 
}

mat


#----Question 4.10----

rm(list = ls())
library(ISLR)
attach(Weekly)
?Weekly
View(Weekly)

  ## 4.10.a----
summary(Weekly)
pairs(Weekly[,-9])
plot(Volume)
plot(Today, type = "l")

# Year and Volume is Positively Correlated

  ## 4.10.b----



#----Question 6.9----

# Check this link out:
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r/

rm(list = ls())
library(MASS)
library(corrplot)
library(ISLR)
# install.packages("leaps")
library(leaps)
attach(College)
?College
# To Predict: #Apps received

#View(College)
dim(College)
Private <- as.factor(Private)
#Scaling the variables
College[c(-1,-2)] <- as.data.frame(scale(College[c(-1,-2)]))

  ## 6.9.a----

n <- dim(College)[1]
tr <- sample(c(1:n), round(0.7*n, digits = 0))
train <- College[tr,]
test <- College[-tr,]

  ## 6.9.b----

# Selecting best Regression Subset (of 7 params)
regmodels <- regsubsets(Apps~.,data=College, nvmax =7)
x <- summary(regmodels)
x$outmat

# Summary Indicates, best 7 parameter model is
# Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+Expend

# How to select best model? In this case error using 7 params almost eq. error using ALL params
mdl <- lm(Apps~Accept+Enroll+Top10perc+Top25perc+Outstate+Room.Board+Expend, data=College)
smry <- summary(mdl)

preds <- predict(mdl,newdata = test)
test_error <- mean((preds-test$Apps)^2)
test_error

  ## 6.9.c----


