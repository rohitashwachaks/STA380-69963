#----Question 2----

rm(list = ls())
library(MASS)
attach(Boston)
?Boston

View(Boston)

  ## 2. a

# How many rows, columns are there in the data?
cat("Rows: ", dim(Boston)[1], ", Columns: ", dim(Boston)[2])

# What do the Rows and Columns represent
colnames(Boston)

  ## 2.b
pairs(Boston[,c("age","crim","lstat","rm","medv")])
# lstat is negatively corelated with medv
# very low medv usually has higher crime rate.
# But largely, crime rate is independent of locality/medv
# lstat independent of crime rate
# rm (#Rooms) +vely corelated to medv

  ## 2.c
pairs(Boston)

  ## 2.e
sum(Boston$chas == 1) # 35 suburbs are bound by the charles river

  ## 2.f
median(Boston$ptratio) # Median -  19.05

  ## 2.g

#----Question 3.15----

rm(list = ls())
library(MASS)
library(corrplot)
attach(Boston)
?Boston

View(Boston)

  ## a
dim(Boston)

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

# mat <- as.data.frame(mat,row.names = colnames(Boston[,-1]))
# colnames(mat) <- c('P Value','Coefficient','R-Squared')


  ## b


  ## c

