#Question 2----

  ##2. a----
rm(list = ls())
library(MASS)
attach(Boston)
?Boston

View(Boston)

# How many rows, columns are there in the data?
cat("Rows: ", dim(Boston)[1], ", Columns: ", dim(Boston)[2])

# What do the Rows and Columns represent
colnames(Boston)

  ##2.b----
pairs(Boston[,c("age","crim","lstat","rm","medv")])
# lstat is negatively corelated with medv
# very low medv usually has higher crime rate.
# But largely, crime rate is independent of locality/medv
# lstat independent of crime rate
# rm (#Rooms) +vely corelated to medv

  ##2.c----
pairs(Boston)

  ##2.e----
sum(Boston$chas == 1) # 35 suburbs are bound by the charles river

  ##2.f----
median(Boston$ptratio) # Median -  19.05

  ##2.g----

