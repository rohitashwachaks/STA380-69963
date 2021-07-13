## Clear Environment
rm(list = ls())

## Assign and print value
res <- sum(1,2)
print(res)

#--------Arrays----------------
arr_1 <- c(1,2,3)       # c: column vectors
arr_2 <- 3:7            # how to skip every second element?
print(arr_1)            # print(arr_1,arr_2) does not print both


# Array addition needs to be of same size
# In R, Everything is in terms of Column Vectors


#--------Matrices----------------

numbers <- c(1,2,3,4,5,6)
mat_1 <- matrix(numbers, nrow = 2, ncol = 3)
mat_1
dim(mat_1)

mat_1*2         # Element-wise Multiply whole matrix by 2
mat_1**2        # Element-wise Exponent whole matrix by 2
t(mat_1)        # Transposing matrix
dim(t(mat_1))

mat_1%*%t(mat_1)  # Multiplying Matrices

mat_1[1:9]      # Lists the elements of a matrix as a column vec

mat_1[,1:2]     # All rows and first 2 cols
mat_1[,-2]      # All rows and cols EXCEPT 2nd col

#--------Data Frame----------------

data("mtcars")
head(mtcars)      # head() is for Head, tail() is for Tail
dim(mtcars)

#------Data Analysis---------

# Adds caption to the plot:
legend("topleft", # Plot's position (top-left)
       c("4", "6", "8"), # Variable names
       pch = c(19, 18, 17), # Point's formats
       col = c("red", "blue", "purple") # Point's colors
)

# Use plot() for a new plot
# Use points() for adding to existing Plot
# 

plot(x = mtcars$wt[mtcars$cyl == 4],
     y = mtcars$disp[mtcars$cyl == 4],
     xlab = "Weight (1000 lbs)",
     ylab = "Displacement (cu.in.)",
     pch = 19, # Format: circles
     col = "red", # Color: red
     xlim = range(mtcars$wt), #limits for the x axis
     ylim = range(mtcars$disp) #limits for the y axis
)

# Adds a OLS line for cyl = 4:
abline(lm(mtcars$disp[mtcars$cyl == 4] ~ mtcars$wt[mtcars$cyl == 4]),
       col = "red"
)

# Adds points for 6 cylinders:
points(x = mtcars$wt[mtcars$cyl == 6],
       y = mtcars$disp[mtcars$cyl == 6],
       pch = 18, # Format: diamond
       col = "blue" # Color: blue
)

# Adds a OLS line for cyl = 6:
abline(lm(mtcars$disp[mtcars$cyl == 6] ~ mtcars$wt[mtcars$cyl == 6]),
       col = "blue"
)

# Adds points for 8 cylinders:
points(x = mtcars$wt[mtcars$cyl == 8],
       y = mtcars$disp[mtcars$cyl == 8],
       pch = 17, # Format: triangles
       col = "purple" # Color: blue
)

# Adds a OLS line for cyl = 8:
abline(lm(mtcars$disp[mtcars$cyl == 8] ~ mtcars$wt[mtcars$cyl == 8]),
       col = "purple"
)