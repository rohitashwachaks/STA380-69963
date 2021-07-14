#### Question-8: College Data-Set ####

## A: Read college.csv
        college <- read.csv("../datasets/college.csv")
        View(college)

## B: Edit Index
        # Replace Row index as college name and drop the latter
        rownames(college) <- college[,1]
        college <- college[,-1]

## No simpler command to replace these above lines??

        # fix() lets you edit Data-Set
        fix(college)
        
## C.1: Summary of Data-Set
        summary(college)
        dim(college)
        
        
## C.2: Scatter-Plot

        # Converting College data to Categorical Data
        college$Private <- factor(college$Private, c("Yes","No"), labels = c("Yes","No"))
        
        # Plot Scatter-plot of first 10 columns
        pairs(college[1:10])
        
# C.3: Plotting OutState v/s Private
        summary(college$Private)
        
        # plot() with x-axis as a Categorical Data plots a Box-Plots
        plot(college$Private,
             college$Outstate,
             xlab = "Private University",
             ylab = "Out of State tuition in USD",
             main = "Box-Plot of Outstate Tuition v/s University Funding")

# C.4: Elite University
        # Adding Column Elite
        Elite <- rep ("No",nrow(college))
        Elite[college$Top10perc >50]="Yes"
        Elite <- as.factor(Elite)
        college <- data.frame(college ,Elite)
        summary(Elite)
        
        # Plotting Outstate v/s Elite
        plot(college$Elite,
             college$Outstate,
             xlab = "Elite University",
             ylab = "Out of State tuition in USD",
             main = "Box-Plot of Outstate Tuition v/s University Type")

# C.5: Plotting Sub-Plots
        par(mfrow = c(2,2))
        hist(college$Books,xlab = "Books", ylab = "Count", breaks = 20)
        hist(college$PhD, xlab = "PhD", ylab = "Count", breaks = 40)
        hist(college$Grad.Rate, xlab = "Grad Rate", ylab = "Count", breaks = 30)
        hist(college$perc.alumni, xlab = "% alumni", ylab = "Count", breaks = 100)
        
par(mfrow = c(1,1))

# C.6. Acceptance Rate v/s %age of students in cohort from top 10 %ile in High school
plot(college$Accept/college$Apps
     ,college$Top10perc
     ,xlab = "Acceptance Rate"
     ,ylab = "%age Top students"
     ,main = "Acceptance Rate v/s Quality"
     ,pch = "*"
     ,col = ifelse(as.character(college$Elite) == "Yes", "red","green")
     ,legend = legend("topright", col=c("red","green"),c("Elite","Not Elite"))
     )

#### Question 9: Auto DataSet ####

# Read Auto Data
auto = read.csv("../datasets/Auto.csv")

# Assign Rownames
        # Duplicate entries for "plymouth reliant 81 - 4 cylinders"
rownames(auto) = paste(as.character(auto$car.name)
                       ,as.character(auto$model.year)
                       ,"-",as.character(auto$cylinders),"cylinders")
fix(auto)

# Summary
summary(auto)
