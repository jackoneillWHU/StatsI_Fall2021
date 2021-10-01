#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stringr"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")


#####################
# Problem 1
#####################

#Create Object Y 
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Rename Y to student_IQ
student_IQ <- y 

#Finf mean of Student IQs
mean_student_IQ <- mean(student_IQ)
print(mean_student_IQ)

#Set up Confidence Interval 
z90 <- qnorm((1 - .90)/2, lower.tail = FALSE)
n <- length(na.omit(student_IQ))
mean_student_IQ <- mean(student_IQ, na.rm = TRUE)
SD_student_IQ <- sd(student_IQ, na.rm = TRUE)
lower_90 <- mean_student_IQ - (z90 * (SD_student_IQ/sqrt(n)))
upper_90 <- mean_student_IQ + (z90 * (SD_student_IQ/sqrt(n)))

confint90 <- c(lower_90, upper_90)

print(confint90)


#Find our variables
str(student_IQ)

#Formulate our null hypothesis - NULL HYPOTHESIS: The average student IQ is less than
#or equal to 100 

IQ_null <- t.test(student_IQ, mu = 100)
IQ_null

#####################
# Problem 2
#####################


#Clear our environment
rm(list = ls())

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

library(ggplot2)

#Plotting the relationships between Y, X1, X2, and X3 

ggplot(expenditure, 
       aes(x = X1, 
           y = Y)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) 


ggplot(expenditure, 
       aes(x = X2, 
           y = Y)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) 

ggplot(expenditure, 
       aes(x = X3, 
           y = Y)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) 

ggplot(expenditure, 
       aes(x = X2, 
           y = X1)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) 

ggplot(expenditure, 
       aes(x = X3, 
           y = X1)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) 

ggplot(expenditure, 
       aes(x = X2, 
           y = X3)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) 


#Plot the relationship between Y and Region 
#The plot indicates that region 4 clearly has the highest per capita spending
#on housing assistance
ggplot(expenditure, 
       aes(x = Region, 
           y = Y)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
  ) +
  labs(x = "Region",
       y = "per capita expenditure on shelters/housing assistance in state",
       title = "Which region has spends most on housing assistance?",)

print(plotY_Region)

#Plot the relationship between Y and X1 
ggplot(expenditure, 
       aes(x = X1, 
           y = Y)) +
  geom_point(color="cornflowerblue", 
             size = 2, 
             alpha=.8) +
  scale_y_continuous(label = scales::dollar, 
  ) +
  scale_x_continuous(label = scales::dollar, 
  ) +
  labs(x = "per capita personal income in state",
       y = "per capita expenditure on shelters/housing assistance in state",
       title = "Personal income vs ependitur on shelters",)

#add one more variable, Region 

ggplot() + geom_point(data = expenditure,
                      aes(x = X1, y = Y, color = Region))

