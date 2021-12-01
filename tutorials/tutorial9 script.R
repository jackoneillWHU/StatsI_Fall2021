# Packages
library(tidyverse)
library(stargazer)

#### Recap on categorical variables using lm()

# Load in salary dataset
salary <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/salary.csv")

# A basic bivariate regression of salary on grants
salary_reg1 <- lm(Salary_9_mo~Avg_Cont_Grants, data = salary)

# Hand-coding a dummy variable
salary$rank_d1 <- rep(0, nrow(salary))
salary$rank_d2 <- rep(0, nrow(salary))
salary[which(salary$Rank_Code == 3), "rank_d1"] <- as.factor("1")
salary[which(salary$Rank_Code == 2), "rank_d2"] <- as.factor("1")

salary_reg2 <- lm(Salary_9_mo~Avg_Cont_Grants + rank_d1 + rank_d2, data = salary)

# Don't do this!
salary_reg3 <- lm(Salary_9_mo~Avg_Cont_Grants + Rank_Code, data = salary)

# What's wrong with model 3?
stargazer(salary_reg1, salary_reg2, salary_reg3, type = "text")

# read.csv and read_csv will cast factors as integers
class(salary$Rank_Code)

# lm() will treat Rank_Code as a continous variable!
plot(salary$Avg_Cont_Grants, salary$Salary_9_mo, type = "p", pch = 20, col = c("red", "blue", "green")[salary$Rank_Code])
abline(salary_reg3$coefficients[1], salary_reg3$coefficients[2], col = "red")
abline(salary_reg3$coefficients[1] + salary_reg3$coefficients[3], salary_reg3$coefficients[2], col = "blue")
abline(salary_reg3$coefficients[1] + salary_reg3$coefficients[3]*2, salary_reg3$coefficients[2], col = "green")

# Cast Rank_Code as a factor first
salary_reg4 <- lm(Salary_9_mo~Avg_Cont_Grants + as.factor(Rank_Code), data = salary)

# The result is now identical with hand-coding as a dummy
stargazer(salary_reg2, salary_reg4, type = "text")

# We can see that we have a proper plot now
plot(salary$Avg_Cont_Grants, salary$Salary_9_mo, 
     type = "p", pch = 20, col = c("red", "blue", "green")[salary$Rank_Code])
abline(salary_reg4$coefficients[1], salary_reg4$coefficients[2], col = "red")
abline(salary_reg4$coefficients[1] + salary_reg4$coefficients[3], salary_reg3$coefficients[2], col = "blue")
abline(salary_reg4$coefficients[1] + salary_reg4$coefficients[4], salary_reg3$coefficients[2], col = "green")

# Note: this is only really relevent when you have categories > 2, as a simple binary 
# variable (i.e. male/female) only has two levels, and therefore only one intercept 
# that differs. Note also that ggplot's geom_smooth lm method knows to cast an integer 
# as a factor when you supply the variable as an argument to colour or group, etc.

#### Mini project part 2

# Load in data
houses <- read.csv("https://raw.githubusercontent.com/gedeck/practical-statistics-for-data-scientists/master/data/house_sales.csv", sep = "\t")

# Last week's model
hmod1 <- lm(AdjSalePrice ~ SqFtLot + BldgGrade, houses)
summary(hmod1)

# Where possible, character vectors are automatically cast as factors by lm()
class(houses$PropertyType)
typeof(houses$PropertyType)

# Group by zipcode frequency
houses %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Plot zipcode frequency
houses %>%
  group_by(ZipCode) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, n)), n)) +
    geom_col() +
    coord_flip() +
    xlab("Zip Code")

# Group by zipcode mean sale price
houses %>%
  group_by(ZipCode) %>%
  summarise(n = n(),
            mean = mean(AdjSalePrice)) %>%
  arrange(desc(mean))

# Remove scientific notation from output
options(scipen = 999)

# Plot (absolute) residuals of initial model, hmod1
plot(predict(hmod1), abs(resid(hmod1)), xlab = "Predicted", ylab = "Absolute Residuals")

# Rather than clustering according to sale price, why not try clustering according 
# to residuals (i.e., where our current model is weak at predicting, we see if 
# zipcode adds anything).

# Tweaking our initial dplyr code to get the median of the residuals
houses %>%
  mutate(resid = resid(hmod1)) %>%
  group_by(ZipCode) %>%
  summarise(median = median(resid),
            n = n()) %>%
  arrange(desc(median)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, median)), median)) +
    geom_col() +
    coord_flip() +
    ylab("Median residual") +
    xlab("Zipcode")

# Is mean better?
houses %>%
  mutate(resid = resid(hmod1)) %>%
  group_by(ZipCode) %>%
  summarise(mean = mean(resid),
            n = n()) %>%
  arrange(desc(mean)) %>%
  ggplot(aes(as.factor(reorder(ZipCode, mean)), mean)) +
    geom_col() +
    coord_flip() +
    ylab("Mean residual") +
    xlab("Zipcode")

# How do we create a new set of groupings?
?ntile

# Use ntile on the cases sorted by median 
houses %>%
  mutate(resid = resid(hmod1)) %>%
  group_by(ZipCode) %>%
  summarise(median = median(resid),
            n = n()) %>%
  arrange(desc(median)) %>%
  mutate(ZipGroup = ntile(median, 5)) %>%
  group_by(ZipGroup) %>%
  summarise(n = n(),
            med = median(median))

# Another method (Bruce, Bruce & Gedeck)
houses %>%
  mutate(resid = resid(hmod1)) %>%
  group_by(ZipCode) %>%
  summarise(median = median(resid),
            n = n()) %>%
  arrange(desc(median)) %>%
  mutate(sum_n = cumsum(n),
         ZipGroup = ntile(sum_n, 5)) %>%
  group_by(ZipGroup) %>%
  summarise(n = n(),
            med = median(median))

# code for mutating - create a new dataframe "zip_groups"
zip_groups <- houses %>%
  mutate(resid = resid(hmod1)) %>%
  group_by(ZipCode) %>%
  summarise(median = median(resid),
            n = n()) %>%
  arrange(desc(median)) %>%
  mutate(ZipGroup = ntile(median, 5))

head(zip_groups)

# join zip_groups to houses using left_join()
?left_join
houses <- houses %>%
  left_join(select(zip_groups, ZipCode, ZipGroup), by = "ZipCode")

# running a regression using ZipGroup
hmod2 <- lm(AdjSalePrice ~ SqFtLot + BldgGrade + as.factor(ZipGroup), houses)
summary(hmod2)
summary(hmod1)

ggplot(houses, aes(SqFtLot, AdjSalePrice, colour = as.factor(ZipGroup))) +
  geom_point() +
  geom_smooth(method = "lm", aes(colour = as.factor(ZipGroup)))

# a solution with three groups could be more parsimonious
houses$ZipGroup3 <- recode(houses$ZipGroup, `2` = 1L, `3` = 2L, `4` = 2L, `5` = 3L)
hmod3 <- lm(AdjSalePrice ~ SqFtLot + BldgGrade + as.factor(ZipGroup3), houses)

summary(hmod3)
summary(hmod2)

ggplot(houses, aes(SqFtLot, AdjSalePrice, colour = as.factor(ZipGroup3))) +
  geom_point() +
  geom_smooth(method = "lm", aes(colour = as.factor(ZipGroup3)))

# let's plot our residuals again and see if our model improved
plot(predict(hmod3), abs(resid(hmod3)), xlab = "Predicted", ylab = "Absolute Residuals")
plot(predict(hmod1), abs(resid(hmod1)), xlab = "Predicted", ylab = "Absolute Residuals")
# seems a bit better

# how about our residual standard error?
stargazer(hmod1, hmod2, hmod3, type = "text")
# seems by adding a location predictor we reduced the RSE by about $25,000. Not bad!