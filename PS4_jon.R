install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#Create new variables 'professsional' 
library(dplyr)
Prestige$professional <- recode(Prestige$type, prof = "1", bc = "0", wc = "0")

#lm with prestige as outcome and income, professional and interaction as predictors 
interact_reg <- lm(data = Prestige, prestige ~ income + professional + income*professional)
summary(interact_reg)
coef(interact_reg)

#t test 1
p1 <- 2*pt(2.625, 128, lower.tail = F)
p1

#t test 2 
p2 <- 2*pt(3.23076923077, 128, lower.tail = F)
p2
