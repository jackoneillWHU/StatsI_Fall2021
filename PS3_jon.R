#Load the data set incumbents 
incumbents <- read_csv("https://raw.githubusercontent.com/jackoneillWHU/StatsI_Fall2021/main/datasets/incumbents_subset.csv")

#run regression with voteshare as outcome variable and difflog as explanatory variable
summary(lm( data = incumbents, voteshare ~ difflog))
lm1 <- lm( data = incumbents, voteshare ~ difflog)


#create a scatterplot with regression line 
ggplot(data = incumbents, aes(x = difflog, y = voteshare)) +
  geom_point(alpha = 0.5) + #add a scatterplot
  geom_smooth(method = "lm") #add a linear regression line

#save residuals as new object 
residuals(lm1)
incumbents$residuals <- residuals(lm1) 

#run regression with presvote as outcome variable and difflog as explanatory variable
summary(lm( data = incumbents, presvote ~ difflog))
lm2 <- lm( data = incumbents, presvote ~ difflog)

#create a scatterplot with regression line 
ggplot(data = incumbents, aes(x = difflog, y = presvote)) +
  geom_point(alpha = 0.5) + #add a scatterplot
  geom_smooth(method = "lm") #add a linear regression line

#save residuals as new object 
residuals(lm2)
incumbents$residuals2 <- residuals(lm2) 

####Question 3 
#run regression with voteshare as outcome variable and presvote as explanatory variable
summary(lm( data = incumbents, voteshare ~ presvote))
lm3 <- lm( data = incumbents, voteshare ~ presvote)

#create a scatterplot with regression line 
ggplot(data = incumbents, aes(x = presvote, y = voteshare)) +
  geom_point(alpha = 0.5) + #add a scatterplot
  geom_smooth(method = "lm") #add a linear regression line


####Question 4 
#run regression with incumbents$residuals as outcome variable and incumbents$residuals2 as the explanatory variable 
summary(lm(data=incumbents, incumbents$residuals ~incumbents$residuals2))


#create a scatterplot with regression line 
ggplot(data = incumbents, aes(x = incumbents$residuals2, y = incumbents$residuals)) +
  geom_point(alpha = 0.5) + #add a scatterplot
  geom_smooth(method = "lm") #add a linear regression line

####Question 5

#run regression with voteshare as outcome variable and difflog and  presvote as the explanatory variables 
summary(lm(data = incumbents, voteshare ~ difflog + presvote))

