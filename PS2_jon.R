
library(readr)
chitable2 <- read_csv("~/Desktop/Quants 1/chitable2.csv")
View(chitable2)

#Tidy up data frame 
chitable2 <- subset(chitable2, select = -c(...1) )
print(chitable2)

#run the Chi Square Test 
chitest <- chisq.test(chitable2)

#get standardized residuals 
chitest$stdres

#Creating p value for the test statistic 
p_value = pchisq(3.7912, df=3, lower.tail = F)
print(p_value)

# With lamba = to 0.1, when we reference the table we find a critical vale of 9.210. 
#As our p value is 0.2849114 (less than the critical value) we would fail to reject our null hypotheses that the variables are statistically indepenent 

#create standardized reziduals

x <- c(6, 7)
#get mean and sd 
c(round(mean(x), 2), round(sd(x), 2)
#standardized distance 
standardized.x <- (x - mean(x)/sd(x))
#view vector for standardized distance 
round(standardized.x, 2)

y <- c(14, 7)
#get mean and sd 
c(round(mean(y), 2), round(sd(y), 2)
  #standardized distance 
  standardized.y <- (y - mean(y)/sd(y))
  #view vector for standardized distance 
  round(standardized.y, 2)

z <- c(7,1)
#get mean and sd 
c(round(mean(z), 2), round(sd(z), 2)
  #standardized distance 
  standardized.z <- (z - mean(z)/sd(z))
  #view vector for standardized distance 
  round(standardized.z, 2)
  
  
#run bivariate regression; Water and Reserved variables 
 lm1 <- lm(women$water ~ women$reserved)
 
 summary(lm1)
  
  abline(lm(women$water ~ women$reserved), col = "red")
  
  plot(lm(women$water ~ women$reserved)
  abline(lm(women$water ~ women$reserved), col = "blue")
  
  ggplot(aes(water, reserved), data = women) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x)
  

  
  
  
  
  
#import the data set 
fruitfly <- read.csv("https://raw.githubusercontent.com/jackoneillWHU/StatsI_Fall2021/main/datasets/fruitfly.csv")
summary(fruitfly)

library(ggplot2)

plot(fruitfly$type, fruitfly$lifespan,
     main = "Scatter Plot Lifespan by Type",
     xlab = "Type",
     ylab = "Lifespan")

ggplot(fruitfly, aes(x=fruitfly$lifespan)) + 
  geom_density() + geom_vline(aes(xintercept=mean(lifespan)),
                                color="blue", linetype="dashed", size=1)
 
#plot lifespan vs thorax 
ggplot(fruitfly, aes(x=thorax, y= lifespan)) +geom_point()

#find correlation between the two 
cor(fruitfly$thorax, fruitfly$lifespan)

#regression; lifespan and thorax 
#run bivariate regression; Lifespan and thorax 
lmfly <- lm(fruitfly$lifespan ~ fruitfly$thorax)

summary(lmfly)
     ggplot(aes(thorax, lifespan), data = fruitfly) +
       geom_point() +
       geom_smooth(method = "lm", formula = y ~ x)
     

     
## produce predicted lifespan for thorax = 0.8mm
 newdata <- data.frame(thorax = 0.8)
 predict(lmfly, newdata)


 newdata <- data.frame(thorax = 0.8)
 predict(lmfly, newdata, interval = "confidence", level = 0.97)
 
 
 plot(lifespan ~ thorax, data = fruitfly, pch = 19, col='darkgrey')
 
 
 newdata <- cbind(newdata, lifespan=predict(lmfly, newdata)
                  plot(lifespan ~ thorax, data = fruitfly, pch = 19, col='darkgrey')
                  points(lifespan ~ thorax, data = newdata, pch = 19, col = "red")                  
       
                  
#90% ci 
      confint(lmfly, level=0.90)
      
      par(mfrow=c(2, 2))
      plot(lmfly, pch=19, col='darkgrey')
      
      
      
      ## create new data frame to predict to
      newdata <- data.frame(
        thorax = seq(min(fruitfly$thorax), 
                     max(fruitfly$thorax), length.out = 50)
      )
      
      ## produce predictions and intervals
      newdata <- cbind(newdata, 
                       predict(lmfly, newdata, 
                               interval = "prediction", 
                               level = 0.90))
      newdata$lifespan <- newdata$lmfly
      newdata$lmfly <- NULL
      
      ## plot fitted line against the raw data
      plot(lifespan ~ thorax, data = fruitfly, 
           pch = 19, col='darkgrey',
           main = "Fitted regression line 
     with 90% prediction interval")
      lines(lifespan ~ thorax, data = newdata)
      lines(lwr ~ thorax, data = newdata, 
            lty = 2)
      lines(upr ~ thorax, data = newdata, 
            lty = 2)    
      