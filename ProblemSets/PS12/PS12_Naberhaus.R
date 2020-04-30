library(sampleSelection)
library(tidyverse)
library(stargazer)
library(RCurl)
library(magrittr)
library(mlogit)

x <- getURL("https://raw.githubusercontent.com/tyleransom/DScourseS20/master/Structural/wages12.csv")
Wages <- read.csv(text = x)
view(Wages)

Wages$college <- as.factor(Wages$college)
Wages$married <- as.factor(Wages$married)
Wages$union <- as.factor(Wages$union)

stargazer(Wages)
Wages %<>% mutate(expersq = exper^2)

#Only Complete Cases
LinearModel <- lm(logwage ~ hgc + union + college + exper + expersq ,data=Wages)
summary(LinearModel)

#Mean Imputation
Wages2 <- Wages
Wages2$logwage[is.na(Wages2$logwage)] <- (mean(Wages2$logwage, na.rm = TRUE))
LinearModel2 <- lm(logwage ~ hgc + union + college + exper + expersq ,data=Wages2)
summary(LinearModel2)

#Heckman
Wages3 <- Wages
Wages3 %<>% mutate(Validity = ifelse(Wages3$logwage == 'NA', "Invalid", "Valid"))
Wages3$Validity[is.na(Wages3$Validity)] <- "Invalid"
Wages3$logwage[is.na(Wages3$logwage)] <- 0
HeckitModel <- selection(selection = Validity ~ hgc + union + college + exper + married + kids ,
                    outcome = logwage ~ hgc + union + college + exper + I(expersq) ,
                    data = Wages3 , method = "2step")

#Stargazing
stargazer(LinearModel, LinearModel2, HeckitModel)

#Probit
Wages4 <- Wages
UtilityModel <- glm(union ~ hgc + college + exper + married + kids, 
                    family = binomial(link = 'probit'), data = Wages4)
summary(UtilityModel)
#PredProb
Wages4$predProbit <- predict(UtilityModel, newdata = Wages4, type = "response")
print(summary(Wages4$predProbit))

#Lose the wife and kids
Wages5 <- Wages
Wages5$married <- as.numeric(Wages5$married)
UtilityModel2 <- glm(union ~ hgc + college + exper + married + kids, 
                     family = binomial(link = 'probit'), data = Wages5)

UtilityModel2$coefficients["married"] <- 0*UtilityModel2$coefficients["married"]
UtilityModel2$coefficients["kids"] <- 0*UtilityModel2$coefficients["kids"]
summary(UtilityModel2)
Wages5$predProbit <- predict(UtilityModel2, newdata = Wages5, type = "response")
print(summary(Wages5$predProbit))

#Stargazing again
stargazer(UtilityModel,UtilityModel2)
