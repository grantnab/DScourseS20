library(mice)
library(tidyverse)
library(stargazer)
library(RCurl)
library(magrittr)
library(tidyr)
library(missForest)

x <- getURL("https://raw.githubusercontent.com/tyleransom/DScourseS20/master/ModelingOptimization/wages.csv")
Wages <- read.csv(text = x)

Wages1 <- Wages %>% drop_na(hgc,tenure)

stargazer(Wages1)

Wages1 %<>% mutate(tenuresq = tenure^2)

Model1 <- lm(logwage ~ hgc + college + tenure + tenuresq + age + married,data=Wages1)
summary(Model1)
#College=1
#NotCollege=0
#Married=1
#Single=0

Wages1[is.na(Wages1)] = 1.625

Model2 <- lm(logwage ~ hgc + college + tenure + tenuresq + age + married,data=Wages1)
summary(Model2)

#I wasn't sure how to use my regression above, but since the data was MAR, I used random forest
Wages2 <- Wages %>% drop_na(hgc,tenure)
Wages2 %<>% mutate(tenuresq = tenure^2)
wages.imp <- missForest(Wages2)
View(wages.imp[["ximp"]])
Model3 <- lm(logwage ~ hgc + college + tenure + tenuresq + age + married,data=wages.imp[["ximp"]])
summary(Model3)


#MICE
tempData2 <- mice(Wages2,m=50,seed=245435)
modelFit2 <- with(tempData2,lm(logwage ~ hgc + college + tenure + tenuresq + age + married))
summary(pool(modelFit2))


stargazer(Model1, Model2, Model3, title="Results", align=TRUE)
