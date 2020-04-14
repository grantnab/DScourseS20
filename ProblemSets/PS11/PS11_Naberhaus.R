library(ballr)
library(tidyverse)
library(tidyselect)
library(magrittr)
library(estimatr)
library(glmnet)
library(caret)
library(mlr)

#For PER 
Players <- NBAPerGameAdvStatistics(season = 2018)
Players %<>% select(player, tm, g, mp, per)

#Create df for each team to find TEAM PER
PlayersATL <- Players
PlayersATL %<>% filter(tm == "ATL")
PlayersATL %<>% mutate(TeamPER = ((PlayersATL$mp)/(sum(PlayersATL$mp))*PlayersATL$per))

PlayersBOS <- Players
PlayersBOS %<>% filter(tm == "BOS")
PlayersBOS %<>% mutate(TeamPER = ((PlayersBOS$mp)/(sum(PlayersBOS$mp))*PlayersBOS$per))

PlayersBRK <- Players
PlayersBRK %<>% filter(tm == "BRK")
PlayersBRK %<>% mutate(TeamPER = ((PlayersBRK$mp)/(sum(PlayersBRK$mp))*PlayersBRK$per))

PlayersCHI <- Players
PlayersCHI %<>% filter(tm == "CHI")
PlayersCHI %<>% mutate(TeamPER = ((PlayersCHI$mp)/(sum(PlayersCHI$mp))*PlayersCHI$per))

PlayersCHO <- Players
PlayersCHO %<>% filter(tm == "CHO")
PlayersCHO %<>% mutate(TeamPER = ((PlayersCHO$mp)/(sum(PlayersCHO$mp))*PlayersCHO$per))

PlayersCLE <- Players
PlayersCLE %<>% filter(tm == "CLE")
PlayersCLE %<>% mutate(TeamPER = ((PlayersCLE$mp)/(sum(PlayersCLE$mp))*PlayersCLE$per))

PlayersDAL <- Players
PlayersDAL %<>% filter(tm == "DAL")
PlayersDAL %<>% mutate(TeamPER = ((PlayersDAL$mp)/(sum(PlayersDAL$mp))*PlayersDAL$per))

PlayersDEN <- Players
PlayersDEN %<>% filter(tm == "DEN")
PlayersDEN %<>% mutate(TeamPER = ((PlayersDEN$mp)/(sum(PlayersDEN$mp))*PlayersDEN$per))

PlayersDET <- Players
PlayersDET %<>% filter(tm == "DET")
PlayersDET %<>% mutate(TeamPER = ((PlayersDET$mp)/(sum(PlayersDET$mp))*PlayersDET$per))

PlayersGSW <- Players
PlayersGSW %<>% filter(tm == "GSW")
PlayersGSW %<>% mutate(TeamPER = ((PlayersGSW$mp)/(sum(PlayersGSW$mp))*PlayersGSW$per))

PlayersHOU <- Players
PlayersHOU %<>% filter(tm == "HOU")
PlayersHOU %<>% mutate(TeamPER = ((PlayersHOU$mp)/(sum(PlayersHOU$mp))*PlayersHOU$per))

PlayersIND <- Players
PlayersIND %<>% filter(tm == "IND")
PlayersIND %<>% mutate(TeamPER = ((PlayersIND$mp)/(sum(PlayersIND$mp))*PlayersIND$per))

PlayersLAC <- Players
PlayersLAC %<>% filter(tm == "LAC")
PlayersLAC %<>% mutate(TeamPER = ((PlayersLAC$mp)/(sum(PlayersLAC$mp))*PlayersLAC$per))

PlayersLAL <- Players
PlayersLAL %<>% filter(tm == "LAL")
PlayersLAL %<>% mutate(TeamPER = ((PlayersLAL$mp)/(sum(PlayersLAL$mp))*PlayersLAL$per))

PlayersMEM <- Players
PlayersMEM %<>% filter(tm == "MEM")
PlayersMEM %<>% mutate(TeamPER = ((PlayersMEM$mp)/(sum(PlayersMEM$mp))*PlayersMEM$per))

PlayersMIA <- Players
PlayersMIA %<>% filter(tm == "MIA")
PlayersMIA %<>% mutate(TeamPER = ((PlayersMIA$mp)/(sum(PlayersMIA$mp))*PlayersMIA$per))

PlayersMIL <- Players
PlayersMIL %<>% filter(tm == "MIL")
PlayersMIL %<>% mutate(TeamPER = ((PlayersMIL$mp)/(sum(PlayersMIL$mp))*PlayersMIL$per))

PlayersMIN <- Players
PlayersMIN %<>% filter(tm == "MIN")
PlayersMIN %<>% mutate(TeamPER = ((PlayersMIN$mp)/(sum(PlayersMIN$mp))*PlayersMIN$per))

PlayersNOP <- Players
PlayersNOP %<>% filter(tm == "NOP")
PlayersNOP %<>% mutate(TeamPER = ((PlayersNOP$mp)/(sum(PlayersNOP$mp))*PlayersNOP$per))

PlayersNYK <- Players
PlayersNYK %<>% filter(tm == "NYK")
PlayersNYK %<>% mutate(TeamPER = ((PlayersNYK$mp)/(sum(PlayersNYK$mp))*PlayersNYK$per))

PlayersOKC <- Players
PlayersOKC %<>% filter(tm == "OKC")
PlayersOKC %<>% mutate(TeamPER = ((PlayersOKC$mp)/(sum(PlayersOKC$mp))*PlayersOKC$per))

PlayersORL <- Players
PlayersORL %<>% filter(tm == "ORL")
PlayersORL %<>% mutate(TeamPER = ((PlayersORL$mp)/(sum(PlayersORL$mp))*PlayersORL$per))

PlayersPHI <- Players
PlayersPHI %<>% filter(tm == "PHI")
PlayersPHI %<>% mutate(TeamPER = ((PlayersPHI$mp)/(sum(PlayersPHI$mp))*PlayersPHI$per))

PlayersPHO <- Players
PlayersPHO %<>% filter(tm == "PHO")
PlayersPHO %<>% mutate(TeamPER = ((PlayersPHO$mp)/(sum(PlayersPHO$mp))*PlayersPHO$per))

PlayersPOR <- Players
PlayersPOR %<>% filter(tm == "POR")
PlayersPOR %<>% mutate(TeamPER = ((PlayersPOR$mp)/(sum(PlayersPOR$mp))*PlayersPOR$per))

PlayersSAC <- Players
PlayersSAC %<>% filter(tm == "SAC")
PlayersSAC %<>% mutate(TeamPER = ((PlayersSAC$mp)/(sum(PlayersSAC$mp))*PlayersSAC$per))

PlayersSAS <- Players
PlayersSAS %<>% filter(tm == "SAS")
PlayersSAS %<>% mutate(TeamPER = ((PlayersSAS$mp)/(sum(PlayersSAS$mp))*PlayersSAS$per))

PlayersTOR <- Players
PlayersTOR %<>% filter(tm == "TOR")
PlayersTOR %<>% mutate(TeamPER = ((PlayersTOR$mp)/(sum(PlayersTOR$mp))*PlayersTOR$per))

PlayersUTA <- Players
PlayersUTA %<>% filter(tm == "UTA")
PlayersUTA %<>% mutate(TeamPER = ((PlayersUTA$mp)/(sum(PlayersUTA$mp))*PlayersUTA$per))

PlayersWAS <- Players
PlayersWAS %<>% filter(tm == "WAS")
PlayersWAS %<>% mutate(TeamPER = ((PlayersWAS$mp)/(sum(PlayersWAS$mp))*PlayersWAS$per))


#TEAMS
#Atlanta
ATLdf <- NBASeasonTeamByYear("ATL", 2018)
ATLdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
ATLdf %<>% mutate(TeamPER = sum(PlayersATL$TeamPER))
#Boston
BOSdf <- NBASeasonTeamByYear("BOS", 2018)
BOSdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
BOSdf %<>% mutate(TeamPER = sum(PlayersBOS$TeamPER))
#Brooklyn
BRKdf <- NBASeasonTeamByYear("BRK", 2018)
BRKdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
BRKdf %<>% mutate(TeamPER = sum(PlayersBRK$TeamPER))
#Chicago
CHIdf <- NBASeasonTeamByYear("CHI", 2018)
CHIdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
CHIdf %<>% mutate(TeamPER = sum(PlayersCHI$TeamPER))
#Charlotte
CHOdf <- NBASeasonTeamByYear("CHO", 2018)
CHOdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
CHOdf %<>% mutate(TeamPER = sum(PlayersCHO$TeamPER))
#Cleveland
CLEdf <- NBASeasonTeamByYear("CLE", 2018)
CLEdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
CLEdf %<>% mutate(TeamPER = sum(PlayersCLE$TeamPER))
#Dallas
DALdf <- NBASeasonTeamByYear("DAL", 2018)
DALdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
DALdf %<>% mutate(TeamPER = sum(PlayersDAL$TeamPER))
#Denver
DENdf <- NBASeasonTeamByYear("DEN", 2018)
DENdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
DENdf %<>% mutate(TeamPER = sum(PlayersDEN$TeamPER))
#Detriot
DETdf <- NBASeasonTeamByYear("DET", 2018)
DETdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
DETdf %<>% mutate(TeamPER = sum(PlayersDET$TeamPER))
#GoldenState
GSWdf <- NBASeasonTeamByYear("GSW", 2018)
GSWdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
GSWdf %<>% mutate(TeamPER = sum(PlayersGSW$TeamPER))
#Houston
HOUdf <- NBASeasonTeamByYear("HOU", 2018)
HOUdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
HOUdf %<>% mutate(TeamPER = sum(PlayersHOU$TeamPER))
#Indiana
INDdf <- NBASeasonTeamByYear("IND", 2018)
INDdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
INDdf %<>% mutate(TeamPER = sum(PlayersIND$TeamPER))
#LAClippers
LACdf <- NBASeasonTeamByYear("LAC", 2018)
LACdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
LACdf %<>% mutate(TeamPER = sum(PlayersLAC$TeamPER))
#LALakers
LALdf <- NBASeasonTeamByYear("LAL", 2018)
LALdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
LALdf %<>% mutate(TeamPER = sum(PlayersLAL$TeamPER))
#Memphis
MEMdf <- NBASeasonTeamByYear("MEM", 2018)
MEMdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
MEMdf %<>% mutate(TeamPER = sum(PlayersMEM$TeamPER))
#Miami
MIAdf <- NBASeasonTeamByYear("MIA", 2018)
MIAdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
MIAdf %<>% mutate(TeamPER = sum(PlayersMIA$TeamPER))
#Milwaukee
MILdf <- NBASeasonTeamByYear("MIL", 2018)
MILdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
MILdf %<>% mutate(TeamPER = sum(PlayersMIL$TeamPER))
#Minnesota
MINdf <- NBASeasonTeamByYear("MIN", 2018)
MINdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
MINdf %<>% mutate(TeamPER = sum(PlayersMIN$TeamPER))
#NewOrleans
NOPdf <- NBASeasonTeamByYear("NOP", 2018)
NOPdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
NOPdf %<>% mutate(TeamPER = sum(PlayersNOP$TeamPER))
#NewYorkKnicks
NYKdf <- NBASeasonTeamByYear("NYK", 2018)
NYKdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
NYKdf %<>% mutate(TeamPER = sum(PlayersNYK$TeamPER))
#OklahomaCity
OKCdf <- NBASeasonTeamByYear("OKC", 2018)
OKCdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
OKCdf %<>% mutate(TeamPER = sum(PlayersOKC$TeamPER))
#Orlando
ORLdf <- NBASeasonTeamByYear("ORL", 2018)
ORLdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
ORLdf %<>% mutate(TeamPER = sum(PlayersORL$TeamPER))
#Philadelphia
PHIdf <- NBASeasonTeamByYear("PHI", 2018)
PHIdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
PHIdf %<>% mutate(TeamPER = sum(PlayersPHI$TeamPER))
#Phoenix
PHOdf <- NBASeasonTeamByYear("PHO", 2018)
PHOdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
PHOdf %<>% mutate(TeamPER = sum(PlayersPHO$TeamPER))
#Portland
PORdf <- NBASeasonTeamByYear("POR", 2018)
PORdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
PORdf %<>% mutate(TeamPER = sum(PlayersPOR$TeamPER))
#Sacramento
SACdf <- NBASeasonTeamByYear("SAC", 2018)
SACdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
SACdf %<>% mutate(TeamPER = sum(PlayersSAC$TeamPER))
#SanAntonio
SASdf <- NBASeasonTeamByYear("SAS", 2018)
SASdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
SASdf %<>% mutate(TeamPER = sum(PlayersSAS$TeamPER))
#Toronto
TORdf <- NBASeasonTeamByYear("TOR", 2018)
TORdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
TORdf %<>% mutate(TeamPER = sum(PlayersTOR$TeamPER))
#Utah
UTAdf <- NBASeasonTeamByYear("UTA", 2018)
UTAdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
UTAdf %<>% mutate(TeamPER = sum(PlayersUTA$TeamPER))
#Washington
WASdf <- NBASeasonTeamByYear("WAS", 2018)
WASdf %<>% select(away_indicator, x_4, daysbetweengames, opponent)
WASdf %<>% mutate(TeamPER = sum(PlayersWAS$TeamPER))


#MasterDataFrame
MergedData <- rbind(ATLdf, BOSdf, BRKdf, CHIdf, CHOdf, CLEdf, DALdf, DENdf, DETdf,
                    GSWdf, HOUdf, INDdf, LACdf, LALdf, MEMdf, MIAdf, MILdf, MINdf,
                    NOPdf, NYKdf, OKCdf, ORLdf, PHIdf, PHOdf, PORdf, SACdf, SASdf,
                    TORdf, UTAdf, WASdf)

#Renaming and Cleaning
names(MergedData) <- c("Home", "Win", "DaysBetweenGames", "OpponentPER", "TeamPER")
MergedData %<>% mutate(OpponentPER = case_when(as.character(OpponentPER)== "Atlanta Hawks"~ sum(PlayersATL$TeamPER),
                                               as.character(OpponentPER)== "Boston Celtics"~ sum(PlayersBOS$TeamPER),
                                               as.character(OpponentPER)== "Brooklyn Nets"~ sum(PlayersBRK$TeamPER),
                                               as.character(OpponentPER)== "Charlotte Hornets"~ sum(PlayersCHO$TeamPER),
                                               as.character(OpponentPER)== "Chicago Bulls"~ sum(PlayersCHI$TeamPER),
                                               as.character(OpponentPER)== "Cleveland Cavaliers"~ sum(PlayersCLE$TeamPER),
                                               as.character(OpponentPER)== "Dallas Mavericks"~ sum(PlayersDAL$TeamPER),
                                               as.character(OpponentPER)== "Denver Nuggets"~ sum(PlayersDEN$TeamPER),
                                               as.character(OpponentPER)== "Detroit Pistons"~ sum(PlayersDET$TeamPER),
                                               as.character(OpponentPER)== "Golden State Warriors"~ sum(PlayersGSW$TeamPER),
                                               as.character(OpponentPER)== "Houston Rockets"~ sum(PlayersHOU$TeamPER),
                                               as.character(OpponentPER)== "Indiana Pacers"~ sum(PlayersIND$TeamPER),
                                               as.character(OpponentPER)== "Los Angeles Clippers"~ sum(PlayersLAC$TeamPER),
                                               as.character(OpponentPER)== "Los Angeles Lakers"~ sum(PlayersLAL$TeamPER),
                                               as.character(OpponentPER)== "Memphis Grizzlies"~ sum(PlayersMEM$TeamPER),
                                               as.character(OpponentPER)== "Miami Heat"~ sum(PlayersMIA$TeamPER),
                                               as.character(OpponentPER)== "Milwaukee Bucks"~ sum(PlayersMIL$TeamPER),
                                               as.character(OpponentPER)== "Minnesota Timberwolves"~ sum(PlayersMIN$TeamPER),
                                               as.character(OpponentPER)== "New Orleans Pelicans"~ sum(PlayersNOP$TeamPER),
                                               as.character(OpponentPER)== "New York Knicks"~ sum(PlayersNYK$TeamPER),
                                               as.character(OpponentPER)== "Oklahoma City Thunder"~ sum(PlayersOKC$TeamPER),
                                               as.character(OpponentPER)== "Orlando Magic"~ sum(PlayersORL$TeamPER),
                                               as.character(OpponentPER)== "Philadelphia 76ers"~ sum(PlayersPHI$TeamPER),
                                               as.character(OpponentPER)== "Phoenix Suns"~ sum(PlayersPHO$TeamPER),
                                               as.character(OpponentPER)== "Portland Trail Blazers"~ sum(PlayersPOR$TeamPER),
                                               as.character(OpponentPER)== "Sacramento Kings"~ sum(PlayersSAC$TeamPER),
                                               as.character(OpponentPER)== "San Antonio Spurs"~ sum(PlayersSAS$TeamPER),
                                               as.character(OpponentPER)== "Toronto Raptors"~ sum(PlayersTOR$TeamPER),
                                               as.character(OpponentPER)== "Utah Jazz"~ sum(PlayersUTA$TeamPER),
                                               as.character(OpponentPER)== "Washington Wizards"~ sum(PlayersWAS$TeamPER)))


MergedData$Home <- as.numeric(as.factor(MergedData$Home))
MergedData %<>% mutate(Home = case_when(as.numeric(Home)== 1~1,as.numeric(Home)== 2~0))
#Home = 1, Away = 0
MergedData$Win <- as.numeric(as.factor(MergedData$Win))
MergedData %<>% mutate(Win = case_when(as.numeric(Win)== 1~0,as.numeric(Win)== 2~1))
#Win = 1, Loss = 0 
#Create Factor Variables for DaysBetweenGames
MergedData %<>% mutate(DaysBetweenGames = factor(DaysBetweenGames),
                       DaysBetweenGames = fct_recode(DaysBetweenGames, 
                                                     MoreThanFiveDays = "10",
                                                     MoreThanFiveDays = "9",
                                                     MoreThanFiveDays = "8",
                                                     MoreThanFiveDays = "7",
                                                     MoreThanFiveDays = "6",
                                                     FiveDays = "5", 
                                                     FourDays = "4", 
                                                     ThreeDays = "3", 
                                                     TwoDays = "2", 
                                                     OneDay = "1"))


#OLS Robust
ModelOLS <- lm_robust(Win ~ TeamPER + OpponentPER + Home + DaysBetweenGames, data = MergedData)
tidy(ModelOLS)
summary(ModelOLS)

#LOGIT
LogitModel <- glm(Win ~ TeamPER + OpponentPER + Home + DaysBetweenGames, data = MergedData)
summary(LogitModel)

#LOGIT Prediction Accuracy (Code from JuanPablo Murillo published on RPubs)
MergedData <- drop_na(MergedData, DaysBetweenGames)
n <- nrow(MergedData)
train <- sample(n, size = .7*n)
test  <- setdiff(1:n, train)
MergedData.Train <- MergedData[train,]
MergedData.Test  <- MergedData[test, ]

model <- glm(Win ~ TeamPER + OpponentPER + Home + DaysBetweenGames, data = MergedData.Train)

MergedData.Test$model_prob <- predict(model, MergedData.Test, type = "response")

MergedData.Test <- MergedData.Test  %>% mutate(model_pred = 1*(model_prob > .51) + 0)

MergedData.Test <- MergedData.Test %>% mutate(accurate = 1*(model_pred == Win))
sum(MergedData.Test$accurate)/nrow(MergedData.Test)

#Elastic Net Model
MergedData <- drop_na(MergedData, DaysBetweenGames)
n <- nrow(MergedData)
train <- sample(n, size = .7*n)
test  <- setdiff(1:n, train)
MergedData.train <- MergedData[train,]
MergedData.test  <- MergedData[test, ]

theTask <- makeRegrTask(id = "taskname", data = MergedData.train, target = "Win")
print(theTask)

predAlg <- makeLearner("regr.glmnet")

resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Take 50 random guesses at lambda within the interval I specified above
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = MergedData.test)

print(head(prediction$data))
print(RMSE(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$beta