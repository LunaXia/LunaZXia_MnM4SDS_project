#spatiotemperal: sptemp lag
library(dplyr)
library(tidyr)
library(randomForest)
library(ranger)
library(rgdal)

EPAfeaV <- c(5:149)
ACSfeaV <- c(150:168)
KEYLOCfeaV <- c(169:176)
timelag1V <- c(177,178)
timelag12V <- c(177:180,183,184)
timelag123V <- c(177:186)
HS_V <- c(187)

# set weght for heroin
varWeight_Heroin2 = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                     0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                     0.52,0.59,0.54, 0.57, 0.8,0.8,0.53,0.5)

varWeight_Heroin3 = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                      0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                      0.52,0.59,0.54, 0.57, 0.8,0.8,0.53,0.5,
                      0.5,0.5)

varWeight_Heroin4 = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                      0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                      0.52,0.59,0.54, 0.57, 0.8,0.8,0.53,0.5,
                      0.5,0.5,0.5,0.5,0.5,0.5)

varWeight_Heroin5 = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                      0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,
                      0.52,0.59,0.54, 0.57, 0.8,0.8,0.53,0.5,
                      0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)

#########################
## No key location model
#########################
Heroin.train <- Heroin[(Heroin$Year >= 161 & Heroin$Year <= 182 ),]
Heroin.test <- Heroin[(Heroin$Year== 191),]
Heroin.train[is.na(Heroin.train)] <- 0
Heroin.test[is.na(Heroin.test)] <- 0

set.seed(123) 
ranger.Heroin1 <- ranger(HS ~ ., data = Heroin.train[,c(EPAsel_Heroin,ACSfeaV,HS_V)], importance = 'impurity', mtry = 9, num.trees=300)
ranger::importance(ranger.Heroin1)

HeroinPreriction1 = predict(ranger.Heroin1, Heroin.test[,c(EPAsel_Heroin,ACSfeaV,HS_V)])
Heroin.test$predict1 = ranger::predictions(HeroinPreriction1)
table(Heroin.test$HS, Heroin.test$predict1)

#########################
## No time lag model
#########################
Heroin.train <- Heroin[(Heroin$Year >= 161 & Heroin$Year <= 182 ),]
Heroin.train[is.na(Heroin.train)] <- 0


#ranger random forest
set.seed(123)
ranger.Heroin2 <- ranger(HS ~ ., data = Heroin.train[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,HS_V)], importance = 'impurity', mtry = 9, num.trees=300, split.select.weights = varWeight_Heroin2)
ranger::importance(ranger.Heroin2)


HeroinPreriction2 = predict(ranger.Heroin2, Heroin.test[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,HS_V)])
Heroin.test$predict2 = ranger::predictions(HeroinPreriction2)
table(Heroin.test$HS, Heroin.test$predict2)



#########################
## time lag = 1 model
#########################
Heroin.train <- Heroin[(Heroin$Year >= 162 & Heroin$Year <= 182 ),]
Heroin.train[is.na(Heroin.train)] <- 0


#ranger random forest
set.seed(123)
ranger.Heroin3 <- ranger(HS ~ ., data = Heroin.train[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag1V,HS_V)], importance = 'impurity', mtry = 9, num.trees=300, split.select.weights = varWeight_Heroin3)
ranger::importance(ranger.Heroin3)


HeroinPreriction3 = predict(ranger.Heroin3, Heroin.test[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag1V,HS_V)])
Heroin.test$predict3 = ranger::predictions(HeroinPreriction3)
table(Heroin.test$HS, Heroin.test$predict3)


#########################
## time lag = 2 model
#########################
Heroin.train <- Heroin[(Heroin$Year >= 171 & Heroin$Year <= 182 ),]
Heroin.train[is.na(Heroin.train)] <- 0


#ranger random forest
set.seed(123)
ranger.Heroin4 <- ranger(HS ~ ., data = Heroin.train[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag12V,HS_V)], importance = 'impurity', mtry = 9, num.trees=300, split.select.weights = varWeight_Heroin4)
ranger::importance(ranger.Heroin4)


HeroinPreriction4 = predict(ranger.Heroin4, Heroin.test[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag12V,HS_V)])
Heroin.test$predict4 = ranger::predictions(HeroinPreriction4)
table(Heroin.test$HS, Heroin.test$predict4)



#########################
## time lag = 3 model
#########################
Heroin.train <- Heroin[(Heroin$Year >= 172 & Heroin$Year <= 182 ),]
Heroin.train[is.na(Heroin.train)] <- 0


#ranger random forest
set.seed(123)
ranger.Heroin5 <- ranger(HS ~ ., data = Heroin.train[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag123V,HS_V)], importance = 'impurity', mtry = 9, num.trees=300, split.select.weights = varWeight_Heroin5)
ranger::importance(ranger.Heroin5)


HeroinPreriction5 = predict(ranger.Heroin5, Heroin.test[,c(EPAsel_Heroin,ACSfeaV,KEYLOCfeaV,timelag123V,HS_V)])
Heroin.test$predict5 = ranger::predictions(HeroinPreriction5)
table(Heroin.test$HS, Heroin.test$predict5)



#######################################
# plot predicted map for three models
#######################################

# create polygon to plot Heroin results
chicagoPoly <- rgdal::readOGR(dsn ='D:/Drug activity/ChicagoData/LacationAnalysis_TidyCSV', layer='ChicagoPoly')
HeroinHS <- read.csv(file = "D:/Drug activity/ChicagoData/LacationAnalysis_TidyCSV/HeroinHS_halfyr.csv")
HeroinHS[,c(4:10)] <- lapply(HeroinHS[,c(4:10)], as.factor)
HeroinPoly <- merge(chicagoPoly, HeroinHS, by.x="GEOID_Data", by.y="GEOID_Data", suffixes = c(".a",".b"))
Heroin.predict <- Heroin.test %>% select(FID, GEOID10, GEOID_Data,predict1,predict2,predict3,predict4,predict5)
HeroinPredictPoly <- merge(HeroinPoly, Heroin.predict, by.x="GEOID_Data", by.y="GEOID_Data", suffixes = c(".c",".d"))


par(mfrow=c(2,3),mai = c(0.01, 0.01, 0.01, 0.01))

polycolor <- c('gray99','orange1')[HeroinPredictPoly$predict2]
bordercolor <- c('gray75','orange1')[HeroinPredictPoly$predict2]
plot(HeroinPredictPoly, col = polycolor, border = bordercolor)

polycolor <- c('gray99','orange1')[HeroinPredictPoly$predict3]
bordercolor <- c('gray75','orange1')[HeroinPredictPoly$predict3]
plot(HeroinPredictPoly, col = polycolor, border = bordercolor)

plot(0,type='n',axes=FALSE,ann=FALSE)

polycolor <- c('gray99','orange1')[HeroinPredictPoly$predict4]
bordercolor <- c('gray75','orange1')[HeroinPredictPoly$predict4]
plot(HeroinPredictPoly, col = polycolor, border = bordercolor)

polycolor <- c('gray99','orange1')[HeroinPredictPoly$predict5]
bordercolor <- c('gray75','orange1')[HeroinPredictPoly$predict5]
plot(HeroinPredictPoly, col = polycolor, border = bordercolor)


polycolor <- c('gray99','orange1')[HeroinPredictPoly$HSheroin191]
bordercolor <- c('gray75','orange1')[HeroinPredictPoly$HSheroin191]
plot(HeroinPredictPoly, col = polycolor, border = bordercolor)


polycolor <- c('gray99','orange1')[HeroinPredictPoly$predict1]
bordercolor <- c('gray75','orange1')[HeroinPredictPoly$predict1]
plot(HeroinPredictPoly, col = polycolor, border = bordercolor)

#writeOGR(HeroinPredictPoly, dsn = "D:/Drug activity/Shp_Map" , layer = "Heroin_HS_halfyr", driver="ESRI Shapefile")





