library(readxl)
library(nnfor)
install.packages(nloptr)

# Load the data set
UoWLoad<-read_excel("/Users/sarahilmy/Desktop/IIT/Year 2 Semester 2/Data Mining and ML/CW/UoW_load.xlsx") 

str(UoWLoad)

table(UoWLoad$`11:00`)

is.ts(UoWLoad)

library(tsbox)

# Date and 11th hour information
UoWLoad11 <- UoWLoad[,-2]
UoWLoad11 <- UoWLoad11[,-2]

# Convert the data set into time series
UoWLoadTS <- ts_ts(ts_long(UoWLoad11))

is.ts(UoWLoadTS)

summary(UoWLoadTS)

plot(UoWLoadTS)

start(UoWLoadTS)

abline(reg=lm(UoWLoadTS~time(UoWLoadTS)))

#----------------------------------AR Approach----------------------------------

library(fpp)
library(caret)

training <- UoWLoad[1:430,]$`11:00`
testset <- UoWLoad[431:500,]
training <- ts_ts(ts_long(training))
test <- ts_ts(ts_long(test))
                             
# AR for t-1
AR1 <- arima(training, order = c(1,0,0))
print(AR1)
# Test
AR1Prediction <- data.frame(actual = testset$`11:00`, prediction = predict(AR1,70))
AR1Prediction
AR1Actual <- AR1Prediction$actual
AR1Prediction <- AR1Prediction$prediction.pred
AR1deviation=((AR1Actual-AR1Prediction)/AR1Actual)
AR1comparison=data.frame(AR1Prediction,AR1Actual,AR1deviation)
AR1accuracy=1-abs(mean(AR1deviation))
AR1accuracy

# AR for t-2
AR2 <- arima(training, order = c(2,0,0))
print(AR2)
# Test
AR2prediction <- data.frame(actual = testset$`11:00`, prediction = predict(AR2,70))
AR2prediction
AR2actual <- AR2prediction$actual
AR2prediction <- AR2prediction$prediction.pred
AR2deviation=((AR2actual-AR2prediction)/AR2actual)
AR2comparison=data.frame(AR2prediction,AR2actual,AR2deviation)
AR2accuracy=1-abs(mean(AR2deviation))
AR2accuracy

# AR for t-3
AR3 <- arima(training, order = c(3,0,0))
print(AR3)
# Test
AR3prediction <- data.frame(actual = testset$`11:00`, prediction = predict(AR3,70))
AR3prediction
AR3actual <- AR3prediction$actual
AR3prediction <- AR3prediction$prediction.pred
AR3deviation=((AR3actual-AR3prediction)/AR3actual)
AR3comparison=data.frame(AR3prediction,AR3actual,AR3deviation)
AR3accuracy=1-abs(mean(AR3deviation))
AR3accuracy

# AR for t-4
AR4 <- arima(training, order = c(4,0,0))
print(AR4)
# Test
AR2prediction <- data.frame(actual = testset$`11:00`, prediction = predict(AR2,70))
AR2prediction
AR2actual <- AR2prediction$actual
AR2prediction <- AR2prediction$prediction.pred
AR2deviation=((AR2actual-AR2prediction)/AR2actual)
AR2comparison=data.frame(AR2prediction,AR2actual,AR2deviation)
AR2accuracy=1-abs(mean(AR2deviation))
AR2accuracy

# AR for t-7
AR7 <- arima(training, order = c(7,0,0))
print(AR7)
# Test
AR2prediction <- data.frame(actual = testset$`11:00`, prediction = predict(AR2,70))
AR2prediction
AR2actual <- AR2prediction$actual
AR2prediction <- AR2prediction$prediction.pred
AR2deviation=((AR2actual-AR2prediction)/AR2actual)
AR2comparison=data.frame(AR2prediction,AR2actual,AR2deviation)
AR2accuracy=1-abs(mean(AR2deviation))
AR2accuracy

#----------------------------------Normalization--------------------------------

dfNormZ <- UoWLoad[,-1]

is.numeric(UoWLoad$`09:00`)

# Z Score Standardization 
dfNormZ <- as.data.frame( scale(UoWLoad$`09:00`))
dfNormZ

# Min Max Normalization 
UoWLoadNormalized <- UoWLoad

min <- min(UoWLoadNormalized$`11:00`)
max <- max(UoWLoadNormalized$`11:00`)

UoWLoadNormalized$`11:00` <-(UoWLoadNormalized$`11:00`-min) /
  (max-min)

UoWLoadNormalized$`09:00` <-(UoWLoadNormalized$`09:00`-
                               min(UoWLoadNormalized$`09:00`)) /
  (max(UoWLoadNormalized$`09:00`)-min(UoWLoadNormalized$`09:00`))

UoWLoadNormalized$`10:00` <-(UoWLoadNormalized$`10:00`-
                               min(UoWLoadNormalized$`10:00`)) /
  (max(UoWLoadNormalized$`10:00`)-min(UoWLoadNormalized$`10:00`))

str(UoWLoadNormalized)

#----------------------------------NARX Approach--------------------------------

fit1 <- mlp(UoWLoad,xreg=tt,xreg.lags=list(0),xreg.keep=TRUE)

#------------------------------MLP for AR approach------------------------------

colnames(UoWLoadNormalized) <- c('Dates','Hour9','Hour10','Hour11')

#training and test data 
trainset <- UoWLoadNormalized[1:430,]
testset <- UoWLoadNormalized[351:500,]

# Neural Network
library(neuralnet)
nn <- neuralnet(`Hour11` ~ Hour11, data=trainset, hidden=c(2,1), 
                linear.output=FALSE, threshold=0.01, learningrate.factor =
                  list(minus = 0.5, plus = 1.2), )
nn$result.matrix
plot(nn)
#Test the resulting output
temp_test <- subset(testset, select = c(`Hour11`))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$`Hour11`, 
                      prediction = nn.results$net.result)
results
# Denormalize results
results <- results*(max-min) + min
results
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
roundedresultsdf

actual <- roundedresultsdf$actual
prediction <- roundedresultsdf$prediction

table(actual,prediction)

deviation=((actual-prediction)/actual)
comparison=data.frame(prediction,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy

#------------------------------MLP for NARX approach----------------------------
nn2 <- neuralnet(`Hour11` ~ `Hour9` + `Hour10` + `Hour11`, data=trainset, 
                 hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn2$result.matrix
plot(nn2)
#Test the resulting output
temp_test2 <- subset(testset, select = c( `Hour9`, `Hour10`, `Hour11`))
head(temp_test2)
nn2.results <- compute(nn2, temp_test2)
results2 <- data.frame(actual = testset$Hour11, 
                       prediction = nn2.results$net.result)
results2
# Denormalize results
results2 <- results2*(max-min) + min
results2
roundedresults2<-sapply(results2,round,digits=0)
roundedresultsdf2=data.frame(roundedresults2)
attach(roundedresultsdf2)
roundedresultsdf2

actual <- roundedresultsdf2$actual
prediction <- roundedresultsdf2$prediction

table(actual,prediction)

deviation=((actual-prediction)/actual)
comparison=data.frame(prediction,actual,deviation)
accuracy=1-abs(mean(deviation))
accuracy
