# Initial

# Libraries and parameters----



library(forecast)
#install.packages("stlplus")
library(stlplus)
library(stats)
library(caret)
#install.packages("imputeTS")
library(imputeTS)
#install.packages("ggfortify")
library(ggfortify)
#install.packages("seasonal")
library(seasonal)
library(tseries)
#install.packages("greybox")
library(greybox)
#install.packages("smooth")
library(smooth)

h = 16


# data loading----

setwd("/Users/mojim/OneDrive - University of Leeds/Forecasting and Advance BA/coursework/r directory")
RawData <- read.csv("/Users/mojim/OneDrive - University of Leeds/Forecasting and Advance BA/coursework/PCE.csv", header = T)
str(RawData)
table(RawData$DATE)

dataPre <- ts(RawData$PCE, start = c(1959,1), end = c(2021,12), frequency = 12)
data <- window(dataPre, start = c(1990,1))
plot(data, xlab = "Years", ylab="Personal consumption expenditures", main="US PCE during time")
is.na(data)

# Data preparation----

sum(is.na(data))
plot(data)
ggseasonplot(data, year.labels=TRUE, year.labels.left=TRUE) + ylab("PEC") + ggtitle("PEC vs Time")
ggsubseriesplot(data) + ylab("Units sold (millions)") + ggtitle("Seasonal subseries plot: iPhone units sold")
adf.test(imputed)

## Imputation

imputed <- na_kalman(data,model = "auto.arima", smooth = F)
imp_locf <- na_locf(data)
ggplot_na_imputations(data, imp_locf)
plot(imputed, main = "Imputed data")
adf.test(imputed)

## Outlier removal

tsclean <- tsclean(imputed, replace.missing = F, iterate = 100) 
par(mfrow = c(1,3))
plot(tsclean, main = "tsclean")
plot(data, main ="original dataset")
write.csv(tsclean,file="tsclean.csv")
adf.test(tsclean)

## Data decomposition using x11

x11<-seas(tsclean, x11="")
par(mfrow = c(2,1))
autoplot(x11)+ggtitle("x11 result for tscleaned data")

x11imputed<-seas(imputed, x11="")
par(mfrow = c(2,1))
autoplot(x11imputed)+ggtitle("x11 result for imputed data")

a <- as.matrix(x11$data)
write.csv(a,file="a.csv")
b <- read.csv("a.csv")
par(mfrow = c(1,1))
plot(b$irregular, 
     col = "blue", main = 'Remainder - X11', ylab = "", ylim = c(0.97,1.03))
qqnorm(b$irregular, ylim = c(0.97,1.03))
qqline(b$irregular)

summary(x11)


## stationary datset

sta <- diff(tsclean)
plot(sta, main = "diff(1)")
adf.test(sta)

## data partitioning

imp1 <- ts(imputed, start=c(1990,1), end = c(2021,12), frequency=12)
tsc1 <- ts(tsclean, start=c(1990,1), end = c(2021,12), frequency=12)

### roll up dataset

allDStrain <- ts(imputed, start=c(1990,1), end = c(2015,12), frequency=12)#80%
allDStest <- ts(imputed, start=c(2016,1), end = c(2021,12), frequency=12)#20%


### imputed data

pallTTRimp <- window(imp1, start=c(1990,1), end =c(2021,10))#train for short period predict
pallTTEimp <- window(imp1, start=c(2021,11),end= c(2021,12))#test for short period predict

P1TTRimp <- window(imp1, start =c(1990,1), end =c(1994,12))#part1 imputed training
P1TTEimp <- window(imp1, start =c(1995,1), end =c(1996,4))#part1 imputed test

P2TTRimp <- window(imp1, start =c(1994,2), end =c(1999,1))#part2 imputed training
P2TTEimp <- window(imp1, start =c(1999,2), end =c(2000,5))#part2 imputed test

P3TTRimp <- window(imp1, start =c(1999,4), end =c(2004,3))#part3 imputed training
P3TTEimp <- window(imp1, start =c(2004,4), end =c(2005,7))#part3 imputed test

P4TTRimp <- window(imp1, start =c(2004,8), end =c(2009,7))#part4 imputed training
P4TTEimp <- window(imp1, start =c(2009,8), end =c(2010,11))#part4 imputed test

P5TTRimp <- window(imp1, start =c(2010,2), end =c(2015,1))#part5 imputed training
P5TTEimp <- window(imp1, start =c(2015,2), end =c(2016,5))#part5 imputed test

P6TTRimp <- window(imp1, start =c(2015,9), end =c(2020,8))#part6 imputed training
P6TTEimp <- window(imp1, start =c(2020,9), end =c(2021,12))#part6 imputed test

###tscleaned data

pallTTR <- window(tsc1, start=c(1990,1), end =c(2021,10))#train for short period predict
pallTTE <- window(tsc1, start=c(2021,11),end= c(2021,12))#test for short period predict

P1TTR <- window(tsc1, start =c(1990,1), end =c(1994,12))#part1 tscleaned training
P1TTE <- window(tsc1, start =c(1995,1), end =c(1996,4))#part1 tscleaned test

P2TTR <- window(tsc1, start =c(1994,2), end =c(1999,1))#part2 tscleaned training
P2TTE <- window(tsc1, start =c(1999,2), end =c(2000,5))#part2 tscleaned test

P3TTR <- window(tsc1, start =c(1999,4), end =c(2004,3))#part3 tscleaned training
P3TTE <- window(tsc1, start =c(2004,4), end =c(2005,7))#part3 tscleaned test

P4TTR <- window(tsc1, start =c(2004,8), end =c(2009,7))#part4 tscleaned training
P4TTE <- window(tsc1, start =c(2009,8), end =c(2010,11))#part4 tscleaned test

P5TTR <- window(tsc1, start =c(2010,2), end =c(2015,1))#part5 tscleaned training
P5TTE <- window(tsc1, start =c(2015,2), end =c(2016,5))#part5 tscleaned test

P6TTR <- window(tsc1, start =c(2015,9), end =c(2020,8))#part6 tscleaned training
P6TTE <- window(tsc1, start =c(2020,9), end =c(2021,12))#part6 tscleaned test

# Forecast models----

## Drift model
Fitdrall <- rwf(pallTTR,drift=TRUE, h=h)
summary(Fitdrall)
?rwf
Fitdr1 <- rwf(P1TTR, drift=TRUE, h=h)
Fitdr2 <- rwf(P2TTR, drift=TRUE, h=h)
Fitdr3 <- rwf(P3TTR, drift=TRUE,h=h)
Fitdr4 <- rwf(P4TTR,drift=TRUE, h=h)
Fitdr5 <- rwf(P5TTR, drift=TRUE,h=h)
Fitdr6 <- rwf(P6TTR, drift=TRUE,h=h)
accsdrall <- accuracy(Fitdrall, pallTTE)
accsdr1 <- accuracy(Fitdr1, P1TTE)
accsdr2 <- accuracy(Fitdr2, P2TTE)
accsdr3 <- accuracy(Fitdr3, P3TTE)
accsdr4 <- accuracy(Fitdr4, P4TTE)
accsdr5 <- accuracy(Fitdr5, P5TTE)
accsdr6 <- accuracy(Fitdr6, P6TTE)

accuracydr <- rbind(accsdr1,accsdr2,accsdr3,accsdr4,accsdr5,accsdr6)
tabdrtscleaned <- as.matrix(accuracydr)
write.csv(tabdrtscleaned, file= "tabdrtscleaned.csv")

plot(Fitdrall)
lines(P5ITE)

Fitdrallimp <- rwf(pallTTRimp,drift=TRUE, h=h)
Fitdr1imp <- rwf(P1TTRimp, drift=TRUE, h=h)
Fitdr2imp <- rwf(P2TTRimp, drift=TRUE, h=h)
Fitdr3imp <- rwf(P3TTRimp, drift=TRUE,h=h)
Fitdr4imp <- rwf(P4TTRimp,drift=TRUE, h=h)
Fitdr5imp <- rwf(P5TTRimp, drift=TRUE,h=h)
Fitdr6imp <- rwf(P6TTRimp, drift=TRUE,h=h)
accsdrallimp <- accuracy(Fitdrallimp, pallTTEimp)
accsdr1imp <- accuracy(Fitdr1imp, P1TTEimp)
accsdr2imp <- accuracy(Fitdr2imp, P2TTEimp)
accsdr3imp <- accuracy(Fitdr3imp, P3TTEimp)
accsdr4imp <- accuracy(Fitdr4imp, P4TTEimp)
accsdr5imp <- accuracy(Fitdr5imp, P5TTEimp)
accsdr6imp <- accuracy(Fitdr6imp, P6TTEimp)
accuracydrimp <- rbind(accsdr1imp,accsdr2imp,accsdr3imp,accsdr4imp,accsdr5imp,accsdr6imp)
tabdrimp <- as.matrix(accuracydrimp)
write.csv(tabdrimp, file= "tabdrtimp.csv")


## simple exponential smoothing model
Fitesall <- ses(pallTTR, initial="optimal",h=h)
Fites1 <- ses(P1TTR, initial="optimal",h=h)
Fites2 <- ses(P2TTR, initial="optimal",h=h)
Fites3 <- ses(P3TTR, initial="optimal",h=h)
Fites4 <- ses(P4TTR, initial="optimal",h=h)
Fites5 <- ses(P5TTR, initial="optimal",h=h)
Fites6 <- ses(P6TTR, initial="optimal",h=h)
summary(Fitesall)
accsesall <- accuracy(Fitesall, pallTTE)
accses1 <- accuracy(Fites1, P1TTE)
accses2 <- accuracy(Fites2, P2TTE)
accses3 <- accuracy(Fites3, P3TTE)
accses4 <- accuracy(Fites4, P4TTE)
accses5 <- accuracy(Fites5, P5TTE)
accses6 <- accuracy(Fites6, P6TTE)

accuracyes <- rbind(accses1,accses2,accses3,accses4,accses5,accses6)
tabsetscleaned <- as.matrix(accuracyes)
write.csv(tabsetscleaned, file= "tabsetscleaned.csv")

plot(Fites5)
lines(P5ITE)

FitSes <- ses(P1TTR, initial="optimal", h=16)
summary(FitSes)
plot(FitSes)

Fitesallimp <- ses(pallTTR, initial="optimal",h=h)
Fites1imp <- ses(P1TTRimp, initial="optimal",h=h)
Fites2imp <- ses(P2TTRimp, initial="optimal",h=h)
Fites3imp <- ses(P3TTRimp, initial="optimal",h=h)
Fites4imp <- ses(P4TTRimp, initial="optimal",h=h)
Fites5imp <- ses(P5TTRimp, initial="optimal",h=h)
Fites6imp <- ses(P6TTRimp, initial="optimal",h=h)
accsesallimp <- accuracy(Fitesallimp, pallTTEimp)
accses1imp <- accuracy(Fites1imp, P1TTEimp)
accses2imp <- accuracy(Fites2imp, P2TTEimp)
accses3imp <- accuracy(Fites3imp, P3TTEimp)
accses4imp <- accuracy(Fites4imp, P4TTEimp)
accses5imp <- accuracy(Fites5imp, P5TTEimp)
accses6imp <- accuracy(Fites6imp, P6TTEimp)
accuracyesimp <- rbind(accses1imp,accses2imp,accses3imp,accses4imp,accses5imp,accses6imp)
tabesimp <- as.matrix(accuracyesimp)
write.csv(tabesimp, file= "tabestimp.csv")

## Auto ARIMA
model_select <- auto.arima(pallTTR)
Fitaraall <- arima(pallTTR, order = c(1,2,1))
Fitarall <- forecast(Fitaraall , h=h)
checkresiduals(Fitaraall)
Fitara1 <- arima(P1TTR, order = c(1,2,1))
Fitar1 <- forecast(Fitara1 , h=h)
Fitara2 <- arima(P2TTR, order = c(1,2,1))
Fitar2 <- forecast(Fitara2 , h=h)
Fitara3 <- arima(P3TTR, order = c(1,2,1))
Fitar3 <- forecast(Fitara3 , h=h)
Fitara4 <- arima(P4TTR, order = c(1,2,1))
Fitar4 <- forecast(Fitara4 , h=h)
Fitara5 <- arima(P5TTR, order = c(1,2,1))
Fitar5 <- forecast(Fitara5 , h=h)
Fitara6 <- arima(P6TTR, order = c(1,2,1))
Fitar6 <- forecast(Fitara6 , h=h)
accarall <- accuracy(Fitarall, pallTTE)
accar1 <- accuracy(Fitar1, P1TTE)
accar2 <- accuracy(Fitar2, P2TTE)
accar3 <- accuracy(Fitar3, P3TTE)
accar4 <- accuracy(Fitar4, P4TTE)
accar5 <- accuracy(Fitar5, P5TTE)
accar6 <- accuracy(Fitar6, P6TTE)

accuracyar <- rbind(accar1,accar2,accar3,accar4,accar5,accar6)
tabartscleaned <- as.matrix(accuracyar)
write.csv(tabartscleaned, file= "tabartscleaned.csv")

plot(window(Fitarall, start=c(2018,1), end =c(2020,10)))
lines(pallTTE)

Summary()

Fitaraallimp <- arima(pallTTRimp, order = c(1,2,1))
Fitarallimp <- forecast(Fitaraallimp , h=h)
checkresiduals(Fitaraallimp)
Fitara1imp <- arima(P1TTRimp, order = c(1,2,1))
Fitar1imp <- forecast(Fitara1imp , h=h)
Fitara2imp <- arima(P2TTRimp, order = c(1,2,1))
Fitar2imp <- forecast(Fitara2imp , h=h)
Fitara3imp <- arima(P3TTRimp, order = c(1,2,1))
Fitar3imp <- forecast(Fitara3imp , h=h)
Fitara4imp <- arima(P4TTRimp, order = c(1,2,1))
Fitar4imp <- forecast(Fitara4imp , h=h)
Fitara5imp <- arima(P5TTRimp, order = c(1,2,1))
Fitar5imp <- forecast(Fitara5imp , h=h)
Fitara6imp <- arima(P6TTRimp, order = c(1,2,1))
Fitar6imp <- forecast(Fitara6imp , h=h)
accarallimp <- accuracy(Fitarallimp, pallTTEimp)
accar1imp <- accuracy(Fitar1imp, P1TTEimp)
accar2imp <- accuracy(Fitar2imp, P2TTEimp)
accar3imp <- accuracy(Fitar3imp, P3TTEimp)
accar4imp <- accuracy(Fitar4imp, P4TTEimp)
accar5imp <- accuracy(Fitar5imp, P5TTEimp)
accar6imp <- accuracy(Fitar6imp, P6TTEimp)
accuracyarimp <- rbind(accar1imp,accar2imp,accar3imp,accar4imp,accar5imp,accar6imp)
tabartimp <- as.matrix(accuracyarimp)
write.csv(tabartimp, file= "tabarimp.csv")


#model evaluation

##plot for tscleaned data


par(mfrow = c(3,3))
plot(Fitar4)
lines(P4TTE)
plot(Fitar5)
lines(P5TTE)
plot(Fitar6)
lines(P6TTE)

plot(Fites4)
lines(P4TTE)
plot(Fites5)
lines(P5TTE)
plot(Fites6)
lines(P6TTE)

plot(Fitdr4)
lines(P4TTE)
plot(Fitdr5)
lines(P5TTE)
plot(Fitdr6)
lines(P6TTE)

##plot for imputed data

par(mfrow = c(3,3))
plot(Fitar4imp)
lines(P4TTEimp)
plot(Fitar5imp)
lines(P5TTEimp)
plot(Fitar6imp)
lines(P6TTEimp)

plot(Fites4imp)
lines(P4TTEimp)
plot(Fites5imp)
lines(P5TTEimp)
plot(Fites6imp)
lines(P6TTEimp)

plot(Fitdr4imp)
lines(P4TTEimp)
plot(Fitdr5imp)
lines(P5TTEimp)
plot(Fitdr6imp)
lines(P6TTEimp)
# october 2022 prediction

FitFinall <- rwf(imp1,drift = T,h=10)
par(mfrow = c(1,1))
plot(FitFinall)
FitFinall2 <- rwf(tsc1,drift = T,h=10)
par(mfrow = c(1,1))
plot(FitFinall2)

# rolling up

fit1 <- ets(allDStrain)
fit2 <- ets(allDStest, model=fit1)
onestep1 <- fitted(fit2)
accrollets <- accuracy(onestep1, allDStest)

fit3 <- Arima(allDStrain, order = c(1,2,1))
fit4 <- Arima(allDStest, model=fit3)
onestep2 <- fitted(fit4)
accrollar <- accuracy(onestep2, allDStest)

fit5 <- rwf(allDStrain, drift=T)
fit6 <- rwf(allDStest, model=fit5)
onestep3 <- fitted(fit6)
accrolldrift <- accuracy(onestep3, allDStest)

rofine <- rbind(accrolldrift,accrollar,accrollets)
rofinemat <- as.matrix(rofine)
write.csv(rofinemat,"rofinemat.csv")
