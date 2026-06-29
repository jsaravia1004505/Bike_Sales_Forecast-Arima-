
# Packages
# install.packages("cropgrowdays")
# install.packages("lubridate")
# install.packages("xts")
# install.packages("tseries")
# install.packages("trend")
# install.packages("randomForest")
# install.packages("caret")
library(trend)
library(tseries)
library(lubridate)
library(cropgrowdays)
library(xts)
library(forecast)
library(randomForest)
library(caret)

# --------------------------------------------------
# Loading Data
# --------------------------------------------------

# 1 | instant | Integer | Sequential index of the record. Unique identifier for each observation.
# 2 | dteday | Date | Date corresponding to the day of measurement.
# 3 | season | Categorical | Season of the year: 1 = spring, 2 = summer, 3 = fall, 4 = winter.
# 4 | yr | Binary categorical | Year of the record: 0 = 2011, 1 = 2012.
# 5 | mnth | Categorical | Month of the year represented by values from 1 to 12.
# 6 | holiday | Binary | Indicates whether the day is a holiday: 0 = non-holiday, 1 = holiday.
# 7 | weekday | Categorical | Day of the week: 0 = Sunday, 1 = Monday, ..., 6 = Saturday.
# 8 | workingday | Binary | Indicates whether the day is a working day: 0 = weekend or holiday, 1 = working day.
# 9 | weathersit | Ordinal categorical | Weather condition: 1 = clear, 2 = partly cloudy, 3 = light rain/snow, 4 = heavy rain/snow/heavy fog.
# 10 | temp | Continuous | Normalized temperature in Celsius degrees divided by 41.
# 11 | atemp | Continuous | Normalized apparent temperature (feels-like temperature) divided by 50.
# 12 | hum | Continuous | Normalized relative humidity divided by 100.
# 13 | windspeed | Continuous | Normalized wind speed divided by 67.
# 14 | casual | Integer | Number of bike rentals made by casual or non-registered users.
# 15 | registered | Integer | Number of bike rentals made by registered users.
# 16 | cnt | Integer | Total number of rented bikes. Target variable of the model (cnt = casual + registered).

setwd("C:/Users/licja/Downloads/Bike Forecast/Data")
getwd()
data <- read.csv("bike_dataset.csv", header = TRUE)
dim(data)
names(data)
table(complete.cases(data)) # no NA's

# --------------------------------------------------
# Data Transformation
# --------------------------------------------------

# index, not needed
data <- data[,-1]
dim(data)

# - - - - - - - - - - - - - - - - - - - - - -

# dteday
table(nchar(data$dteday)) # al date have lenght 10
data[substr(data$dteday,6,10)=="02-29",] 
data <- data[-425,] # remove Feb 29
dim(data)
new_dataset <- as.data.frame(cbind(data$dteday))
names(new_dataset) <- c("dateday")
head(new_dataset)

# - - - - - - - - - - - - - - - - - - - - - -

# Day of the Year
Date <- as.Date(data$dteday)
DAY_YEAR <- day_of_year(as.Date(data$dteday), type = c("calendar"),return_year = FALSE,  base = NULL)
new_data <- as.data.frame(cbind(Date, DAY_YEAR))
new_data$Date <- as.Date(new_data$Date)
str(new_data$Date)
new_data$DAY_YEAR[(substr(as.character(new_data$Date),1,4) == "2012") & (new_data$DAY_YEAR >= 61)] = new_data$DAY_YEAR[(substr(as.character(new_data$Date),1,4) == "2012") & (new_data$DAY_YEAR >= 61)] - 1
#summary(new_data$DAY_YEAR[(substr(as.character(new_data$Date),1,4) == "2012")])
str(new_data)
rm(DAY_YEAR)
rm(Date)
head(new_data)
summary(new_data)
new_dataset <- cbind(new_dataset, new_data$DAY_YEAR)
names(new_dataset)[2] <- "day365"
head(new_dataset)
rm(new_data)

# - - - - - - - - - - - - - - - - - - - - - -

# month
N <- as.numeric(substr(as.character(data$dteday),6,7))
table(N)
length(N)
new_dataset <- cbind(new_dataset, N)
names(new_dataset)[3] <- "month"

# - - - - - - - - - - - - - - - - - - - - - -

# 16 | cnt | Total number of rented bikes. Target variable of the model (cnt = casual + registered).
# this will be the feature to predict - let's called "Y"
data$cnt
new_dataset <- cbind(new_dataset, data$cnt)
rm(data)
names(new_dataset)[4] <- "Y"
str(new_dataset)
new_dataset$dateday <- as.Date(new_dataset$dateday)
summary(new_dataset$dateday)

# --------------------------------------------------
# will add a month to the left
# --------------------------------------------------

summary(new_dataset$dateday)
table(new_dataset$dateday <= "2011-06-30")
X <- new_dataset[new_dataset$dateda <= "2011-06-30",]
dim(X)
X <- as.data.frame(cbind(
   substr(gsub("2011-","",as.character(X$dateday)),4,5)
  ,substr(gsub("2011-","",as.character(X$dateday)),1,2)
  ,X$Y))

head(X)
names(X) <- c("Day","Month","Qty")
X$Day <- as.integer(X$Day)
summary(X$Day)
X$Month <- as.integer(X$Month)
X$Qty <- as.numeric(as.character(X$Qty))

temp <- aggregate(X$Qty, by = list(X$Day), FUN = "mean")
rm(X)
head(temp)

temp2 <- as.data.frame(
  cbind(temp, seq(as.Date("2010-12-01"), as.Date("2010-12-31"), by = "day")))
dim(temp2)
head(temp2)
temp2 <- temp2[,-1]
round(temp2[,1],0)
rm(temp)
temp <- as.data.frame(cbind(temp2[,2],temp2[,1]))
temp$V1 <- as.Date(temp$V1)
temp$V2 <- round(temp$V2,0)
head(temp)
dim(temp)

aditional_month <- as.data.frame(
  cbind(
    temp$V1 # dateday 
    , seq(1, 31, by = 1) # day365
    , rep(12, 31) # month
    , temp$V2 # Y
  ))
rm(temp)
names(aditional_month) <- names(new_dataset)
head(aditional_month)
dim(new_dataset)
new_dataset <- rbind(aditional_month, new_dataset)
str(new_dataset)
new_dataset$dateday <- as.Date(new_dataset$dateday)
rm(aditional_month)

# --------------------------------------------------
# Prepare Train and Test
# Decompose and analyze data
# --------------------------------------------------

B <- !(new_dataset$dateday >= "2012-12-01")
table(B)
TRAIN <- new_dataset[B,]
dim(TRAIN)
TEST <- new_dataset[!B,]
dim(TEST)

train_ts <- ts(TRAIN$Y, frequency = 365, start = c(2010,12))
decom <- decompose(train_ts)
plot(decom)
save_plot1 <- recordPlot() # ts decomposition
# replayPlot(save_plot1)

# --------------------------------------------------
# Hyphotesis Test
# --------------------------------------------------

# --- Dickey Fuller --- 
# install.packages("forecast", repos = "https://cloud.r-project.org")
Dickey_Fuller <- round(adf.test(train_ts)$p.value,4)
# Ho = no estacionaria  Hi = si estacionaria
pruebas <- as.data.frame(cbind(
  "Dickey Fuller"
  , Dickey_Fuller
  , "Accept Ho"
  , "Data is not stationary"
))
names(pruebas) <- c("Test", "PValue", "Decisition", "Interpretation")
pruebas

# --- Mann-Kendall --- 
P <- round(mk.test(train_ts)$p.value,4)
temp <- as.data.frame(cbind(
  "Mann-Kendall"
  , P
  , "Accept Hi"
  , "Data have trend"
))
names(temp) <- c("Test", "PValue", "Decisition", "Interpretation")
temp

pruebas <- rbind(pruebas, temp)
names(pruebas)
pruebas

# --- Ljung-Box test ---
P <- Box.test(train_ts,
               lag = 30,
               type = "Ljung-Box")$p.value
temp <- as.data.frame(cbind(
  "Ljung-Box"
  , P
  , "Accept Hi"
  , "there is auto regression"
))
names(temp) <- c("Test", "PValue", "Decisition", "Interpretation")
temp

pruebas <- rbind(pruebas, temp)
names(pruebas)
pruebas

# --------------------------------------------------
# Choose best model; using Auto Arima
# --------------------------------------------------

pruebas
# Dickey Fuller P-Value of 0.1845, Mann-Kendall P-Value of 0, Ljung-Box P-Value of 0 
acf(train_ts, lag.max = 365) 
save_plot2 <- recordPlot() # ACF plot

modelo <- auto.arima(train_ts, seasonal = TRUE)
class(modelo)
modelo$arma
checkresiduals(modelo)

fcst <- as.data.frame(forecast(modelo, h = 31))
P <- fcst$`Point Forecast`
R <- TEST$Y
MAPE1 <- mean(abs((R - P) / R))
MAPE1

# replayPlot(save_plot1) # ts decomposition plot
# replayPlot(save_plot2) # ACF plot

# --------------------------------------------------
# Prepare monthly data
# --------------------------------------------------

D <- as.Date(paste(substr(as.character(new_dataset$dateday),1,7),"-01",sep=""))
summary(D)
new_dataset$dateday <- D
summary(new_dataset$dateday)

X <- aggregate(new_dataset$Y, by = list(new_dataset$dateday), FUN = "sum")
names(X) <- c("date","qty")
TRAIN <- X[-25,]
TEST <- X[25,]
summary(TRAIN$date)
summary(TEST$date)

train_ts <- ts(TRAIN$qty, frequency = 12, start = c(2010,12))
length(train_ts)
plot(train_ts)
D <- decompose(train_ts)
plot(D)

# --------------------------------------------------
# Run hyphotesis test
# --------------------------------------------------

adf.test(train_ts) # Augmented Dickey-Fuller Test
mk.test(train_ts) # Mann-Kendall trend test
lb_test <- Box.test( # Box-Ljung test
  train_ts,
  lag = 12,
  type = "Ljung-Box"
)

# --- Modelo 1: Auto Arima ---
model1 <- auto.arima(train_ts, stationary = TRUE)
# saveRDS(modelo, "modelo_arima.rds")
temp <- as.data.frame(forecast(model1))
P <- temp$`Point Forecast`[1]
R <- TEST$qty
abs((R - P) / R)

# --- Modelo 2: ETS ---
model2 <- ets(train_ts)
temp <- as.data.frame(forecast(model2))
P <- temp$`Point Forecast`[1]
R <- TEST$qty
abs((R - P) / R)

# --- Modelo 3: lm  regression ---
model3 <- lm(qty ~ date, data = TRAIN)
summary(model3)
P <- as.numeric(predict(model3, newdata = as.data.frame(TEST)))
R <- TEST$qty
abs((R - P) / R)

# --- Modelo 4: random forest ---
model4 <- randomForest::randomForest(
  qty ~ date
  , data = TRAIN
  , ntree = 20
)

P <- as.numeric(predict(model4, newdata = TEST))
R <- TEST$qty
abs((R - P) / R)
