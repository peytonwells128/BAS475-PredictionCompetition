library(fpp3)
library(parallel)
library(doParallel)


credit <- read.csv("credit.csv")

credit$Period <- yearmonth(credit$Period)
# Convert to tsibble
credit <- tsibble(credit)


class(credit)

#transformation

lambda <- credit %>% features(ï..credit_in_millions, features = guerrero) %>% pull(lambda_guerrero)
lambda
credit %>% autoplot(box_cox(ï..credit_in_millions, lambda))

credit %>% autoplot()

#data split into training and holdout

train <- credit[1:480,]

holdout <- credit [481:492,]


# ALL MODELS FIT AND TESTED VIA Cross Validation

cluster <- makeCluster(detectCores() - 1) #Line 1 for parallelization
registerDoParallel(cluster) #Line 2 for parallelization

credit %>% stretch_tsibble(.init = 48, .step = 24) %>%
  model(NNET = NNETAR(ï..credit_in_millions),
        arima210 = ARIMA(ï..credit_in_millions ~ pdq(2,1,0)),
        arima013 = ARIMA(ï..credit_in_millions ~ pdq(0,1,3)),
        stepwise = ARIMA(ï..credit_in_millions),
        search = ARIMA(ï..credit_in_millions, stepwise=FALSE),
        ExponentialSmoothing = ETS(ï..credit_in_millions ~ error("A") + trend("N") + season("N"))) -> fit3

fit3 %>% forecast(h = 12) %>% accuracy(credit)

stopCluster(cluster) #Line 3 for parallelization






#NNET

fit <- train %>%
  model(NNET = NNETAR(ï..credit_in_millions),
        arima210 = ARIMA(ï..credit_in_millions ~ pdq(2,1,0)),
        arima013 = ARIMA(ï..credit_in_millions ~ pdq(0,1,3)),
        stepwise = ARIMA(ï..credit_in_millions),
        search = ARIMA(ï..credit_in_millions, stepwise=FALSE))
fit %>%
  forecast(h = 12) %>% accuracy(credit)

credit_fc <- fit %>%
  forecast(h=12)

accuracy(fit, credit)

#Cross Validation

cluster <- makeCluster(detectCores() - 1) #Line 1 for parallelization
registerDoParallel(cluster) #Line 2 for parallelization

train %>% stretch_tsibble(.init = 48, .step = 24) %>%
  model(NNET = NNETAR(ï..credit_in_millions),
        arima210 = ARIMA(ï..credit_in_millions ~ pdq(2,1,0)),
        arima013 = ARIMA(ï..credit_in_millions ~ pdq(0,1,3)),
        stepwise = ARIMA(ï..credit_in_millions),
        search = ARIMA(ï..credit_in_millions, stepwise=FALSE),
        ExponentialSmoothing = ETS(ï..credit_in_millions ~ error("A") + trend("N") + season("N"))) -> fit3

fit3 %>% forecast(h = 12) %>% accuracy(credit)

stopCluster(cluster) #Line 3 for parallelization

#Estimating just RMSE using created function

rmse <- function(y_actual, y_pred) {
  sqrt(mean((y_actual - y_pred)^2))
}



#Estimating just MAPE using created function

mape <- function(y_actual, y_pred) {
  mean(abs(y_actual - y_pred) / y_actual)
}


## EVERYTHING BELOW THIS LINE IS SCRATCH WORK 



#ARIMAs

fits <- credit %>%
  model(arima210 = ARIMA(ï..credit_in_millions ~ pdq(2,1,0)),
        arima013 = ARIMA(ï..credit_in_millions ~ pdq(0,1,3)),
        stepwise = ARIMA(ï..credit_in_millions),
        search = ARIMA(ï..credit_in_millions, stepwise=FALSE))
glance(fits)

fits %>% select(stepwise) %>%
  forecast(h = 12) %>%
  autoplot(credit)

arima_fc <- fits %>% forecast(h=12)

accuracy(fit, holdout)

#Exponential Smoothing

fit2 <- credit %>%
  model(ETS(ï..credit_in_millions ~ error("A") + trend("N") + season("N")))

glance(fit2)


#Basic Models - 


google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )