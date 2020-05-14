library(tseries)
library(forecast)
library(dplyr)

train = read.csv("C:\\Users\\hungu\\Downloads\\covid19-global-forecasting-week-2\\train.csv")
test = read.csv("C:\\Users\\hungu\\Downloads\\covid19-global-forecasting-week-2\\test.csv")

dates = c("2020-03-19","2020-03-20","2020-03-21","2020-03-22","2020-03-23","2020-03-24","2020-03-25",
          "2020-03-26")

for(date in dates){
  test_d1 = which(test$Date==date)
  train_d1 = which(train$Date==date)
  test[test_d1,'ConfirmedCases'] = train[train_d1,'ConfirmedCases']
  test[test_d1,'Fatalities'] = train[train_d1,'Fatalities']
}

train = train %>% mutate(Province_State=replace(Province_State, Province_State=="", NA))
unique(train$Province_State)
countries = as.character(unique(train$Country_Region))

preds = list()
i = 1

for(country in countries){
  print(country)
  c = train[train$Country_Region==country,]
  rows = dim(c)[1]
  if(rows > 65 || sum(is.na(c)) == 0){
    c = c %>% mutate(Province_State=replace(Province_State, is.na(Province_State), ""))
    states = as.character(unique(c$Province_State))
    for(state in states){
      c1 = c[c$Province_State==state,]
      rows = dim(c1)[1]
      data = ts(c1$ConfirmedCases, start=c(2020,1), end=c(2020,rows), frequency=rows)
      auto = auto.arima(data)
      ARMA = arima(data,order=c(auto$arma[1],auto$arma[6],auto$arma[2]))
      pred = predict(ARMA, n.ahead=43-8)
      preds[[i]] = round(pred$pred)
      i = i + 1
    }
  }
  else{
    data = ts(c$ConfirmedCases, start=c(2020,1), end=c(2020,rows), frequency=rows)
    auto = auto.arima(data)
    ARMA = arima(data,order=c(auto$arma[1],auto$arma[6],auto$arma[2]))
    pred = predict(ARMA, n.ahead=43-8)
    preds[[i]] = round(pred$pred)
    i = i + 1
  }
}

dataframe = data.frame(ConfirmedCases = c(t(t(data.frame(preds)))))

index = which(is.na(test$ConfirmedCases))
test[index,'ConfirmedCases'] = dataframe$ConfirmedCases

# Fatalities

preds = list()
i = 1

for(country in countries){
  print(country)
  c = train[train$Country_Region==country,]
  rows = dim(c)[1]
  if(rows > 65 || sum(is.na(c)) == 0){
    c = c %>% mutate(Province_State=replace(Province_State, is.na(Province_State), ""))
    states = as.character(unique(c$Province_State))
    for(state in states){
      c1 = c[c$Province_State==state,]
      rows = dim(c1)[1]
      if(all(c1$Fatalities==0)){
        preds[[i]] = c(1:35)*0
      }
      else{
        data = ts(c1$Fatalities, start=c(2020,1), end=c(2020,rows), frequency=rows)
        auto = auto.arima(data)
        ARMA = arima(data,order=c(auto$arma[1],auto$arma[6],auto$arma[2]))
        pred = predict(ARMA, n.ahead=43-8)
        preds[[i]] = round(pred$pred)
      }
      i = i + 1
    }
  }
  else{
    
    if(all(c$Fatalities==0)){
      preds[[i]] = c(1:35)*0
    }
    else{
      data = ts(c$Fatalities, start=c(2020,1), end=c(2020,rows), frequency=rows)
      auto = auto.arima(data)
      ARMA = arima(data,order=c(auto$arma[1],auto$arma[6],auto$arma[2]))
      pred = predict(ARMA, n.ahead=43-8)
      preds[[i]] = round(pred$pred)
    }
    i = i + 1
  }
}

dataframe = data.frame(Fatalities = c(t(t(data.frame(preds)))))

index = which(is.na(test$Fatalities))
test[index,'Fatalities'] = dataframe$Fatalities

submission = data.frame(test$ForecastId, as.integer(test$ConfirmedCases), as.integer(test$Fatalities))
names(submission) = c("ForecastId","ConfirmedCases","Fatalities")
write.csv(submission, "~/Desktop/Competition/COVID-19/submission_.csv", row.names = F)
