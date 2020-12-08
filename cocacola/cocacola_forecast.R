####################### Foresting on Cocacola dataset ##############
library(forecast)
install.packages("smooth")
library(smooth)
library(tseries)

timeSale <- ts(CocaCola_Sales_Rawdata$Sales, frequency = 4)  ## converting data into time object
View(timeSale)
dim(timeSale)
str(timeSale)
train_data <- timeSale[1:38]
train_data
test_data <- timeSale[39:42]
plot(timeSale)
dim(train_data)
train_data <- ts(train_data,frequency = 4)
test_data <- ts(test_data,frequency = 4)
hw_object <- HoltWinters(train_data, alpha = .2, beta = F, gamma = F)
hw_object
hw_pred <- data.frame(predict(hw_object,n.ahead = 4))
hw_pred
plot(forecast(hw_object,h=4))
error1 <- MAPE(hw_pred$fit,test_data)*100
error1
hw_lts_object <- HoltWinters(train_data,alpha = .2, beta = .15, gamma = 0.05)
hw_lts_object
hw_lts_pred <- data.frame(predict(hw_lts_object,n.ahead = 4))
plot(forecast(hw_lts_object,h=4))
error2 <- MAPE(hw_lts_pred$fit,test_data)*100
error2

hw_ts <- HoltWinters(train_data,beta = F,gamma = F)
plot(forecast(hw_ts,h=4))
hw_ts_pred <- data.frame(predict(hw_ts,n.ahead = 4))
(error3 <- MAPE(hw_ts_pred$fit,test_data)*100)

hw_s <- HoltWinters(train_data,gamma = F)
plot(forecast(hw_s,h=4))
hw_s_pred <- data.frame(predict(hw_s,n.ahead = 4))
(error4 <- MAPE(hw_s_pred$fit,test_data)*100)

hw <- HoltWinters(train_data)
hw
plot(forecast(hw,h=4))
hw_pred <- data.frame(predict(hw,n.ahead = 4))
(erro5 <- MAPE(hw_pred$fit,test_data)*100)

df_mape<-data.frame(c("alpha=.5,bita=F,gamma=F","alpha=.2,bita=.15,gamma=.05","bita=F,gamma=F",
                      "gamma=F","no values given"),
                    c(error1,error2,error3,error4,erro5))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)