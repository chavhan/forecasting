################### forecasting on airline dataset ##################
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,'==') + 0)
head(X)
colnames(X) <- month.abb
airline_adv <- cbind(airlines,X)
head(airline_adv)
airline_adv['t'] <- c(1:96)
airline_adv['tsquare'] <- airline_adv$t*airline_adv$t
airline_adv['logPassengers'] <- log(airline_adv$Passengers)
names(airline_adv)
head(airline_adv)
train_data <- airline_adv[1:84,]
test_data <- airline_adv[85:96,]
dim(test_data)
test_data

attach(airline_adv)

####### Linear model #############
linear_model <- lm(airline_adv$Passengers~airline_adv$t,data = train_data)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model,test_data, interval='predict'))
linear_pred
#rmse_linear <- sqrt(mean((test_data$Passengers-linear_pred$fit)^2,na.rm = T))
#rmse_linear
rmse_linear <- RMSE(linear_pred$fit,test_data$Passengers,na.rm = F)
rmse_linear

################### Exponential Model ###########
expo_model <- lm(logPassengers~t,data=train_data)
expo_model
expo_pred <- data.frame(predict(expo_model,test_data,interval='predict'))
expo_pred
rmse_expo <- RMSE(exp(expo_pred$fit),test_data$Passengers,na.rm = F)
rmse_expo

################### Quadratic Model #############
quad_model <- lm(Passengers~t+tsquare,data = train_data)
quad_model
quad_pred <- data.frame(predict(quad_model,test_data,interval='predict'))
rmse_quad <- RMSE(quad_pred$fit,test_data$Passengers,na.rm = T)
rmse_quad
rmse_quad_na <- RMSE(quad_pred$fit,test_data$Passengers,na.rm = F)
rmse_quad_na
################# Addictive Seasonality Model ######################
sea_add_model <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_data)
sea_add_model
sea_add_pred <- data.frame(predict(sea_add_model,test_data,interval='predict'))
rmse_sea_add <- RMSE(sea_add_pred$fit,test_data$Passengers,na.rm = T)
rmse_sea_add
######################### Addictive Seasonality with Quadratic ##############
add_sea_quad_model <- lm(Passengers~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_data)
add_sea_quad_model
add_sea_quad_pred <- data.frame(predict(add_sea_quad_model,test_data,interval='predict'))
add_sea_quad_pred
rmse_add_sea_quad <- RMSE(add_sea_quad_pred$fit,test_data$Passengers,na.rm = T)
rmse_add_sea_quad
######################### Multiplicative Seasonality #############
multi_sea_model <- lm(logPassengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train_data)
multi_sea_model
multi_sea_pred <- data.frame(predict(multi_sea_model,test_data,interval='predict'))
rmse_multi_sea <- RMSE(exp(multi_sea_pred$fit),test_data$Passengers,na.rm = T)
rmse_multi_sea
################## Total Result #######################
total_res <- data.frame(c('rmse_linear','rmse_expo','rmse_quad','rmse_sea_add','rmse_add_sea_quad','rmse_multi_sea','rmse_add_sea_quad_final'),
                        c(rmse_linear,rmse_expo,rmse_quad,rmse_sea_add,rmse_add_sea_quad,rmse_multi_sea,rmse_add_sea_quad_final)
)
names(total_res) <- c('Model','Error')
View(total_res)
total_res

############# Applying Addictive seasonal Quad model on entire dataset #########
add_sea_quad_model <-lm(Passengers~t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = airline_adv)
add_sea_quad_model
add_sea_quad_pred <- data.frame(predict(add_sea_quad_model,airline_adv,interval='predict'))
rmse_add_sea_quad_final <- RMSE(add_sea_quad_pred$fit,airline_adv$Passengers,na.rm = T)
rmse_add_sea_quad_final

plot(rmse_add_sea_quad_final)
acf(add_sea_quad_model$residuals,lag.max = 10)

A <- arima(add_sea_quad_model$residuals,order = c(2,0,0))
A$residuals
acf(A$residuals,lag.max = 10)

################## predicting next 12 months errors ###################
library(forecast)
error_12 <- data.frame(forecast(A,n=12))
View(error_12)
future_error <- error_12$Point.Forecast
head(future_error)

pred_new_value_12 <- add_sea_quad_pred + future_error
pred_new_value_12

write.csv(pred_new_value_12,file = 'airling_forecast_12.csv',row.names = F)
getwd()

