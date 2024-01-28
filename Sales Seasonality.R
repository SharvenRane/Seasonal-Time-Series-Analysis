library(tseries)
library(tidyverse)
library(ggplot2)
library(stats)
library(forecast)
library(TSA)
library(urca)
library(FinTS)
df_main = read_csv("C:/Users/sharv/Desktop/Time Series Project/Seasonal/monthlysales.csv",show_col_types = FALSE)
str(df_main)
colnames(df_main) = c('date','sales')

df_main$date <- as.Date(df_main$date, "%d-%m-%Y")
df_main$date
plot(df_main, main = 'original_dataset', type='l')

###############################################################################

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2019-12-01")
last_date = as.Date("2019-12-01")
########################

#for prediction purpose: ## just next 36 month
#############################

## for model fitting purpose
df <- df_main[df_main$date >= start_date & df_main$date < end_date,]

df_test <- df_main[df_main$date >= end_date & df_main$date < as.Date("2020-03-01"),]

data1 = df_main$sales
data_365 = ts(data1, frequency = 12)
decompose_data = decompose(data_365, "multiplicative")
plot(data1, type='l')
plot(decompose_data, type='l',lwd=2, col = 'blue')

########################################################

########################################################

data1 = df_main$sales
data1

## spikes are there at start

## Performing dickey Fuller Test

adf_test = adf.test(data1)
print(adf_test)

acf(data1, main = 'ACF of original data', lag.max = 20)
pacf(data1,main = 'PACF of original data', lag.max = 20 )

###################################################################
######################################################################

## full data with seasonal and first difference

close_sdiff = (diff(data1, lag = 12))
#plot(close_sdiff, type='l', main = 'seasonal log diff ')

adf_test = adf.test(close_sdiff)
adf_test


#################################################

## Model Determination:


## for the SARMA Model
acf(as.vector(close_sdiff), lag.max = 50, main = 'ACF for full data for P and Q')
pacf(as.vector(close_sdiff), lag.max = 50, main = 'PACF for full data for P and Q')

## from the ACF: its significant: so SMA(Q) = 0
## from the PACF: its significant: so SAR(P) = 0
# SARIMA (p,0,q)x(0,1,0) m = 12


######################################################


# object to store ARIMA summary in
model_summary <- data.frame()

# loop to store
for(p in 0:4){
  for(q in 0:4){
      fit <- Arima(df_main$sales, order = c(p,0,q), seasonal = list(order = c(0,1,0), period = 12))
      
      s <- shapiro.test(rstandard(fit))
      
      # H0: The model does not show lack of fit
      # H1: not H0
      lb <- LB.test(fit, lag = 10)
      
      # gather everything into a single data frame
      # AIC, BIC, SHAPIRO OF RESIDUALS, LB TEST
      acc_ext <- data.frame(# arima order
        p,
        d=0,
        q,
        # goodness of fit
        LJUNG = lb$p.value,
        SHAPIRO = s$p.value,
        AIC = AIC(fit),
        BIC = BIC(fit)
      )
      
      # add ARIMA summary
      model_summary <- rbind(model_summary, acc_ext)
  }
}

# show summary
filter(model_summary[order(model_summary$BIC, decreasing = FALSE),], LJUNG > 0.05)


best_fit <- Arima(df_main$sales, order =  c(1,0,1), seasonal = list(order = c(0,1,0), period = 12))

acf(rstandard(best_fit))
qqnorm(rstandard(best_fit))
qqline(rstandard(best_fit))

plot(forecast(best_fit, h = 36), main = 'Forecast from SARIMA (1,0,1)(0,1,0)')

