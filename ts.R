#' ----
#' title: timeseries
#' description: build ts objects from raw data via library(forecast)
#' author: nathan day
#' ---

source("hauncher/explore.R")

client_ts <- ts(clients$clients)
cts <- ts(clients$clients, 
          freq = 7)

autoplot(client_ts)
ggAcf(client_ts)

autoplot(diff(client_ts))
ggAcf(diff(client_ts))

Box.test(diff(client_ts), lag = 1, "L")

naive(client_ts) %>% checkresiduals()

## Split into test/train -----------------------------------------------------

# hold out last week
train <- client_ts %>% subset(end = 346)

naive_fc <- naive(train, 7)
plot(naive_fc) # uses the last value, repeats 7x

mean_fc <- meanf(train, h = 7) 
plot(mean_fc) # uses the overal mean

accuracy(naive_fc, client_ts) # ok
accuracy(mean_fc, client_ts) # sucks

# Time-Series CV ----------------------------------------------------
e <- matrix(NA_real_, nrow = 353, ncol = 7) # build; rows = length(ts), cols = length(h)

for (h in 1:7) { e[, h] <- tsCV(client_ts, forecastfunction = naive, h = h) } # populate

tibble( h = 1:7,
        mse = colMeans(e^2, na.rm = T) ) %>% # Compute the MSE values and remove missing values
  ggplot(aes(h, mse)) +
  geom_point()

# Exponential Smooths --------------------------------------------------
fc <- ses(client_ts, 7)

summary(fc)
autoplot(fc) +
  autolayer(fitted(fc))

# Holt smooth
fc <- holt(client_ts, 7)

summary(fc)
autoplot(fc) +
  autolayer(fitted(fc))


train <- cts %>%
  subset(end = length(.) - 7)
fc <- hw(train, h = 7)
autoplot(fc)

accuracy(fc, cts)

### ETS for automatic model selection
fc <- ets(train)
checkresiduals(fc)
accuracy(forecast(fc, h = 7), cts)
autoplot(forecast(fc))

## auto.arima
fc <- auto.arima(train)
checkresiduals(fc)
autoplot(forecast(fc))
accuracy(forecast(fc, h = 7), cts)

## box.cox transformation
lmba <- BoxCox.lambda(train)
bc_fc <- auto.arima(train, lambda = lmba)
accuracy(forecast(bc_fc, h = 7), cts)

ets_fc <- ets(train, lambda = lmba)
accuracy(forecast(ets_fc, h = 7), cts)

### Look at 4 hour intervals ----------------------------------------------------

usage %<>% .[-1,] # first row in 2016 and empty
usage$tod <- hour(usage$time)
usage$day <- wday(usage$time, label = T)

group_by(usage, day, tod) %>%
  summarise(avg = mean(total)) %>%
ggplot(aes(day, avg, fill = as.factor(tod))) +
 geom_col()

uts <- ts(usage$total, freq = 6)

train <- window(uts, end = c(347,3))
test <- window(uts, start = c(347,4))

autoplot(train)

ggAcf(train)

ets_mod <- ets(train, damped = TRUE)
autoplot(ets_mod)
checkresiduals(ets_mod)

fc <- forecast(ets_mod, 42)
autoplot(fc)
accuracy(fc, test)


aa_mod <- auto.arima(train, lambda = .1)
autoplot(aa_mod)
checkresiduals(aa_mod)

fc <- forecast(aa_mod, 42)
autoplot(fc)
accuracy(fc, test)

autoplot(test)


hist(test)

train


