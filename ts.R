# build time series objects for clients, sessions an usage
# nathancday@gmail.com

source("hauncher/clean.R")

p_load(forecast)

ggplot(clients, aes(time, clients)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month")

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

