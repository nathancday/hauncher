# build time series objects for clients, sessions an usage
# nathancday@gmail.com

source("hauncher/clean.R")

p_load(forecast)


client_ts <- ts(clients$clients)

autoplot(client_ts)
ggAcf(client_ts)

autoplot(diff(client_ts))
ggAcf(diff(client_ts))

Box.test(diff(client_ts), lag = 10, "L")
