#' ----
#' title: model.R
#' author: Nathan Day
#' ---

source("hauncher/explore.R")
source("hauncher/weather.R")

head(freq) # from 'hauncher/explore.R'
head(summarized) # from 'hauncher/weather.R

dat <- inner_join(freq, by_day, by = c("time" = "new_date"))
dat$day <- wday(dat$time, label = TRUE)

ggplot(dat, aes(time, clients, fill = max_temp, color = day)) +
  geom_point(size = 3, shape = 21, stroke = 2) +
  scale_fill_viridis() +
  scale_color_brewer(palette = "Dark2")

ggplot(dat, aes(max_temp, clients)) +
  geom_point()

ggplot(dat, aes(min_temp, clients)) +
  geom_point()

dat %<>% rowwise() %>% mutate(mean_temp = sum(max_temp, min_temp) / 2)

ggplot(dat, aes(mean_temp, clients)) +
  geom_point() +
  stat_smooth()

dts <- ts(dat, frequency = 7) # losing cond; need to convert to a factor

# a wraper for auto.arima for using formula syntax for xreg 
xarima <- function(data, formula, ...) {
  
  form <- as.formula(formula) %>% as.character()
  
  x_strng <- strsplit(form[3], " ?\\+ ?") %>% unlist()
  
  xregs <- data[, x_strng] %>%
    as.matrix() %>%
    set_colnames(x_strng)

  response <- data[ , form[2]]

  auto.arima(response, ..., xreg = xregs)
  
}

mod <- xarima(train, clients ~ max_temp)

fx <- function(mod, new_data, h = 1, ...) {
  xregs <- new_data[, colnames(mod$xreg) ]
  forecast(mod, h = h, xreg = xregs, ...)
}

fx(mod, test)

# split for training
train <- window(dts, end = c(50,3))
test <- window(dts, start = c(50, 4))

xarima(train, clients ~ max_temp) %>%
  forecast(1) %>%
  accuracy(dts)

autoplot(fx(mod, test))

accuracy(fx(mod, test), test[, "clients"])

ets_mod <- ets(train[,"clients"])
ets_fc <- forecast(ets_mod)

accuracy(forecast(ets_mod), test[, "clients"])

aa_mod <- auto.arima(train[,"clients"])

accuracy(forecast(aa_mod), test[, "clients"])

l <- BoxCox.lambda(train[,"clients"])
mod_bc <- xarima(train, clients ~ max_temp, lambda = l)
  
accuracy(fx(mod_bc, test), test[, "clients"])




