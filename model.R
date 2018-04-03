#' ----
#' title: model.R
#' author: Nathan Day
#' ---

source("hauncher/explore.R")
source("hauncher/weather.R")
source("hauncher/events.R")

head(freq) # from 'hauncher/explore.R'
head(by_day) # from 'hauncher/weather.R

dat <- inner_join(freq, by_day, by = c("time" = "new_date"))

dat %<>% left_join(events)
dat$num_events %<>% ifelse(is.na(.), 0, .) %>% as.factor()

dat$day <- wday(dat$time, label = TRUE)

# explore a bit here changing color
ggplot(dat, aes(time, clients, fill = max_temp, color = day)) +
  geom_point(size = 3, shape = 21, stroke = 2) +
  scale_fill_viridis() +
  scale_color_brewer(palette = "Dark2")

### Prep for training --------------------------------------------------

dts <- ts(dat, frequency = 7) # formal ts object

# split for accuracy benchmarks
train <- window(dts, end = c(50,3))
test <- window(dts, start = c(50, 4))

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

# custom forecast-like function for easy-use
fx <- function(mod, new_data, h = 1, ...) {
  xregs <- new_data[, colnames(mod$xreg) ]
  forecast(mod, h = h, xreg = xregs, ...)
}

fx(mod, test)


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




