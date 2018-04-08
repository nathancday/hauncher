#' ----
#' title: model
#' description: test seasonal models for prediction accuracy
#' author: nathancday@gmail.com
#' ---

source("hauncher/explore.R")

head(freq) # from 'hauncher/explore.R'
head(by_day) # from 'hauncher/weather.R

dat <- inner_join(freq, by_day, by = c("time" = "new_date"))

dat %<>% left_join(events)
dat$num_events %<>% ifelse(is.na(.), 0, .) %>% as.factor()

dat$day <- wday(dat$time, label = TRUE)
dat$week <- week(dat$time)

# explore a bit here changing color
ggplot(dat, aes(time, clients, fill = max_temp, color = day)) +
  geom_point(size = 3, shape = 21, stroke = 2) +
  scale_fill_viridis() +
  scale_color_brewer(palette = "Dark2")


ggplot(tail(dat, 40), aes(time, clients, fill = max_temp, color = day)) +
  geom_point(size = 3, shape = 21, stroke = 2) +
  scale_fill_viridis() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 day", date_labels = "%d")

### Learn with Clients --------------------------------------------------

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
mod1 <- xarima(train, clients ~ max_temp, stepwise = FALSE)

# custom forecast-like function for easy-use
fx <- function(mod, new_data, h = 1, ...) {
  xregs <- new_data[, colnames(mod$xreg) ]
  forecast(mod, h = h, xreg = xregs, ...)
}

accuracy(fx(mod, test), test[, "clients"])
accuracy(fx(mod1, test), test[, "clients"]) # slightly better


ets_mod <- ets(train[,"clients"])
ets_fc <- forecast(ets_mod)

accuracy(forecast(ets_mod), test[, "clients"])

aa_mod <- auto.arima(train[,"clients"])

forecast(aa_mod) %>% autoplot()

accuracy(forecast(aa_mod), test[, "clients"])

l <- BoxCox.lambda(train[,"clients"])
mod_bc <- xarima(train, clients ~ max_temp, lambda = l)
  
accuracy(fx(mod_bc, test), test[, "clients"])


# retest best with more predictors from explore.R
mod2 <- xarima(train, clients ~ max_temp + cond)
mod3 <- xarima(train, clients ~ max_temp + day) # new best
mod4 <- xarima(train, clients ~ max_temp + day + precip)
mod5 <- xarima(train, clients ~ max_temp + day + num_events)

accuracy(fx(mod, test), test[, "clients"]) # best from earlier
accuracy(fx(mod2, test), test[, "clients"]) 
accuracy(fx(mod3, test), test[, "clients"]) # better than best
accuracy(fx(mod4, test), test[, "clients"]) # not as good
accuracy(fx(mod3, test), test[, "clients"]) # not as good

mod3 <- xarima(train, clients ~ max_temp + day) # new best
mod3s <- xarima(train, clients ~ max_temp + day, step = FALSE) # same???

accuracy(fx(mod3, test), test[, "clients"])


modT <- tbats(train[,'clients'])
accuracy(forecast(modT), test[, "clients"])

# check on a bunch of next weeks

# custom function to help
mape <- function(...) { accuracy(...)[,"MAPE"][2] }
mape(fx(mod3, test), test[, "clients"]) # show it off

dat_cv <- tibble(week = 30:50) %>%
  mutate(train_dat = map(week, ~ window(dts, end = c(., 3))),
         test_dat = map(week, ~ window(dts, start = c(., 4), stop = c(., 3))),
         mod0 = map(train_dat, ~ auto.arima(.[,"clients"])),
         modA = map(train_dat, ~ xarima(., clients ~ max_temp + day)),
         acc0 = map2_dbl(mod0, test_dat, ~mape(forecast(..1, ..2), ..2[,"clients"])),
         accA = map2_dbl(modA, test_dat, ~mape(fx(..1, ..2), ..2[,"clients"])))


select(dat_cv, -train_dat, -test_dat) %>% with(plot(acc0, accA))

fx(mod3, test) %>% autoplot

### Sessions ------------------------------------------------------

# rinse and repeat, for session
head(dts)

dat_cvs <- tibble(week = 30:50) %>% # lol
  mutate(train_dat = map(week, ~ window(dts, end = c(., 3))),
         test_dat = map(week, ~ window(dts, start = c(., 4), stop = c(., 3))),
         mod0 = map(train_dat, ~ auto.arima(.[,"sessions"])),
         modA = map(train_dat, ~ xarima(., sessions ~ max_temp + day,)),
         acc0 = map2_dbl(mod0, test_dat, ~mape(forecast(..1, ..2), ..2[,"sessions"])),
         accA = map2_dbl(modA, test_dat, ~mape(fx(..1, ..2), ..2[,"sessions"])))

select(dat_cvs, -train_dat, -test_dat) %>% with(plot(acc0, accA))
# still better than auto.arima

### Usage --------------------------------------------------------------

# rework for usage b/c 4hr blocks

head(usage)
usage %<>% .[-1, ] # drop first obs from 2016
head(usage)

usagew <- inner_join(usage, by_4hour) # join weather data
usagew$kb <- usagew$total / (2^10) # convert B to kB :)
usagew$kb %<>% ifelse(.<.1, .1, .)

usagew$day <- wday(usagew$time, label = TRUE) # rebuild day labels
usagew$week <- week(usagew$time) # build similar val for week

# usage correlated to sessions
usagew %<>% inner_join(select(dat, time, sessions), by = c("new_date"="time"))

# split into test and train
test <- filter(usagew, between(time, as_datetime("2017-12-12 00:00:00"), max(time)))
train <- anti_join(usagew, test)
lil_train <- filter(train, between(time, as_datetime("2017-12-04 00:00:00"), max(time)) )


# usage  %<>% filter(complete.cases(.))
# 
# usage %<>% group_by(new_date) %>%
#   filter(n() == 6)

uts <- ts(train$kb, frequency = 6)
mts <- msts(train$kb, seasonal.periods = c(6,42))


uff <- fourier(uts, K = 1)
mff <- fourier(mts, K = c(3, 3))

mod <- auto.arima(uts, D = 1)

autoplot(forecast(mod, h = 42)) + 
  scale_x_continuous(limits = c(335,NA))

summary(mod)
map( 1:3, ~ auto.arima(uts, xreg = fourier(uts, K = .), seasonal = FALSE) %>% 
  summary()  )

nff <- fourier(uts, K = 3, h = 6*7)
autoplot(forecast(fmod, h = 42, xreg = nff)) + 
  scale_x_continuous(limits = c(335,NA))

mod2 <- auto.arima(mts, D = 1)
mod3 <- auto.arima(mts, D = 1, stepwise = FALSE, parallel = TRUE)

summary(mod2)
summary(mod3)

autoplot(forecast(mod3, h = 42)) + 
  scale_x_continuous(limits = c(45,NA))

fc <- function(mod, h = 2, ...){
  forecast(mod, h = h)[["mean"]] %>%
    as.numeric() %>%
    ifelse(. < 0, 0.1, .) }

fc(mod3, h = 45)
fc(mod)

accuracy(fc(mod3, h = 54), test$kb)
plot(fc(mod3, h = 54), test$kb)

mean( ( test$kb - fc(mod3, h = 54) ) / test$kb )* 100/54


ubd <- group_by(usagew, time = new_date) %>%
  summarise(kb = max(kb)) %>%
  inner_join(dat)

ubd %>% select(time, max_temp, kb, clients, sessions) %>%
  gather(k, v, -time, -max_temp) %>%
ggplot(aes(time, v, color = max_temp)) +
  geom_point() +
  facet_wrap(~k,scales = "free")


cor.test(ubd$kb, ubd$sessions) # more correlated
cor.test(ubd$kb, ubd$clients)




# Holiday effects -----
lil <- tail(ubd, 40)

select(lil, time, max_temp, kb, clients, sessions) %>%
  gather(k, v, -time, -max_temp) %>%
  ggplot(aes(time, v, color = max_temp)) +
  geom_point() +
  scale_x_date(date_breaks = "1 day", date_labels = "%d") +
  facet_wrap(~k, scales = "free", ncol = 1)

filter(ubd, kb == min(kb))

# use Thanksgiving day value for Xmas day
filter(ubd, time == as_date("2017-11-23"))




