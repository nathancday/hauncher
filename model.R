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

# check on a bunch of next weeks

# custom function to help
mape <- function(...) { accuracy(...)[,"MAPE"][1] }
mape(fx(mod3, test), test[, "clients"]) # show it off

dat_cv <- tibble(week = 30:50) %>%
  mutate(train_dat = map(week, ~ window(dts, end = c(., 3))),
         test_dat = map(week, ~ window(dts, start = c(., 4), stop = c(., 3))),
         mod0 = map(train_dat, ~ auto.arima(.[,"clients"])),
         modA = map(train_dat, ~ xarima(., clients ~ max_temp + day)),
         acc0 = map2_dbl(mod0, test_dat, ~mape(forecast(..1, ..2), ..2[,"clients"])),
         accA = map2_dbl(modA, test_dat, ~mape(fx(..1, ..2), ..2[,"clients"])))


select(dat_cv, -train_dat, -test_dat) %>% with(plot(acc0, accA))
