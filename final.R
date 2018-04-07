#' ----
#' title: final
#' description: build the 3 models and export forecasts as csv
#' author: nathancday@gmail.com
#' ---

source("hauncher/explore.R")


### Set Up ---------------

dat <- inner_join(freq, by_day, by = c("time" = "new_date"))
dat$day <- wday(dat$time, label = TRUE)
dat$week <- week(dat$time)

dts <- ts(dat, frequency = 7)


# custom arima wrapper
xarima <- function(data, formula, ...) {
  
  form <- as.formula(formula) %>% as.character()
  
  x_strng <- strsplit(form[3], " ?\\+ ?") %>% unlist()
  xregs <- data[, x_strng] %>%
    as.matrix() %>%
    set_colnames(x_strng)
  
  response <- data[ , form[2]]
  
  auto.arima(response, ..., xreg = xregs)
}

# custom forecast wrapper
fx <- function(mod, new_data, h = 1, ...) {
  xregs <- new_data[, colnames(mod$xreg) ]
  forecast(mod, h = h, xreg = xregs, ...)
}

test_df <- filter(by_day, new_date > as_date("2017-12-20"), new_date < as_date("2017-12-28")) %>%
  mutate(day = wday(new_date, label = TRUE))

test <- test_df %>%
  ts(frequency = 7)

test_df$new_date

### Clients ---------------
mod_clients <- xarima(dts, clients ~ max_temp + day)

fc_clients <- tibble(Daytime = test_df$new_date,
                     Prediction = as.numeric(fx(mod_clients, test)$mean))
# write.csv(fc_clients, "hauncher/predictions/clients.csv")

### Sessions ---------------
mod_session <- xarima(dts, sessions ~ max_temp + day)

fc_session <- tibble(Daytime = test_df$new_date,
                     Prediction = as.numeric(fx(mod_session, test)$mean))
# write.csv(fc_session, "hauncher/predictions/sessions.csv")

### Usage ---------------

usagew <- inner_join(usage, by_4hour)
usagew$kb <- usagew$total / (2^10)

test2 <- anti_join(by_4hour, usagew) %>%
  filter(time > as_date("2017-12-21"), time < as_date("2017-12-28"))

uts <- msts(usagew$kb, seasonal.periods =  c(6, 42))

mod <- auto.arima(uts, D = 1, stepwise = FALSE, parallel = TRUE)

fc_usage <- tibble(Daytime = test2$time,
                     Prediction = as.numeric(forecast(mod, h = 42)$mean))
fc_usage$Prediction %<>% ifelse(. < 0, 0, .)

write.csv(fc_usage, "hauncher/predictions/usage.csv")


