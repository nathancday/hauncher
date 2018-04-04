#' ----
#' title: explore
#' description: combine accessory data and hunt for correlation
#' author: nathancday@gmail.com, samanthactoet@gmail.com
#' ---

source("hauncher/clean.R")
source("hauncher/weather.R")
source("hauncher/events.R")

a12 <- as_date("2017-08-12")


# clients
p1 <- ggplot(clients, aes(time, clients)) +
  geom_point(aes(color = time == a12)) +
  stat_smooth()

# sesssions
p2 <- ggplot(sessions, aes(time, sessions)) +
  geom_point(aes(color = time == a12)) +
  stat_smooth()

# usage
p3 <- ggplot(usage, aes(time, total)) +
  geom_point(aes(color = time == a12), alpha = .1) +
  stat_smooth()
 
plot_grid(p1, p2, p3) # missing January, 2017 data for # sessions

# group clients and sessions
freq <- inner_join(clients, sessions) %>%
  mutate(norm_ses = sessions / clients)

ggplot(freq, aes(time, norm_ses)) +
  geom_point() +
  geom_smooth()

ggplot(freq, aes(clients, sessions)) +
  geom_point()

# Impute missing month in Sessions via a model

filter(freq, sessions == 0) %>% tail() # goes until Feb 1st

not_missing <- filter(freq, time > as_date("2017-02-01"))

impute_mod <- lm(sessions ~ clients, data = not_missing)
summary(impute_mod)

new_data <- anti_join(freq, not_missing) %>% # sorts descending for some reason
  arrange(time) %>% # so reorient
  mutate(sessions = predict(impute_mod, .),
         norm_ses = sessions / clients)

freq <- bind_rows(new_data, not_missing) # putting it back together again


ggplot(freq, aes(clients, sessions)) +
  geom_point()

new_mod <- lm(sessions ~ clients, data = freq)
summary(new_mod)

freq %<>% select(-norm_ses)

# join in weather data:
dat <- inner_join(freq, by_day, by = c("time" = "new_date"))
dat$day <- wday(dat$time, label = TRUE)

# visualize:
ggplot(dat, aes(time, clients, col = cond)) +
  geom_point()

# test if condition is useful predictor:
m0 <- lm(clients ~ cond, data = dat)
summary(m0) 

# now try precipitation:
ggplot(dat, aes(time, clients, col = log(precip + 1))) +
  geom_point(size = 3) +
  scale_color_viridis()

m0 <- lm(clients ~ log(precip + 1), data = dat)
summary(m0)

# max temp:
ggplot(dat, aes(time, clients, col = max_temp)) +
  geom_point(size = 3) +
  scale_color_viridis()

m0 <- lm(clients ~ max_temp, data = dat)
summary(m0)

ggplot(dat, aes(clients, max_temp)) +
  geom_point()

# day of the week:
ggplot(dat, aes(time, clients, col = day)) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = T)

m0 <- lm(clients ~ day, data = dat)
summary(m0)

# looks like max temp is our best option

m0 <- lm(clients ~ max_temp, data = dat)
ma <- lm(clients ~ max_temp + day, data = dat)
ma2 <- lm(clients ~ max_temp + day + cond, data = dat) # precip's not worth it


anova(m0, ma, ma2)
