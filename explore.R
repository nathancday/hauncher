# Exploratory vizualizations
# nathancday@gmail.com

source("hauncher/clean.R")

p_load(cowplot)

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

impt_mod <- lm(sessions ~ clients, data = not_missing)
summary(impt_mod)

new_data <- anti_join(freq, not_missing) %>% # sorts descending for some reason
  arrange(time) %>% # so reorient
  mutate(sessions = predict(impt_mod, .),
         norm_ses = sessions / clients)

freq <- bind_rows(new_data, not_missing)


ggplot(freq, aes(clients, sessions)) +
  geom_point()

new_mod <- lm(sessions ~ clients, data = freq)
summary(new_mod)

freq %<>% select(-norm_ses)



