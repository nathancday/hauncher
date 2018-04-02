# Import external weather data about Charlottesville
# samanthactoet@gmail.com

source("hauncher/clean.R")

# Documentation: https://cran.r-project.org/web/packages/rwunderground/rwunderground.pdf

# scrape data from API:

# ( commented to save time )

# key <- set_api_key("98424ba247eed14b")
# location <- set_location(zip_code = "22902")
# weather <- history_range(location, date_start = "20170101", date_end = "20171227")
# saveRDS(weather, "hauncher/data/weather.RDS")

weather <- readRDS("hauncher/data/weather.RDS")

ggplot(weather, aes(date, temp)) +
  geom_point(alpha = .1)

# summarize temps, precip, and condition by day:
weather$new_date <- as.Date(weather$date)

daily_condition <- function(day) {
  names(sort(table(day))[length(sort(table(day)))])
}

lil_weather <- weather %>% group_by(new_date) %>% 
  summarise(max_temp = max(temp), 
            min_temp = min(temp), 
            precip = sum(precip, na.rm = T),
            cond = daily_condition(cond))

# visualize:
ggplot(lil_weather, aes(new_date, max_temp, col = cond)) +
  geom_point()

ggplot(lil_weather, aes(new_date, precip, col = cond)) +
  geom_point()



