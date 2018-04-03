# Import external weather data about Charlottesville
# samanthactoet@gmail.com

source("hauncher/clean.R")

# Documentation: https://cran.r-project.org/web/packages/rwunderground/rwunderground.pdf

key <- set_api_key("98424ba247eed14b")

location <- set_location(zip_code = "22902")

# scrape data from API:
weather <- history_range(location, date_start = "20170101", date_end = "20171227")

# to save read in time saveRDS(weather, "hauncher/data/weather2017.csv")

weather <- readRDS("hauncher/data/weather2017.csv")

ggplot(weather, aes(date, temp)) +
  geom_point(alpha = .1)

# summarize temps, precip, and condition by day:
weather$new_date <- as.Date(weather$date)

daily_condition <- function(day) {
  names(sort(table(day))[length(sort(table(day)))])
}

summarized <- weather %>% group_by(new_date) %>% 
  summarise(max_temp = max(temp), 
            min_temp = min(temp), 
            precip = sum(precip, na.rm = T),
            cond = daily_condition(cond))

# visualize:
ggplot(summarized, aes(new_date, max_temp, col = cond)) +
  geom_point()







