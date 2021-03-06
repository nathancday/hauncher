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

# ggplot(weather, aes(date, temp)) +
#   geom_point(alpha = .1)

# summarize temps, precip, and condition by day:
weather$new_date <- as.Date(weather$date)

daily_condition <- function(day) {
  names(sort(table(day))[length(sort(table(day)))])
}

by_day <- weather %>% group_by(new_date) %>% 
  summarise(max_temp = max(temp, na.rm = TRUE), 
            min_temp = min(temp, na.rm = TRUE), 
            precip = sum(precip, na.rm = TRUE),
            cond = daily_condition(cond))

# ggplot(by_day, aes(new_date, max_temp, col = cond)) +
#   geom_point()

### Recode by_day$cond
#  might want to move this earlier ^^^ later

# collapse rain and show into Heavy/Light Precip
by_day$cond %<>% gsub("((Freezing)? Rain| Snow)", " Precip", .) %>% gsub("  ", " ", .)

#collapse Mostly cloudy as overcase
by_day$cond %<>% gsub("Mostly Cloudy", "Overcast", .)

# collapse other clouds as clean
by_day$cond %<>% gsub(".* Cloud.*", "Clear", .)

# drop Heavy/Light designation for Precip
by_day$cond %<>% gsub(".* ", "", .)


# ggplot(by_day, aes(new_date, precip, col = cond)) +
#   geom_point() # recode based on precipitation

### Recode by by_day$precip

# hist(by_day$precip, breaks = 15)

by_day %<>% mutate(cond = case_when(precip > .1 ~ "Precip",
                                    precip > 0 ~ "Overcast",
                                    TRUE ~ as.character(cond)))


# make sensible ordered factor
by_day$cond %<>% as.ordered()

# table(by_day$cond)

# saveRDS(by_day, "hauncher/data/weather_by_day.RDS")


### Repeat for 4 hour intervals --------------


# re-summariese using day and 4 hour chunks
weather$hour <- hour(weather$date)

filter(weather, is.na(hour))

chunk_dc <- rep(1:6, each = 4) %>% set_names(0:23)

weather$chunk <- chunk_dc[weather$hour + 1]


by_4hour <- group_by(weather, new_date, chunk) %>%
  summarise(max_temp = max(temp, na.rm = TRUE), 
            min_temp = min(temp, na.rm = TRUE), 
            precip = sum(precip, na.rm = TRUE),
            cond = daily_condition(cond))

chunk_rc <- seq(2,22, 4) %>% set_names(1:6)

by_4hour$hour <- chunk_rc[by_4hour$chunk]
table(by_4hour$hour)

by_4hour$time <- as_datetime(paste0(by_4hour$new_date," ", by_4hour$hour, ":00:00" ))



