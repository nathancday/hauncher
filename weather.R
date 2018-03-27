# Import external weather data about Charlottesville
# samanthactoet@gmail.com

source("hauncher/clean.R")



p_load(rwunderground)

# Documentation: https://cran.r-project.org/web/packages/rwunderground/rwunderground.pdf

key <- set_api_key("98424ba247eed14b")

location <- set_location(zip_code = "22902")

# save the scrape time and use the "hauncher/data/weather2017.csv"
weather <- history_range(location, date_start = "20170101", date_end = "20171227")

# saveRDS(weather, "hauncher/data/weather2017.csv")

ggplot(weather, aes(date, temp)) +
  geom_point(alpha = .1)


