# Import external weather data about Charlottesville
# samanthactoet@gmail.com



install.packages("rwunderground")

# Documentation: https://cran.r-project.org/web/packages/rwunderground/rwunderground.pdf

key <- rwunderground::set_api_key("98424ba247eed14b")

location <- rwunderground::set_location(zip_code = "22902")

weather <- rwunderground::history_range(location, date_start = "20170101", date_end = "20171227")





