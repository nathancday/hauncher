# Clean up raw character strings to proper dates
# nathancday@gmail.com

# start from here...
source("hauncher/read.R")


# clean names
names(clients) %<>% tolower() %>% gsub("# ", "", .)
names(sessions) %<>% tolower() %>% gsub("# ", "", .)
names(usage) %<>% tolower() %>% gsub(" .*", "", .)


# clean dates
clients$time %<>% as_date()
sessions$time %<>% as_date()

# usage has actual time values
usage$time %<>% as_datetime()

# drop download var from usage, contest is for total data usage
usage %<>% select(time, total)

