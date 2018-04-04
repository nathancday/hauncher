#' ----
#' title: clean
#' description: date parsing plus nice names
#' author: nathancday@gmail.com
#' ---

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

