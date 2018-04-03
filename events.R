#'---
#' title: events.R
#' subtitle: Get a curated event history Cville in 2017
#' author: nathancday@gmail.com
#' ---

source("hauncher/install.R")

# see read.R for better comments

gs_auth()

mysheets <- gs_ls()

events_sht <- mysheets$sheet_title[1] %>%
  gs_title()
  
events <- gs_ws_ls(events_sht) %>%
  map( ~ gs_read(events_sht, ws = .)[-1,]) %>%
  set_names(gs_ws_ls(events_sht))


# clean Football
events[["Football"]] %<>% set_names("time") %>%
  mutate(time = as_date(time) )

# clean Jefferson
events[["Jefferson"]] %<>% set_names("time") %>%
  separate(time, c("time", "show"), sep = "2017 ") %>%
  mutate(time = trimws(time) %>% gsub("^.*, ", "", .) %>%
           paste0(., " 2017") %>% mdy(),
         show = gsub(".*(Present(s)?\\:?|welcomes) ", "", show, ignore.case = TRUE))

# clean Pavilion (the hardest for last)
show_index = c(F,T,F,F)
time_index <- c(T,F,F,F)

events[["Pavillion"]] <- tibble(time = events[["Pavillion"]][[1]][time_index],
                                show = events[["Pavillion"]][[1]][show_index]) %>%
  mutate(time = gsub(".*day", "", time) %>% trimws() %>% dmy())


events <- bind_rows(events, .id = "venue")

### sum events by day
# might want to weight this

events %<>% count(time)
events %<>% rename(num_events = n)


saveRDS(events, "hauncher/data/events.RDS")

