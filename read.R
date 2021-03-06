# Read in the data for Astrea ML competition
# samanthactoet@gmail.com

source("hauncher/install.R")

gs_auth() #authenticate with Google account

mysheets <- gs_ls() #list sheets in Google Drive

# Convert xlsx to Gsheet on Drive 

# get sheets of interest
mysheets %<>% filter(grepl("^Summary", sheet_title))

# Read in sessions sheets:
mysheets$sheet_title[2:1] %>% #careful about order
  map_df(~ gs_title(.) %>% gs_read(ws = "Number of sessions over time")) -> sessions

# Read in usage sheets:
mysheets$sheet_title[2:1] %>%
  map_df(~ gs_title(.) %>% gs_read(ws = "Usage over time")) -> usage

# Read in clients sheets:
mysheets$sheet_title[2:1] %>%
  map_df(~ gs_title(.) %>% gs_read(ws = "Clients per day")) -> clients

rm(mysheets)
