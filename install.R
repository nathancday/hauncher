#' ----
#' title: clean
#' description: all require container mods and R libraries
#' author: samanthactoet@gmail.com
#' ---


## Required bash code

# docker ps # find the ID of the running container you want to add a package to
# a docker command to start a bash shell in your container
# apt-get update
# apt-get install zlib1g-dev
# apt-get install libxml2-dev

# install.packages("pacman")
library(pacman)

p_load(forecast, tidyverse, magrittr, googlesheets,
       cowplot, lubridate, rwunderground, viridis) # ongoing list
