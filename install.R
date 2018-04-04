#' ----
#' title: clean
#' description: all require container mods and R libraries
#' author: samanthactoet@gmail.com, nathancday@gmail.com
#' ---

## Required bash code
# docker run -d -p 8787:8787-v /Users/on_osx/Desktop/hauncher:/home/rstudio/hauncher -e ROOT=TRUE rocker/rstudio

## System library installs in the Rstudio terminal
# apt-get update
# apt-get install zlib1g-dev
# apt-get install libxml2-dev

# install.packages("pacman")
library(pacman)

p_load(forecast, tidyverse, magrittr, googlesheets,
       cowplot, lubridate, rwunderground, viridis,
       mgcv) # ongoing list
