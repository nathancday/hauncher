# Install required packages via pacman method
# samanthactoet@gmail.com

# install.packages("pacman")

## System libraries required
# apt-get update
# adpt-get install zlib1g-dev
# apt-get install libxml2-dev

library(pacman)

p_load(forecast, tidyverse, magrittr, googlesheets,
       cowplot, lubridate, rwunderground, viridis) # ongoing list
