---
title: README
output: html_document
---


This is the repository for Team Hauncher in the Astrea Open Data Challenge. 


### Team Members


Nathan Day nathancday@gmail.com
Samantha Toet samanthactoet@gmail.com


### Submission Contents


This repository contains the following:


* `install.R`: loads the docker Rstudio container and all required packages
* `read.R`: reads in the data from the Google Drive
* `clean.R`: tidyies the data and puts into a more readable date format
* `weather.R`: loads weather data for the Downtown Mall
* `events.R`: loads event data for the Downtown Mall
* `ts.R`: builds time series model for WiFi data
* `explore.R`: looks at relationships between WiFi, weather, and event data and builds hypothesis
* `model.R`: tests the hypothesis and finalizes predictive model 
* `final.R`: builds final csv's for submission




### Targets


Best Predictive Model: shown in `final.R` 


### Setup Up on MacOS 
(change local paths)


Quick:
Clone GitHub repository:
https://github.com/nathancday/hauncher




Get image from our final image from Docker Hub
nathancday/hauncher


Run Docker image via bash :
`docker run -d -p 8787:8787-v /Users/nathancday/Desktop/hauncher:/home/rstudio/hauncher -e ROOT=TRUE nathancday/hauncher`




From scratch (rocker/rstudio):


`docker run -d -p 8787:8787-v /Users/on_osx/Desktop/hauncher:/home/rstudio/hauncher -e ROOT=TRUE rocker/rstudio`


System library installs done inside the Rstudio terminal or a separate bash terminal:


* `apt-get update`
* `apt-get install zlib1g-dev`
* `apt-get install libxml2-dev`


Required R packages all available from [CRAN](https://cran.r-project.org/) under the [GPL-3 License](https://cran.r-project.org/web/licenses/GPL-3):


* `forecast`
* `tidyverse`
* `magrittr`
* `googlesheets`
* `cowplot`
* `lubridate`
* `rwunderground` 
* `viridis`
* `mgcv`
* `pacman`


The packages are installed in the `install.R` file and called to with `p_load`.






### Running the Model


1.  Source `final.R`