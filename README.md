---
title: README
output: html_document
---

This is the repository for Team Hauncher in the Astrea Open Data Challenge. 

### Team Members

Nathan Day nathancday@gmail.com <br />
Samantha Toet samanthactoet@gmail.com

### Submission Contents

The files in this repository complete the following:

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

Best Predictive Model: shown in `final.R` <br />


### Libraries 

Required bash code:

`docker run -d -p 8787:8787-v /Users/on_osx/Desktop/hauncher:/home/rstudio/hauncher -e ROOT=TRUE rocker/rstudio`

Github repository:
https://github.com/nathancday/hauncher

System library installs in the Rstudio terminal:

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

1. Run the docker image using the above code
2. Clone the GitHub repo (to save time loading the data)
3. Source `final.R`


Thanks!


