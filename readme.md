# Feed Assessment Tool (FEAST) data repository - Shiny download and visualisation applications
This repository hosts the R-Shiny code to download and visualise data stored in the [FEAST Global Data Repository](https://feastdata.ilri.org/).
There are two seperate applications for download and visualise functions.

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## CHANGELOG


## Documentation

* [Shiny server set-up](#shiny-server-set-up)
* [Required packages](#required-packages)
* [Run the applications](#run-the-applications)


### Shiny server set-up
On an Ubuntu GNU/Linux system, install R (>4.0.0) and follow [this guide](https://rstudio.com/products/shiny/download-server/ubuntu/)

```console
$ sudo apt install r-base
$ sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
```

Follow instructions in [the Shiny-server wiki](https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source).

Run and test your Shiny-server implementation:

```console
$ sudo systemctl start shiny-server
```

For troubleshooting, see the [Shiny-server administrator's guide](https://docs.rstudio.com/shiny-server/).

### Required packages
Install R package dependencies

```console
$ sudo apt install build-essential libfontconfig1-dev libcairo2-dev gfortran libcurl4-openssl-dev libssl-dev libgit2-dev libudunits2-dev libxt-dev gdal-bin libgdal-dev libharfbuzz-dev libfribidi-dev
```

Install R packages:

```console
$ sudo su -
# R -e "install.packages(c('RMariaDB', 'DBI', 'pool', 'dplyr', 'ggplot2', 'DT', 'devtools', 'shinyjs', 'openxlsx', 'leaflet', 'wesanderson', 'ggthemes', 'ggsci', 'cowplot', 'plotly', 'shinydashboard', 'shinyWidgets', 'dbplyr', 'svglite', 'Cairo', 'jsonlite', 'raster', 'sf', 'readr', 'shinydisconnect'), repos='https://cran.rstudio.com/')"
```
Alternative to sf: `exactextractr` and `geojsonsf` (more efficient than sf_read). 


### Run the applications
Enter the database name, host, username and password in the two `Global.R` files.

Change file permissions recursively for www folders in Downloads and Visualise

Test application in R:

```console
$ R
> library(shiny)
> runApp('/srv/Shiny/Download/', launch.browser = F)
```

### Acknowledgement

This research was conducted as part of the [CGIAR](https://www.cgiar.org/) Research Program on Livestock and is supported by contributors to the [CGIAR Trust Fund](https://www.cgiar.org/funders/). CGIAR is a global research partnership for a food-secure future. Its science is carried out by 15 Research Centers in close collaboration with hundreds of partners across the globe.
