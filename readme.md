Feed Assessment Tool (FEAST) data repository - download and visualisation
------------
This repository hosts the R-Shiny code to download and visualise data stored in the [FEAST Global Data Repository](https://feastdata.ilri.org/).
There are two seperate applications for download and visualise functions.

[![MIT Licensed](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat-square)](LICENSE.md)



### CHANGELOG


### Documentation

* [Shiny server set-up](#Shiny server set-up)
* [Required packages](#Required packages)
* [Run the applications](#Run the applications)


### Shiny server set-up
On a Ubuntu Linux system, install R (>4.0.0) and follow [this guide](https://rstudio.com/products/shiny/download-server/ubuntu/)
```
$ sudo apt-get install r-base
sudo su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
```



If running on an Arch Linux system, you may need to build from source
Follow instructions in [the Shiny-server wiki](https://github.com/rstudio/shiny-server/wiki/Building-Shiny-Server-from-Source)
```
$ git clone https://aur.archlinux.org/r-devel.git
$ cd r-devel/
$ makepkg -sri
```

Run and test your Shiny-server implementation
```
systemctl start shiny-server
```

For troubleshooting, see the [Shiny-server administrator's guide](https://docs.rstudio.com/shiny-server/)

### Required packages
Install R package dependencies
```
$ sudo apt-get installlibfontconfig1-dev 
$ sudo apt-get installlibcairo2-dev
$ sudo apt-get installgfortran

```

Install R packages
```
$ sudo su - -c "R -e \"install.packages(c('RMySQL', 'DBI', 'pool', 'dplyr', 'ggplot2', 'DT', 'devtools', 'shinyjs', 'openxlsx', 'leaflet', 'wesanderson', 'ggthemes', 'ggsci', 'cowplot', 'plotly', 'shinydashboard'), repos='https://cran.rstudio.com/')\""
```

### Run the applications
Enter the database name, host, username and password in the two Global.R files
Test application in R 
```
$ R
> library(shiny)
> runApp(/srv/Shiny/Download/, launch.browser = F)
```

