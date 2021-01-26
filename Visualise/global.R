library(shiny)
library(DBI)
library(pool)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(DT)
library(openxlsx)
library(ggplot2)
library(plotly)
#library(ggpubr)
library(cowplot)
library(leaflet)
library(stringr)
library(ggsci) #GGplot colour palettes https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
library(wesanderson) #GGplot colour palettes
library(svglite)
library(shinydashboard)
library(Cairo)#For better Linux display
library(shinydisconnect)


options(digits=1, scipen = 999) #Globally set maximum number of decimal digits




source('/var/www/html/webroot/rscripts/feastCred.R')
#pool <- dbPool(
#  drv      = RMySQL::MySQL(),
#  dbname   = "feast_web",
#  host     = "localhost",
#  username = "root", 
#  password = "",
#  port     = 3306
#)

onStop(function() {
  poolClose(pool)
  
})


###Visualisation themes
themePublication <- function(base_size=12, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(#face = "bold",
            size = rel(1.2), hjust = 0.5, margin = margin(0,0,20,0)),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(size = rel(1)), #face = "bold",
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(size = rel(1)), 
            #axis.line.x = element_line(colour="black"),
            #axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "left", #"top",
            legend.direction = "vertical",#  "horizontal",
            #legend.box = "vetical",
            legend.key.size= unit(0.5, "cm"),
            #legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            #strip.text = element_text(face="bold")
            
    ))
}


scaleFillPublication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01","#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)
}



scaleColourPublication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#f87f01","#7fc97f","#ef3b2c","#feca01","#a6cee3","#fb9a99","#984ea3","#8C591D")), ...)
}


###End vis themes



start_time <- Sys.time() #Get start time to allow data to load before download
#dldFlag <- data.frame(download_flag = 0)

####
#Country centroid data sourced from here: https://worldmap.harvard.edu/data/geonode:country_centroids_az8
#print(file.exists(paste0(cacheDIR, "/country_centroids_az8.csv")))
if(file.exists("www/country_centroids_az8.csv")) {countryCentroid <- read.csv("www/country_centroids_az8.csv")} else
{countryCentroid <- read.csv(url("http://worldmap.harvard.edu/download/wfs/34645/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Acountry_centroids_az8&version=1.0.0"))}
countryCentroid$name_long <- gsub("Democratic Republic of the Congo", "Congo The Democratic Republic Of The", countryCentroid$name_long)
#countryCentroid <- read.csv(paste0(cacheDIR, "/country_centroids_az8.csv"))
siteGPS <- read.csv('www/siteGPS.csv')
siteGPS$site <- trimws(siteGPS$Site)
siteGPS$site <- str_to_sentence(siteGPS$site)
siteGPS$prodSystem <- siteGPS$Adjusted.system
#siteGPS <- left_join(siteGPS, select(countryCentroid, country = name_long, lat = Latitude, lon = Longitude))

#Data preparation
tabFull <- dbListTables(pool)
tablesExport <- tabFull[grepl("export", tabFull)] # alt: exportTables <- dbGetQuery(pool, "select TABLE_NAME FROM information_schema.tables WHERE substring(TABLE_NAME, 1, 6) = 'export'") #get all expor table names
tablesInputDisp <- gsub("export_", "", tablesExport)
tablesInputDisp <- data.frame(tabFull = tablesExport, tabFile = tablesInputDisp, tabLab = gsub("_", " ", tablesInputDisp))
tablesInputDisp$tabLab <- paste0(toupper(substr(tablesInputDisp$tabLab, 1, 1)), substr(tablesInputDisp$tabLab, 2, nchar(tablesInputDisp$tabLab)))


dfeast <- data.frame(tbl(pool, "export_project_site"))
dfeast$created_at <- as.Date(substr(dfeast$uploaded_at, 1, 10), "%Y-%m-%d")
dfeast$site_country_name <- trimws(dfeast$site_country_name)
dfeast$site_name <- trimws(dfeast$site_name)
dfeast$site_name <- str_to_sentence(dfeast$site_name)


mapfeast <- group_by(dfeast, site_country_name)
mapfeast <- tally(mapfeast)
mapfeast <- left_join(mapfeast, select(countryCentroid, site_country_name = name_long, lat = Latitude, lon = Longitude))
mapfeast <- mapfeast[trimws(mapfeast$site_country_name) != "Antarctica",]

##Make new export environment
env.export <- new.env()



################################

#DF to lookup prepared data
tablesInputPlot <- data.frame(tabLab = c("Map of sites", "Farm size", "Feed availability", "Crop utilisation"), 
plotDFs = c("", "farm_size", "export_feed_source_availability", "crop_utilisation"),
facetVar = c("", NA, "site_name", "site_name"))
#tablesInputPlot <- data.frame(tabLab = c("Map of sites", "Farm size", "Feed availability", "Livestock holdings", "Liveweight", "Distance to market", "AI uptake", "Fodder yield", "Fodder area cultivated", "Crop utilisation"), 
#plotDFs = c("", "farm_size", "export_feed_source_availability", "livestock_holding", "export_livestock_holding", "FGD", "FGD", "export_fodder_crop_cultivation", "export_fodder_crop_cultivation", "crop_utilisation"),
#facetVar = c("", NA, "site_name", NA, "site_country", NA, NA, "site_country", "site_country", "site_name"))




