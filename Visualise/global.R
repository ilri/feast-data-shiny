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


options(digits=1, scipen = 999) #Globally set maximum number of decimal digits


#Initialise persist data directory
if(!dir.exists(file.path("www/FEASTresources"))) { 
  dir.create(file.path("www/FEASTresources"))
  } #Check if cache data directory exists and if not, create it.

#Initialise cache data directory
if(!dir.exists(file.path("/tmp/FEASTdata"))) { 
  dir.create(file.path("/tmp/FEASTdata"))
  } #Check if cache data directory exists and if not, create it.

persistDIR <- "www/FEASTresources" #file.access returns 0 if has permission to write or -1 if not
cacheDIR <- "/tmp/FEASTdata" #ifelse(as.logical(file.access(".", 2) == 0), "FEASTdata", 




pool <- dbPool(
  drv      = RMySQL::MySQL(),
  dbname   = "feast_web",
  host     = "127.0.0.1",
  username = "", 
  password = "",
  port     = 3306
)

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

site <- pool %>% tbl("site")
project <- pool %>% tbl("project")
country <- pool %>% tbl("country")
world_region <- tbl(pool, "world_region")
dfRaw <- left_join(site, select(project, id_project = id, project = title))
dfRaw <- left_join(dfRaw, select(country, id_country = id, country = name, id_world_region))
dfRaw <- left_join(dfRaw, select(world_region, id_world_region = id, world_region = name))
dfRaw <- filter(dfRaw, world_region != "Antarctica") #Remove all excluded datasets exclude != 1 & 
dfeast <- select(dfRaw, world_region, country, project, site = name, created_at, exclude)
dfeast <- data.frame(dfeast)
dfeast$created_at <- as.Date(substr(dfeast$created_at, 1, 10), "%Y-%m-%d")
dfeast$country <- trimws(dfeast$country)


mapfeast <- group_by(dfeast, country)
mapfeast <- tally(mapfeast)
mapfeast <- left_join(mapfeast, select(countryCentroid, country = name_long, lat = Latitude, lon = Longitude))


##Prepare tables for export. Bring export SQL tables into env. Wait until UI data is loaded
#if (!is.null(dfeast) & file.exists("FEASTdata/cacheDownloadData.RDATA")){
#for(i in 1:length(tablesExport)){ #Bring all export tables into R assigning each table as an object
#  assign(tablesExport[i], data.frame(tbl(pool, tablesExport[i])))

#	}#		#Alt
#		#assign(exportTables[i]) <- dbGetQuery(pool, paste0("SELECT GROUP_CONCAT(COLUMN_NAME) AS headers FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME =", "'", tablesExport[1], "'", "ORDER BY ORDINAL_POSITION"))
# }

##Make new export environment
env.export <- new.env()

##Overwrite cache folder if new data added to repository
if(file.exists(paste0(persistDIR, "/NewDataCheck.rds"))) {newDataCheck <- readRDS(paste0(persistDIR, "/NewDataCheck.rds"))
} else{newDataCheck <- data.frame(site)[1,]} #If the newDatacheck file doesn't exist, then take the first row of the site table. This will be saved to disk.
if(nrow(newDataCheck) < nrow(data.frame(site))) {
  for(i in 1:length(tablesExport)){ #Bring all export tables into R assigning each table as an object
  assign(tablesExport[i], data.frame(tbl(pool, tablesExport[i])))
  ##Exclude data here. Filter out observations with a 1 year embargo. First create a new variable exclDate and then filter
  assign(tablesExport[i], `[[<-`(get(tablesExport[i]), 'exclDate', value = as.POSIXct(eval(parse(text = tablesExport[i]))$uploaded_at) + (365*24*60*60))) 
  assign(tablesExport[i], filter(eval(parse(text = tablesExport[i])), !(exclDate > Sys.time() & excluded == 1) | is.na(excluded)))
  assign(tablesExport[i], filter_at(eval(parse(text = tablesExport[i])), vars(starts_with("site_country")), all_vars(. != "Antarctica ")))
  
  }
  saveRDS(data.frame(site), paste0(persistDIR, "/NewDataCheck.rds"))

  save(list = ls()[grepl("export", ls())], file = paste0(persistDIR, "/FEASTdatCache.RDATA"))
  


}

if(!file.exists(paste0(persistDIR, "/FEASTdatCache.RDATA")) & !exists("export_project_site")){ #Just in case the RDATA file is deleted but the RDS file is there with equal rows.
  for(i in 1:length(tablesExport)){ #Bring all export tables into R assigning each table as an object
  assign(tablesExport[i], data.frame(tbl(pool, tablesExport[i])))
  ##Exclude data here. Filter out observations with a 1 year embargo. First create a new variable exclDate and then filter
  assign(tablesExport[i], `[[<-`(get(tablesExport[i]), 'exclDate', value = as.POSIXct(eval(parse(text = tablesExport[i]))$uploaded_at) + (365*24*60*60))) 
  assign(tablesExport[i], filter(eval(parse(text = tablesExport[i])), !(exclDate > Sys.time() & excluded == 1) | is.na(excluded)))
  assign(tablesExport[i], filter_at(eval(parse(text = tablesExport[i])), vars(starts_with("site_country")), all_vars(. != "Antarctica ")))

  }

  saveRDS(data.frame(site), paste0(persistDIR, "/NewDataCheck.rds"))

  save(list = ls()[grepl("export", ls())], file = paste0(persistDIR, "/FEASTdatCache.RDATA"))

  #@Tmp hach to improve UX
  export_feed_source_availability$site_country <- trimws(export_feed_source_availability$site_country)

export_feed_source_availability <- export_feed_source_availability[!is.na(export_feed_source_availability$feed_source_description),]
export_feed_source_availability <- export_feed_source_availability[export_feed_source_availability$feed_source_description != "GRAZING",]


#export_feed_source_availability$percentage[is.na(export_feed_source_availability$percentage)] <- 0
export_feed_source_availability$feed_source_description <- str_to_sentence(export_feed_source_availability$feed_source_description)
export_feed_source_availability$feed_source_description <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", export_feed_source_availability$feed_source_description, perl=TRUE) #Remove more than one space in a row
export_feed_source_availability$feed_source_description <- trimws(export_feed_source_availability$feed_source_description)
export_feed_source_availability$feed_source_description <- gsub(",([A-Za-z])", ", \\1", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
export_feed_source_availability$feed_source_description <- gsub("Collected fooder", "Collected fodder", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
export_feed_source_availability$feed_source_description <- gsub("Collect fodder", "Collected fodder", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
#export_feed_source_availability$focus_group_community <- str_to_sentence(export_feed_source_availability$focus_group_community)


export_feed_source_availability$site_country <- trimws(export_feed_source_availability$site_country)

export_feed_source_availability$feedCategory <- ifelse(export_feed_source_availability$feed_source_description %in% c("Acacia ponds, bean tops", "Cereal crop residues", "Leguminous crop residues"), "Crop residues", 
                                                        ifelse(export_feed_source_availability$feed_source_description %in% c("Collected fodder", "Fodder", "Green forage (e.g., weeds, fodder crops, leaves"), "Green forage",
                                                               ifelse(export_feed_source_availability$feed_source_description == "Grazing", "Grazing",
                                                               ifelse(export_feed_source_availability$feed_source_description %in% c("Concentrates", "Pastures, fodder, concertrates, animal by products and industrial by products"), "Concentrates", "Other"))))

  export_feed_source_availability$site_name <- str_to_sentence(export_feed_source_availability$site_name)  

  feed_availability_prop <- group_by(export_feed_source_availability, site_country, site_id, focus_group_id, focus_group_community, site_name, month_id, feedCategory)
  feed_availability_prop <- summarise(feed_availability_prop, percentage = mean(percentage, na.rm=T), )
  feed_availability_prop$site_country[feed_availability_prop$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
  feed_availability_score <- group_by(export_feed_source_availability, site_country, site_id, focus_group_id, focus_group_community, site_name, month_id)
  feed_availability_score <- summarise(feed_availability_score, feedAvailability = mean(feed_availability, na.rm=T), )
  range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm=T)-min(x, na.rm = T))}
  feed_availability_score$feedAvailability <- range01(feed_availability_score$feedAvailability) #Rescale between 0 and 1
  feed_availability_score <- feed_availability_score[feed_availability_score$site_country != "Antarctica",]
  feed_availability_score$site_country[feed_availability_score$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          
  feed_availability_prop <- arrange(feed_availability_prop, site_country, focus_group_community)
  feed_availability_score <- arrange(feed_availability_score, site_country, focus_group_community)
          
  feed_availability_prop$focus_group_community <- str_to_sentence(feed_availability_prop$focus_group_community)
  feed_availability_score$focus_group_community <- str_to_sentence(feed_availability_score$focus_group_community)


}

if(file.exists(paste0(persistDIR, "/FEASTdatCache.RDATA")) & !exists("export_project_site")){ #load data only if the file exists and the objects haven't already been imported due to data update
  load(paste0(persistDIR, "/FEASTdatCache.RDATA"))
}

################################



#################################
## Data cleaning
export_coop_membership$site_country <- trimws(export_coop_membership$site_country)                 
export_income_activity$site_country <- trimws(export_income_activity$site_country)
export_core_context_attribute_score$site_country  <- trimws(export_core_context_attribute_score$site_country)   
export_labour_activity$site_country <- trimws(export_labour_activity$site_country)
export_crop_cultivation$site_country <- trimws(export_crop_cultivation$site_country)      
export_livestock_holding$site_country <- trimws(export_livestock_holding$site_country)
export_decision_making_by_household$site_country <- trimws(export_decision_making_by_household$site_country)   
export_livestock_sale$site_country <- trimws(export_livestock_sale$site_country)
export_feed_labor_division$site_country <- trimws(export_feed_labor_division$site_country)            
export_project_site$site_country <- trimws(export_project_site$site_country_name)
export_project_site$site_country_name <- trimws(export_project_site$site_country_name) #@Duplicating temporarily
export_feed_source_availability$site_country <- trimws(export_feed_source_availability$site_country)       
export_purchased_feed$site_country <- trimws(export_purchased_feed$site_country)
export_focus_group$site_country <- trimws(export_focus_group$site_country)               
export_respondent$site_country <- trimws(export_respondent$site_country)
export_focus_group_monthly_statistics$site_country <- trimws(export_focus_group_monthly_statistics$site_country)  
export_respondent_monthly_statistics$site_country <- trimws(export_respondent_monthly_statistics$site_country)
export_fodder_crop_cultivation$site_country <- trimws(export_fodder_crop_cultivation$site_country)        
export_womens_income_activity$site_country <- trimws(export_womens_income_activity$site_country)

export_coop_membership$site_name <- trimws(export_coop_membership$site_name)                 
export_income_activity$site_name <- trimws(export_income_activity$site_name)
export_core_context_attribute_score$site_name  <- trimws(export_core_context_attribute_score$site_country)   
export_labour_activity$site_name <- trimws(export_labour_activity$site_name)
export_crop_cultivation$site_name <- trimws(export_crop_cultivation$site_name)      
export_livestock_holding$site_name <- trimws(export_livestock_holding$site_name)
export_decision_making_by_household$site_name <- trimws(export_decision_making_by_household$site_name)   
export_livestock_sale$site_name <- trimws(export_livestock_sale$site_name)
export_feed_labor_division$site_name <- trimws(export_feed_labor_division$site_name)            
export_project_site$site_name <- trimws(export_project_site$site_name)
export_feed_source_availability$site_name <- trimws(export_feed_source_availability$site_name)       
export_purchased_feed$site_name <- trimws(export_purchased_feed$site_name)
export_focus_group$site_name <- trimws(export_focus_group$site_name)               
export_respondent$site_name <- trimws(export_respondent$site_name)
export_focus_group_monthly_statistics$site_name <- trimws(export_focus_group_monthly_statistics$site_name)  
export_respondent_monthly_statistics$site_name <- trimws(export_respondent_monthly_statistics$site_name)
export_fodder_crop_cultivation$site_name <- trimws(export_fodder_crop_cultivation$site_name)        
export_womens_income_activity$site_name <- trimws(export_womens_income_activity$site_name)


export_coop_membership$site_name <- str_to_sentence(export_coop_membership$site_name)                 
export_income_activity$site_name <- str_to_sentence(export_income_activity$site_name)
export_core_context_attribute_score$site_name  <- str_to_sentence(export_core_context_attribute_score$site_country)   
export_labour_activity$site_name <- str_to_sentence(export_labour_activity$site_name)
export_crop_cultivation$site_name <- str_to_sentence(export_crop_cultivation$site_name)      
export_livestock_holding$site_name <- str_to_sentence(export_livestock_holding$site_name)
export_decision_making_by_household$site_name <- str_to_sentence(export_decision_making_by_household$site_name)   
export_livestock_sale$site_name <- str_to_sentence(export_livestock_sale$site_name)
export_feed_labor_division$site_name <- str_to_sentence(export_feed_labor_division$site_name)            
export_project_site$site_name <- str_to_sentence(export_project_site$site_name)
export_feed_source_availability$site_name <- str_to_sentence(export_feed_source_availability$site_name)       
export_purchased_feed$site_name <- str_to_sentence(export_purchased_feed$site_name)
export_focus_group$site_name <- str_to_sentence(export_focus_group$site_name)               
export_respondent$site_name <- str_to_sentence(export_respondent$site_name)
export_focus_group_monthly_statistics$site_name <- str_to_sentence(export_focus_group_monthly_statistics$site_name)  
export_respondent_monthly_statistics$site_name <- str_to_sentence(export_respondent_monthly_statistics$site_name)
export_fodder_crop_cultivation$site_name <- str_to_sentence(export_fodder_crop_cultivation$site_name)        
export_womens_income_activity$site_name <- str_to_sentence(export_womens_income_activity$site_name)

###########################################

##Prepare data for export and visualisation by putting in a new environment with less verbose table names
for(i in 1:length(tablesExport)){
assign(substr(tablesExport[i], 8, nchar(tablesExport[i])), eval(parse(text = tablesExport[i])), envir = env.export) #Add the table to a new environment and remove 'export_' from the start
}



###Data preparation
#Feed availability
export_feed_source_availability <- export_feed_source_availability[!is.na(export_feed_source_availability$feed_source_description),]
export_feed_source_availability <- export_feed_source_availability[export_feed_source_availability$feed_source_description != "GRAZING",]


#export_feed_source_availability$percentage[is.na(export_feed_source_availability$percentage)] <- 0
export_feed_source_availability$feed_source_description <- str_to_sentence(export_feed_source_availability$feed_source_description)
export_feed_source_availability$feed_source_description <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", export_feed_source_availability$feed_source_description, perl=TRUE) #Remove more than one space in a row
export_feed_source_availability$feed_source_description <- trimws(export_feed_source_availability$feed_source_description)
export_feed_source_availability$feed_source_description <- gsub(",([A-Za-z])", ", \\1", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
export_feed_source_availability$feed_source_description <- gsub("Collected fooder", "Collected fodder", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
export_feed_source_availability$feed_source_description <- gsub("Collect fodder", "Collected fodder", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
#export_feed_source_availability$focus_group_community <- str_to_sentence(export_feed_source_availability$focus_group_community)


export_feed_source_availability$site_country <- trimws(export_feed_source_availability$site_country)

export_feed_source_availability$feedCategory <- ifelse(export_feed_source_availability$feed_source_description %in% c("Acacia ponds, bean tops", "Cereal crop residues", "Leguminous crop residues"), "Crop residues", 
                                                        ifelse(export_feed_source_availability$feed_source_description %in% c("Collected fodder", "Fodder", "Green forage (e.g., weeds, fodder crops, leaves"), "Green forage",
                                                               ifelse(export_feed_source_availability$feed_source_description == "Grazing", "Grazing",
                                                               ifelse(export_feed_source_availability$feed_source_description %in% c("Concentrates", "Pastures, fodder, concertrates, animal by products and industrial by products"), "Concentrates", "Other"))))



#Farm size
export_focus_group$site_country <- trimws(export_focus_group$site_country)
export_focus_group$farmSizeU1prop <- ifelse(export_focus_group$focus_group_threshold_small_farm_ha <= 1 & export_focus_group$focus_group_threshold_large_farm_ha <=1, rowSums(export_focus_group[, c("focus_group_percent_households_small", "focus_group_percent_households_medium")], na.rm = T), 
                                            export_focus_group$focus_group_percent_households_small)
#matchFGD <- export_feed_source_availability[!duplicated(export_feed_source_availability), c("focus_group_id", "focus_group_community")], focus_group_id)

FGD <- left_join(export_focus_group, select(export_crop_cultivation, site_name, focus_group_id, focus_group_community))

farm_size <- gather(select(FGD, site_country, focus_group_community, site_name, focus_group_id, Landless = focus_group_percent_households_landless, U1ha = farmSizeU1prop, Large = focus_group_percent_households_large), farmSize, proportion, c(Landless, U1ha, Large))
farm_size$proportion[is.na(farm_size$proportion)] <- 0
farm_size$farmSize[farm_size$farmSize == "U1ha"] <- "Under 1 ha"

#Livestock
export_livestock_holding$site_country <- trimws(export_livestock_holding$site_country)
export_livestock_holding$livestock_holding_dominant_breed <- str_to_sentence(export_livestock_holding$livestock_holding_dominant_breed)
export_livestock_holding$livestock_holding_dominant_breed <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", export_livestock_holding$livestock_holding_dominant_breed, perl=TRUE) #Remove more than one space in a row
export_livestock_holding$livestock_holding_dominant_breed <- trimws(export_livestock_holding$livestock_holding_dominant_breed)
export_livestock_holding$livestock_holding_dominant_breed <- gsub(",([A-Za-z])", ", \\1", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma
export_livestock_holding$livestock_holding_dominant_breed <- gsub("Collected fooder", "Collected fodder", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma
export_livestock_holding$livestock_holding_dominant_breed <- gsub("Collect fodder", "Collected fodder", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma

export_livestock_holding$cattleBreeds <- ifelse(export_livestock_holding$livestock_holding_dominant_breed %in% c("L0cal zebu", "Llocal", "Loal", "Local", "Local'", "Local + improved", "Local and exotic", "Local beed", "Local beed (ri)", "Local breed", "Local breed (ri)", "Local breed\\", "Local east africa", "Local east african", "Local improved", "Local zebu", "Local zeu", "Locl", "Locsl", "Zebu", "Zebu (local)", "Zebu/ayrshire", "Indegenous", "Indeginous", "Indigeneous", "Indigenous", "Indigenousl", "Indigineous", "Indiginous", "Indiginous/galla cross"), "Local breed",
                                                ifelse(export_livestock_holding$livestock_holding_dominant_breed %in% c("Arshire", "Arshyre", "Aryshire", "Aryshire cross", "Aryshire/fresian", "Ashiers", "Ayshire"), "Ayshire", 
                                                       ifelse(substr(export_livestock_holding$livestock_holding_dominant_breed, 1, 1) == "F", "Friesian", 
                                                              ifelse(substr(export_livestock_holding$livestock_holding_dominant_breed, 1, 1) == "J", "Jersey", "Other"))))
export_livestock_holding$cattleBreeds[is.na(export_livestock_holding$cattleBreeds)] <- "Other"

export_livestock_holding$TLU <- (export_livestock_holding$livestock_holding_average_weight/250) * export_livestock_holding$livestock_holding_headcount

livestock_holding <- group_by(export_livestock_holding, site_country, site_name, focus_group_id, focus_group_community)
livestock_holding <- summarise(livestock_holding, TLU = sum(TLU, na.rm=T))

#Crops
export_crop_cultivation$site_country <- trimws(export_crop_cultivation$site_country)


crop_utilisation <- gather(select(export_crop_cultivation, site_country, site_name, focus_group_id, focus_group_community, crop_type_name, crop_cultivation_percent_fed, crop_cultivation_percent_burned, crop_cultivation_percent_mulched, crop_cultivation_percent_sold, crop_cultivation_percent_other), use, percentage, c(crop_cultivation_percent_fed, crop_cultivation_percent_burned, crop_cultivation_percent_mulched, crop_cultivation_percent_sold, crop_cultivation_percent_other))
crop_utilisation$use <- gsub("crop_cultivation_percent_", "", crop_utilisation$use)
crop_utilisation$use <- str_to_sentence(crop_utilisation$use)
#crop_utilisation$focus_group_community <- str_to_sentence(crop_utilisation$focus_group_community)
crop_utilisation$cropName <- gsub("\\s*\\([^\\)]+\\)","", crop_utilisation$crop_type_name)

#Fodder
export_fodder_crop_cultivation$site_country <- trimws(export_fodder_crop_cultivation$site_country)
export_fodder_crop_cultivation$fodderName <- gsub("\\s*\\([^\\)]+\\)", "", export_fodder_crop_cultivation$fodder_crop_type_name)
export_fodder_crop_cultivation$fodder_crop_cultivation_cultiavted_land_ha <- as.numeric(export_fodder_crop_cultivation$fodder_crop_cultivation_cultiavted_land_ha)




#DF to lookup prepared data
#tablesInputPlot <- tablesInputDisp[!tablesInputDisp$tabLab %in% c("Coop membership", "Core context attribute score", "Decision making by household", "Feed labor division", "Focus group monthly statistics", "Income activity", "Labour activity", "Livestock sale", "Purchased feed", "Respondent", "Respondent monthly statistics", "Womens income activity"),]
tablesInputPlot <- data.frame(tabLab = c("Map of sites", "Farm size", "Feed availability", "Livestock holdings", "Liveweight", "Distance to market", "AI uptake", "Fodder yield", "Fodder area cultivated", "Crop utilisation"), 
plotDFs = c("", "farm_size", "export_feed_source_availability", "livestock_holding", "export_livestock_holding", "FGD", "FGD", "export_fodder_crop_cultivation", "export_fodder_crop_cultivation", "crop_utilisation"),
facetVar = c("", NA, "site_name", NA, "site_country", NA, NA, "site_country", "site_country", "site_name"))




