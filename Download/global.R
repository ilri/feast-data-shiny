library(shiny)
library(DBI)
library(pool)
library(dplyr)
library(shinyWidgets)
library(DT)
library(openxlsx)
library(stringr)
library(shinydashboard)
library(shinyjs)
library(Cairo)#For better Linux display
library(shinydisconnect)



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
  #rm(list = ls(), envir = env.export)
  
  #cleanup FEASTdata files at the end of the session
  if(file.exists(paste0(cacheDIR, "/complete", start_time, "FEAST.xlsx"))){
  file.remove(paste0(cacheDIR, "/complete", start_time, "FEAST.xlsx"))
  }
  if(file.exists(paste0(cacheDIR, "/selected", start_time, "FEAST.xlsx"))){
  file.remove(paste0(cacheDIR, "/selected", start_time, "FEAST.xlsx"))
  }
  if(file.exists(paste0(cacheDIR, "/complete", start_time, "FEAST.zip"))){
  file.remove(paste0(cacheDIR, "/complete", start_time, "FEAST.zip"))
  }
  if(file.exists(paste0(cacheDIR, "/selected", start_time, "FEAST.zip"))){
  file.remove(paste0(cacheDIR, "/selected", start_time, "FEAST.zip"))
  }
  if(!is.null(length(list.files(cacheDIR, full.names = T)[grep(paste0(start_time, "FEAST.csv"), list.files(cacheDIR))]))){
  unlink(list.files(cacheDIR, full.names = T)[grep(paste0(start_time, "FEAST.csv"), list.files(cacheDIR))])
  }
  if(!is.null(length(list.files(cacheDIR, full.names = T)[grep(paste0(start_time, "FEASTsub.csv"), list.files(cacheDIR))]))){
  unlink(list.files(cacheDIR, full.names = T)[grep(paste0(start_time, "FEASTsub.csv"), list.files(cacheDIR))])
  }  
  if(file.exists(paste0(cacheDIR, "/complete", start_time, "FEAST.RDATA"))){
  file.remove(paste0(cacheDIR, "/complete", start_time, "FEAST.RDATA"))
  }
   if(file.exists(paste0(cacheDIR, "/selected", start_time, "FEAST.RDATA"))){
  file.remove(paste0(cacheDIR, "/selected", start_time, "FEAST.RDATA"))
  }
})





####
#Data preparation
tabFull <- dbListTables(pool)
tablesExport <- tabFull[grepl("export", tabFull)] # alt: exportTables <- dbGetQuery(pool, "select TABLE_NAME FROM information_schema.tables WHERE substring(TABLE_NAME, 1, 6) = 'export'") #get all expor table names
tablesInputDisp <- gsub("export_", "", tablesExport)
tablesInputDisp <- data.frame(tabFull = tablesExport, tabFile = tablesInputDisp, tabLab = gsub("_", " ", tablesInputDisp))
tablesInputDisp$tabLab <- paste0(toupper(substr(tablesInputDisp$tabLab, 1, 1)), substr(tablesInputDisp$tabLab, 2, nchar(tablesInputDisp$tabLab)))



#site <- pool %>% tbl("site") %>% data.frame()
#project <- pool %>% tbl("project") %>% data.frame()
#country <- pool %>% tbl("country") %>% data.frame()
#world_region <- tbl(pool, "world_region") %>% data.frame()
#dfRaw <- left_join(site, select(project, id_project = id, project = title))
#dfRaw <- left_join(dfRaw, select(country, id_country = id, country = name, id_world_region))
#dfRaw <- left_join(dfRaw, select(world_region, id_world_region = id, world_region = name))
#dfRaw <- filter(dfRaw, world_region != "Antarctica") #Remove all excluded datasets exclude != 1 & 
#dfeast <- select(dfRaw, world_region, country, project, site = name, created_at, exclude)
#dfeast <- data.frame(dfeast)

#dfeast <- dfeast[, c("world_region", "country", "project", "site", "created_at", "exclude")] #Restrict presentation of results to these columns
#dfeast$country <- trimws(dfeast$country)
#dfeast$site <- trimws(dfeast$site)
#dfeast$site <- str_to_sentence(dfeast$site) #@remove excluded data from these selection options

dfeast <- data.frame(tbl(pool, "export_project_site"))
dfeast$uploaded_at <- as.Date(substr(dfeast$uploaded_at, 1, 10), "%Y-%m-%d")
dfeast$site_country_name <- trimws(dfeast$site_country_name)
dfeast$site_name <- trimws(dfeast$site_name)
dfeast$site_name <- str_to_sentence(dfeast$site_name)
dfeast <- filter(dfeast, site_world_region != "Antarctica" & !is.na(site_world_region))

##Make new export environment
env.export <- new.env()
env.export.sub <- new.env()