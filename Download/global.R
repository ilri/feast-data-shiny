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
library(readr)



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



dfeast <- data.frame(tbl(pool, "export_project_site"))
dfeast$exclDate <- as.POSIXct(dfeast$uploaded_at + (365*24*60*60)) 
dfeast <- filter(dfeast, !(exclDate > Sys.time() & !(private %in% c(0, NA))) & excluded %in% c(0, NA))
dfeast$uploaded_at <- as.Date(substr(dfeast$uploaded_at, 1, 10), "%Y-%m-%d")
dfeast$site_country_name <- trimws(dfeast$site_country_name)
dfeast$site_name <- trimws(dfeast$site_name)
dfeast$site_name <- str_to_sentence(dfeast$site_name)
dfeast <- filter(dfeast, site_world_region != "Antarctica" & !is.na(site_world_region))

##Make new export environment
env.export <- new.env()
env.export.sub <- new.env()