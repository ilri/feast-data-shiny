library(shiny)
library(DBI)
library(pool)
library(dplyr)
library(shinyWidgets)
library(DT)
library(openxlsx)
library(stringr)
#library(rhandsontable)
#library(RODBC) #Use to have all data available for download?
#con <- odbcConnect(dsn = "FEAST", uid="root", pwd="yarr")

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



start_time <- Sys.time() #Get start time to allow data to load before download
#dldFlag <- data.frame(download_flag = 0)

####
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
  assign(tablesExport[i], select(eval(parse(text = tablesExport[i])), -exclDate, -excluded, -export_time, -private))
  
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
  assign(tablesExport[i], select(eval(parse(text = tablesExport[i])), -exclDate, -excluded, -export_time, -private))
  
  }

  saveRDS(data.frame(site), paste0(persistDIR, "/NewDataCheck.rds"))

  save(list = ls()[grepl("export", ls())], file = paste0(persistDIR, "/FEASTdatCache.RDATA"))
}

if(file.exists(paste0(persistDIR, "/FEASTdatCache.RDATA")) & !exists("export_project_site")){ #load data only if the file exists and the objects haven't already been imported due to data update
  load(paste0(persistDIR, "/FEASTdatCache.RDATA"))
}





##Further revisions to datasets
export_focus_group$site_country <- trimws(export_focus_group$site_country)
export_focus_group$farmSizeU1prop <- ifelse(export_focus_group$focus_group_threshold_small_farm_ha <= 1 & export_focus_group$focus_group_threshold_large_farm_ha <=1, rowSums(export_focus_group[, c("focus_group_percent_households_small", "focus_group_percent_households_medium")], na.rm = T), 
                                            export_focus_group$focus_group_percent_households_small)
#matchFGD <- export_feed_source_availability[!duplicated(export_feed_source_availability), c("focus_group_id", "focus_group_community")], focus_group_id)

FGD <- left_join(export_focus_group, select(export_crop_cultivation, focus_group_id, focus_group_community))

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


export_feed_source_availability <- export_feed_source_availability[!is.na(export_feed_source_availability$feed_source_description),]
export_feed_source_availability <- export_feed_source_availability[export_feed_source_availability$feed_source_description != "GRAZING",]

#export_feed_source_availability$percentage[is.na(export_feed_source_availability$percentage)] <- 0
export_feed_source_availability$feed_source_description <- str_to_sentence(export_feed_source_availability$feed_source_description)
export_feed_source_availability$feed_source_description <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", export_feed_source_availability$feed_source_description, perl=TRUE) #Remove more than one space in a row
export_feed_source_availability$feed_source_description <- trimws(export_feed_source_availability$feed_source_description)
export_feed_source_availability$feed_source_description <- gsub(",([A-Za-z])", ", \\1", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
export_feed_source_availability$feed_source_description <- gsub("Collected fooder", "Collected fodder", export_feed_source_availability$feed_source_description) #Add space to cases where a character follows a comma
export_feed_source_availability$feed_source_description <- gsub("Collect fodder", "Collected fodder", export_feed_source_availability$feed_source_description) #Add space to cases where a character 

export_livestock_holding$site_country <- trimws(export_livestock_holding$site_country)
export_livestock_holding$livestock_holding_dominant_breed <- str_to_sentence(export_livestock_holding$livestock_holding_dominant_breed)
export_livestock_holding$livestock_holding_dominant_breed <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", export_livestock_holding$livestock_holding_dominant_breed, perl=TRUE) #Remove more than one space in a row
export_livestock_holding$livestock_holding_dominant_breed <- trimws(export_livestock_holding$livestock_holding_dominant_breed)
export_livestock_holding$livestock_holding_dominant_breed <- gsub(",([A-Za-z])", ", \\1", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma
export_livestock_holding$livestock_holding_dominant_breed <- gsub("Collected fooder", "Collected fodder", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma
export_livestock_holding$livestock_holding_dominant_breed <- gsub("Collect fodder", "Collected fodder", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma

export_fodder_crop_cultivation$fodderName <- gsub("\\s*\\([^\\)]+\\)", "", export_fodder_crop_cultivation$fodder_crop_type_name)
export_fodder_crop_cultivation$fodder_crop_cultivation_cultiavted_land_ha <- as.numeric(export_fodder_crop_cultivation$fodder_crop_cultivation_cultiavted_land_ha)


##Prepare data for export and visualisation by putting in a new environment with less verbose table names
for(i in 1:length(tablesExport)){
assign(substr(tablesExport[i], 8, nchar(tablesExport[i])), eval(parse(text = tablesExport[i])), envir = env.export) #Add the table to a new environment and remove 'export_' from the start
}
