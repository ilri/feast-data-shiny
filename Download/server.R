server <- function(input, output, session) {

#@Raw data prepration is in global file.

session$allowReconnect(TRUE) #Make sure server reconnects rather than grey out


start_time <- Sys.time() #Get start time to allow data to load before download

#Initialise persist data directory
if(!dir.exists(file.path("/tmp/FEASTpersist"))) { 
  dir.create(file.path("/tmp/FEASTpersist"))
  } #Check if cache data directory exists and if not, create it.

#Initialise cache data directory
if(!dir.exists(file.path(paste0("/tmp/FEASTdata/", as.numeric(start_time))))) { 
  dir.create(file.path(paste0("/tmp/FEASTdata/", as.numeric(start_time))))
  } #Check if cache data directory exists and if not, create it.

persistDIR <- "/tmp/FEASTpersist" #file.access returns 0 if has permission to write or -1 if not
cacheDIR <- paste0("/tmp/FEASTdata/", as.numeric(start_time)) #ifelse(as.logical(file.access(".", 2) == 0), "FEASTdata", 
#########################################################
#Data preparation 
###@Need reactive DB checks https://www.r-bloggers.com/2018/06/database-bulk-update-and-inline-editing-in-a-shiny-application-2/



##Load cached reference
if(file.exists(paste0(persistDIR, "/NewDataCheck.rds"))) {newDataCheck <- readRDS(paste0(persistDIR, "/NewDataCheck.rds"))
} else{newDataCheck <- dfeast}


if(file.exists(paste0(persistDIR, "/FEASTdatCache.RDATA")) & identical(newDataCheck$project_title, data.frame(tbl(pool, "export_project_site"))$project_title) & identical(newDataCheck$site_name, data.frame(tbl(pool, "export_project_site"))$site_name) & 
identical(newDataCheck$sp_site_lastup, data.frame(tbl(pool, "export_project_site"))$sp_site_lastup) & identical(newDataCheck$sp_fg_lastup, data.frame(tbl(pool, "export_project_site"))$sp_fg_lastup) & !exists("export_project_site")){ #load data only if the file exists and the objects haven't already been imported due to data update
  withProgress(message = 'Loading cached data',
    detail = 'Please wait', value = 10, {
  load(paste0(persistDIR, "/FEASTdatCache.RDATA"))
  })
} else{
	withProgress(message = 'Receiving data updates',
    detail = 'Please wait', value = 0, {
#if(!file.exists(paste0(persistDIR, "/FEASTdatCache.RDATA")) | !file.exists(paste0(persistDIR, "/NewDataCheck.rds"))){ #Just in case the RDATA file is deleted but the RDS file is there with equal rows.
  for(i in 1:length(tablesExport)){ #Bring all export tables into R assigning each table as an object
  assign(tablesExport[i], data.frame(tbl(pool, tablesExport[i])))
  ##Exclude data here. Filter out observations with a 1 year embargo. First create a new variable exclDate and then filter
  assign(tablesExport[i], `[[<-`(get(tablesExport[i]), 'exclDate', value = as.POSIXct(eval(parse(text = tablesExport[i]))$uploaded_at) + (365*24*60*60))) 
  assign(tablesExport[i], filter(eval(parse(text = tablesExport[i])), !(exclDate > Sys.time() & !(private %in% c(0, NA))) & excluded %in% c(0, NA)))
  assign(tablesExport[i], filter_at(eval(parse(text = tablesExport[i])), vars(starts_with("site_country")), all_vars(. != "Antarctica ")))
  assign(tablesExport[i], select(eval(parse(text = tablesExport[i])), -exclDate, -excluded, -export_time, -private))
  
  incProgress(amount = 0.05, message = "Receiving data updates", detail = "This may take a few minutes") #Progress indicator increment
  }
  
	incProgress(amount = 0.05, message = "Cleaning data", detail = "") #Progress indicator increment
	export_focus_group$site_country <- trimws(export_focus_group$site_country)
	export_focus_group$farmSizeU1prop <- ifelse(export_focus_group$focus_group_threshold_small_farm_ha <= 1 & export_focus_group$focus_group_threshold_large_farm_ha <=1, rowSums(export_focus_group[, c("focus_group_percent_households_small", "focus_group_percent_households_medium")], na.rm = T), 
                                            export_focus_group$focus_group_percent_households_small)
	#matchFGD <- export_feed_source_availability[!duplicated(export_feed_source_availability), c("focus_group_id", "focus_group_community")], focus_group_id)

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
	export_core_context_attribute_score$site_name  <- str_to_sentence(export_core_context_attribute_score$site_name)   
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
	export_core_context_attribute_score$site_name  <- trimws(export_core_context_attribute_score$site_name)   
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
	
	###Add spatial data to project_site and focus_group
		library(sf)
		library(raster)
		library(rgdal)
	
		if(file.exists('/var/www/html/webroot/rscripts/spatial/glps_gleam_61113_10km.tif') & file.exists('/var/www/html/webroot/rscripts/spatial/glps_gleam_61113_10km.tif')){
			glps <- raster('/var/www/html/webroot/rscripts/spatial/glps_gleam_61113_10km.tif')
			
			tlu <- raster('/var/www/html/webroot/rscripts/spatial/TLU_2010_Aw_ha.tif')
			
			glpsLegend <- data.frame(sp_livestock_system_id = 1:15, sp_livestock_system = c("Livestock only systems HyperArid", "Livestock only systems Arid", "Livestock only systems Humid", "Livestock only systems Temperate (and Tropical Highlands)", "Mixed rainfed HyperArid", "Mixed rainfed Arid", "Mixed rainfed Humid", "Mixed rainfed Temperate (and Tropical Highlands)", "Mixed irrigated HyperArid", "Mixed irrigated Arid", "Mixed irrigated Humid", "Mixed irrigated Temperate (and Tropical Highlands)", "Urban areas", "Other_Tree based systems", "Unsuitable"))
		
			export_focus_group$sp_livestock_system_id <- NA
			export_focus_group$sp_TLU_ha_2010_Aw <- NA
			for(i in 1:nrow(export_focus_group)){
				if(!is.na(export_focus_group$geo_json[i]) & substr(export_focus_group$geo_json[i], 1, 10) == '{\"type\":\"F'){
					if(st_is_valid(st_read(export_focus_group$geo_json[i], quiet = T)) == TRUE & !is.na(st_is_valid(st_read(export_focus_group$geo_json[i], quiet = T)))){
						
							s1 <- st_read(export_focus_group$geo_json[i], quiet = T)
							
							export_focus_group$sp_TLU_ha_2010_Aw[i] <- round(as.numeric(extract(tlu, s1, fun = mean)), 2)
							
							export_focus_group$sp_livestock_system_id[i] <- as.numeric(extract(glps, s1, fun = max))
							
							export_focus_group <- left_join(export_focus_group, glpsLegend)
				
					}}
			
				}
		
		}
		detach("package:sf", unload=TRUE)
		detach("package:raster", unload=TRUE)
		detach("package:rgdal", unload=TRUE)
	

	
	incProgress(amount = 0.05, message = "Caching data", detail = "One moment, please") #Progress indicator increment

	saveRDS(data.frame(tbl(pool, "export_project_site")), paste0(persistDIR, "/NewDataCheck.rds"))

	save(list = ls()[grepl("export", ls())], file = paste0(persistDIR, "/FEASTdatCache.RDATA"))
	})#End progress indicator
}




##Further revisions to datasets

FGD <- left_join(export_focus_group, select(export_crop_cultivation, focus_group_id, focus_group_community))


##Prepare data for export and visualisation by putting in a new environment with less verbose table names
for(i in 1:length(tablesExport)){
assign(substr(tablesExport[i], 8, nchar(tablesExport[i])), eval(parse(text = tablesExport[i])), envir = env.export) #Add the table to a new environment and remove 'export_' from the start
}



####
#Observe input events to update other input options
  observeEvent(input$SI_Region,
               { 
                 updatePickerInput(session, "SI_Country",
                        choices =sort(unique((dfeast %>%
                                               filter(site_world_region %in% input$SI_Region))$site_country_name)))
               })
               

  observeEvent(input$SI_Country,
               { c_daterange <- as.Date((dfeast %>%
                                filter(site_country_name %in% input$SI_Country))$uploaded_at)
      
      updateDateRangeInput(session, "date", start = min(c_daterange)-30,
                           end = max(c_daterange)+1)
                           
      updatePickerInput(session, "SI_Site",
                        choices = sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country 
          & uploaded_at > min(c_daterange)-1 & uploaded_at < max(c_daterange)+1
          )
        )$site_name)),
        selected = sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country 
          & uploaded_at > min(c_daterange)-1 & uploaded_at < max(c_daterange)+1
          )
        )$site_name)))        
               })
             
  observeEvent(input$date,
               { c_daterange <- as.Date((dfeast %>%
                                filter(site_country_name %in% input$SI_Country))$uploaded_at)
                              
      updatePickerInput(session, "SI_Site",
                        choices = sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country 
          & uploaded_at > as.character(input$date[1]) & uploaded_at < as.character(input$date[2])
          )
        )$site_name)),
        selected = sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country 
          & uploaded_at > as.character(input$date[1]) & uploaded_at < as.character(input$date[2])
          )
        )$site_name)))        
               })


 observe({
    if (length(input$SI_Region) == 0)
    {
        updatePickerInput(session, "SI_Site",
                        choices = "",
        selected = "")
        }        
               })   

  
 observe({ #disable download button if no tables selected
    if (length(input$SI_Tables) == 0){
      #shinyjs::disable("dldSelDat_R")
	  
      
            updatePickerInput(session, "SI_TabViewSel",
                        choices = "", selected = "Project site")
        }
else {
      #shinyjs::enable("dldSelDat_R")
      
      updatePickerInput(session, "SI_TabViewSel",
                        choices = input$SI_Tables, selected = input$SI_Tables[1]) #Selected is the first choice of a SI_Tables session
        }

  })  

#Hide download button until download data is ready - @!not working

  observeEvent(input$SI_Region,
               { 
                 shinyjs::enable("dldAllDat_R")
				 shinyjs::show("dldSelDat_R")
				 shinyjs::enable("dldSelDat_R")
				 
               }, ignoreNULL = FALSE, once = TRUE)

####
#Reactive to prepare all table data
All_data <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

    req(input$SI_TabViewAll)
  
    dataOut <- NA

    dataOut <- eval(parse(text = tablesInputDisp$tabFull[tablesInputDisp$tabLab == input$SI_TabViewAll]))
      #dataOut$site_country[dataOut$site_country == "Congo The Democratic Republic Of The"] <- "DRC"

    dropCol <- which(colnames(dataOut) %in% c("user_id", "uploaded_at", "project_id", "project_description", "project_partner_organization", "site_id", "site_world_region_id", "site_country_id", "focus_group_id", "respondent_id", "respondent_head_of_household_gender_id", "crop_type_id", "unit_area_id", "unit_mass_weight_id", "site_grazing_metabolisable_energy", "site_grazing_crude_protein_percentage", "site_collected_fodder_metabolisable_energy", "site_collected_fodder_crude_protein_percentage", "loc_json", "geo_json"))
	
	dataOut <- select(dataOut, -dropCol)
    
    return(dataOut)
  })

#Reactive to prepare selected table data when no tables selected
Selected_dataAllTab <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

    #req(input$date)
    req(input$SI_TabViewSelCountry)
    #validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    #validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    dataOutSel <- NA

    #if (length(input$SI_Table) > 0){
      #dataOut$site_country[dataOut$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
      dataOutSel <- eval(parse(text = tablesInputDisp$tabFull[tablesInputDisp$tabLab == input$SI_TabViewSelCountry]))
      if(input$SI_TabViewSelCountry == "Project site"){
      dataOutSel <- dataOutSel %>% filter( site_country_name %in% input$SI_Country & site_name %in% input$SI_Site)
      } else {dataOutSel <- dataOutSel %>%
        filter(
          site_country %in% input$SI_Country & site_name %in% input$SI_Site  #&
       #   created_at > as.character(input$date[1]) & created_at < as.character(input$date[2])
        )
        }
    #}
	
	dropCol <- which(colnames(dataOutSel) %in% c("user_id", "uploaded_at", "project_id", "project_description", "project_partner_organization", "site_id", "site_world_region_id", "site_country_id", "focus_group_id", "respondent_id", "respondent_head_of_household_gender_id", "crop_type_id", "unit_area_id", "unit_mass_weight_id", "site_grazing_metabolisable_energy", "site_grazing_crude_protein_percentage", "site_collected_fodder_metabolisable_energy", "site_collected_fodder_crude_protein_percentage", "loc_json", "geo_json"))
	
	dataOutSel <- select(dataOutSel, -dropCol)
    
    return(dataOutSel)
  })
  
#Reactive to prepare selected table data 
Selected_data <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

    #req(input$date)
    req(input$SI_TabViewSel)
    #validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    #validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    dataOutSel <- NA

    #if (length(input$SI_Table) > 0){
      #dataOut$site_country[dataOut$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
      dataOutSel <- eval(parse(text = tablesInputDisp$tabFull[tablesInputDisp$tabLab == input$SI_TabViewSel]))
      if(input$SI_TabViewSel == "Project site"){
      dataOutSel <- dataOutSel %>% filter( site_country_name %in% input$SI_Country & site_name %in% input$SI_Site
      )
      } else {dataOutSel <- dataOutSel %>%
        filter(
          site_country %in% input$SI_Country & site_name %in% input$SI_Site  #&
       #   created_at > as.character(input$date[1]) & created_at < as.character(input$date[2])
        )
        }
    #}
	
	dropCol <- which(colnames(dataOutSel) %in% c("user_id", "uploaded_at", "project_id", "project_description", "project_partner_organization", "site_id", "site_world_region_id", "site_country_id", "focus_group_id", "respondent_id", "respondent_head_of_household_gender_id", "crop_type_id", "unit_area_id", "unit_mass_weight_id", "site_grazing_metabolisable_energy", "site_grazing_crude_protein_percentage", "site_collected_fodder_metabolisable_energy", "site_collected_fodder_crude_protein_percentage", "loc_json", "geo_json"))
	
	dataOutSel <- select(dataOutSel, -dropCol)
    
    return(dataOutSel)
  })




####
#Main output tables

output$default_table <- DT::renderDataTable(

            All_data(), rownames = FALSE, selection = 'none',
            options = list(#lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
			dom = 'Bfrtip',
			scrollX = TRUE,
			scrollY = FALSE,
          pageLength = 5)
            #dom = 't', paging = FALSE) #Ref@https://datatabl
 )

output$selected_tableAllTab <- DT::renderDataTable(

            Selected_dataAllTab(), rownames = FALSE, selection = 'none',
            options = list(#lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
			dom = 'Bfrtip',
			scrollX = TRUE,
			scrollY = FALSE,
          pageLength = 5)
			#dom = 't', paging = FALSE) #Ref@https://datatabl
 )

output$selected_table <- DT::renderDataTable(

            Selected_data(), rownames = FALSE, selection = 'none',
            options = list(#lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
			dom = 'Bfrtip',
			scrollX = TRUE,
			scrollY = FALSE,
          pageLength = 5)
			#dom = 't', paging = FALSE) #Ref@https://datatabl
 )





 
############Prepare data for download within download handler

output$dldAllDat_R <- downloadHandler(
  

   filename = function() {
   	if(input$DATA_FormatAll == "CSV"){
   	paste("FeastData", ".zip", sep = "")
          }
        else{
        paste("FeastData", ".", input$DATA_FormatAll, sep = "")}
   },
   
   content = function(con) {
    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

     if(input$DATA_FormatAll == "CSV") {
      withProgress(message = 'Your download is being prepared',
      detail = 'Please wait', value = 0, {
		    for(i in 1:length(tablesExport)) {
		      readr::write_csv(eval(parse(text = tablesExport[i])), paste0(cacheDIR, "/", substr(tablesExport[i], 8, nchar(tablesExport[i])), ".csv"))
          incProgress(amount = 1/length(tablesExport), message = "Generating your CSVs", detail = paste(i, "of", length(tablesExport))) #Progress indicator increment
		    }
		    csvFiles <- list.files(cacheDIR, full.names = T)[grep(paste0(".csv"), list.files(cacheDIR))]
		    zip(paste0(cacheDIR, "/FEAST", ".zip"), csvFiles, flags = "-j") #, flags = "-j"
			#unlink(csvFiles)
        file.copy(paste0(cacheDIR, "/FEAST", ".zip"), con)
		#unlink(paste0(cacheDIR, "/complete", start_time, "FEAST.zip")
      })#end of progress indicator
     }
     
     if(input$DATA_FormatAll == "XLSX") {
      withProgress(message = 'Your download is being prepared',
      detail = 'Please wait', value = 0, {
       
       wb = createWorkbook()
		    for(i in 1:length(tablesExport)) {
			    sheet = addWorksheet(wb, tablesInputDisp$tabLab[i])
			    writeData(wb, sheet=sheet, eval(parse(text = tablesExport[i]))) 			#eval(parse()) to return the object of the same name as the string
          incProgress(amount = 0.8/length(tablesExport), message = "Adding sheets to workbook", detail = paste(i, "of", length(tablesExport))) #Progress indicator increment
		  	}
        incProgress(amount = 0.01, message = "Saving workbook", detail = "Please wait") #Progress indicator increment
			  saveWorkbook(wb, paste0(cacheDIR, "/complete", start_time, "FEAST.xlsx"), overwrite = T)
        incProgress(amount = 0.19, message = "Saving workbook", detail = "Please wait") #Progress indicator increment

       file.copy(paste0(cacheDIR, "/complete", start_time, "FEAST.xlsx"), con)

       
      })#end of progress indicator
     }
     
     if(input$DATA_FormatAll == "RDATA") {
       #file.copy(paste0(cacheDIR, "/complete", start_time, "FEAST.RDATA"), con)
       save(list = ls(env.export)[!grepl("Sub", ls(env.export))], file = con, envir = env.export) #All files already in env. Imported in global.R #ls(env.export)
     }
	 #Remove temporary files
	 unlink(list.files(cacheDIR, full.names = T))

   }


   #}
 )

 

output$dldSelDat_R <- downloadHandler(
   filename = function() {
   	if(input$DATA_FormatSel == "CSV"){
   	paste("FeastSub", ".zip", sep = "")
          }
        else{
        paste("FeastSub", ".", input$DATA_FormatSel, sep = "")}
   },
   
   content = function(con) {
    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

     if(input$DATA_FormatSel == "CSV") {
       withProgress(message = 'Your download is being prepared',
       detail = 'Please wait', value = 0, {
       for(i in 1:length(tablesExport)) {
		    if(tablesExport[i] %in% tablesInputDisp$tabFull[tablesInputDisp$tabLab %in% input$SI_Tables] || length(input$SI_Tables) == 0){ #Only incorporate tables selected by the user. Default is all selected
		      tmpExportTab <- eval(parse(text = tablesExport[i])) #eval(parse()) to return the object of the same name as the string
		    if("site_country" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   	      tmpExportTab <- tmpExportTab %>% filter(site_country %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. 
	  		 }
	  	
	  	  if("site_country_name" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   	    tmpExportTab <- tmpExportTab %>% filter(site_country_name %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. 
	  		}

		readr::write_csv(tmpExportTab, paste0(cacheDIR, "/", substr(tablesExport[i], 8, nchar(tablesExport[i])), ".csv"))

		}
    incProgress(amount = 1/length(input$SI_Tables), message = "Generating your CSVs") #Progress indicator increment
		}
		csvFiles <- list.files(cacheDIR, full.names = T)[grep(paste0(".csv"), list.files(cacheDIR))]
		zip(paste0(cacheDIR, "/",  "FEASTsub", ".zip"), csvFiles, flags = "-j") #, flags = "-j"
       file.copy(paste0(cacheDIR, "/FEASTsub", ".zip"), con)
      })#end of progress indicator
     }
     
     if(input$DATA_FormatSel == "XLSX") {
      withProgress(message = 'Your download is being prepared',
      detail = 'Please wait', value = 0, {
      wb = createWorkbook()
	    for(i in 1:length(tablesExport)) {#! Fix disjointed tables Export and tabLab. Bring into one DF and use consistently throughout
			  if(tablesExport[i] %in% tablesInputDisp$tabFull[tablesInputDisp$tabLab %in% input$SI_Tables] || length(input$SI_Tables) == 0){ #Only incorporate tables selected by the user. Default is all selected
			    tmpExportTab <- eval(parse(text = tablesExport[i])) #eval(parse()) to return the object of the same name as the string

	  		if("site_country" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   		  tmpExportTab <- tmpExportTab %>% filter(site_country %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. # 
	  		 }

	  		if("site_country_name" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   		  tmpExportTab <- tmpExportTab %>% filter(site_country_name %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. # 

          incProgress(amount = 0.8/length(input$SI$Tables), message = "Adding sheets to workbook") #Progress indicator increment
	  		 }
			sheet = addWorksheet(wb, tablesInputDisp$tabLab[i])
			writeData(wb, sheet=sheet, tmpExportTab) 
			}
			}
			            
      incProgress(amount = 0.01, message = "Saving workbook", detail = "Please wait") #Progress indicator increment
			saveWorkbook(wb, paste0(cacheDIR, "/selected", start_time, "FEAST.xlsx"), overwrite = T)
      incProgress(amount = 0.19, message = "Saving workbook", detail = "Ready") #Progress indicator increment
      file.copy(paste0(cacheDIR, "/selected", start_time, "FEAST.xlsx"), con)
      })#end of progress indicator 
     }
     
     if(input$DATA_FormatSel == "RDATA") {
      withProgress(message = 'Your download is being prepared',
      detail = 'Please wait', value = 0, {
      dflis <- list()
	#Prepare data 	
	  for(i in 1:length(tablesExport)){ 
	    if(tablesExport[i] %in% tablesInputDisp$tabFull[tablesInputDisp$tabLab %in% input$SI_Tables] || length(input$SI_Tables) == 0){ #Only incorporate tables selected by the user. Default is all selected
	      tmpExportTab <- eval(parse(text = tablesExport[i]))
        if("site_country" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   		  tmpExportTab <- tmpExportTab %>% filter(site_country %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. # 
	  		  }

	  		if("site_country_name" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   		  tmpExportTab <- tmpExportTab %>% filter(site_country_name %in% input$SI_Country & site_name %in% input$SI_Site)  #SI_Site already filtered by date. # 
	  		  }
	    assign(paste0(substr(tablesExport[i], 8, nchar(tablesExport[i]))), tmpExportTab, envir = env.export.sub) 		 
		  }
		incProgress(amount = 1/length(input$SI$Tables), message = "Adding dataframes to RDATA file") #Progress indicator increment
		}
    #save(list = ls(env.export)[grepl("Sub", ls(env.export))], file = paste0(cacheDIR, "/selected", start_time, "FEAST.RDATA"), envir = env.export)
	 save(list = ls(env.export.sub), file = con, envir = env.export.sub)
      #file.copy(paste0(cacheDIR, "/selected", start_time, "FEAST.RDATA"), con)
       })#end of progress indicator
      }
	  #Remove temporary files
	  unlink(list.files(cacheDIR, full.names = T))
  }
  )

 
}
