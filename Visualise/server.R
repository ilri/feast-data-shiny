function(input, output, session) {

#@Raw data prepration is in global file.

session$allowReconnect(TRUE) #Make sure server reconnects rather than grey out


#Initialise persist data directory
if(!dir.exists(file.path("/tmp/FEASTpersist"))) { 
  dir.create(file.path("/tmp/FEASTpersist"))
  } #Check if cache data directory exists and if not, create it.

#Initialise cache data directory
if(!dir.exists(file.path("/tmp/FEASTdata"))) { 
  dir.create(file.path("/tmp/FEASTdata"))
  } #Check if cache data directory exists and if not, create it.

persistDIR <- "/tmp/FEASTpersist" #file.access returns 0 if has permission to write or -1 if not
cacheDIR <- "/tmp/FEASTdata" #ifelse(as.logical(file.access(".", 2) == 0), "FEASTdata", 


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




	#Farm size
	export_focus_group$site_country <- trimws(export_focus_group$site_country)
	export_focus_group$farmSizeU1prop <- ifelse(export_focus_group$focus_group_threshold_small_farm_ha <= 1 & export_focus_group$focus_group_threshold_large_farm_ha <=1, rowSums(export_focus_group[, c("focus_group_percent_households_small", "focus_group_percent_households_medium")], na.rm = T), 
												export_focus_group$focus_group_percent_households_small)
	#matchFGD <- export_feed_source_availability[!duplicated(export_feed_source_availability), c("focus_group_id", "focus_group_community")], focus_group_id)



	#Livestock
	export_livestock_holding$site_country <- trimws(export_livestock_holding$site_country)
	export_livestock_holding$livestock_holding_dominant_breed <- str_to_sentence(export_livestock_holding$livestock_holding_dominant_breed)
	export_livestock_holding$livestock_holding_dominant_breed <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", export_livestock_holding$livestock_holding_dominant_breed, perl=TRUE) #Remove more than one space in a row
	export_livestock_holding$livestock_holding_dominant_breed <- trimws(export_livestock_holding$livestock_holding_dominant_breed)
	export_livestock_holding$livestock_holding_dominant_breed <- gsub(",([A-Za-z])", ", \\1", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma
	export_livestock_holding$livestock_holding_dominant_breed <- gsub("Collected fooder", "Collected fodder", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma
	export_livestock_holding$livestock_holding_dominant_breed <- gsub("Collect fodder", "Collected fodder", export_livestock_holding$livestock_holding_dominant_breed) #Add space to cases where a character follows a comma


	#Crops
	export_crop_cultivation$site_country <- trimws(export_crop_cultivation$site_country)



	#Fodder
	export_fodder_crop_cultivation$site_country <- trimws(export_fodder_crop_cultivation$site_country)
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

FGD <- left_join(export_focus_group, select(export_crop_cultivation, site_name, focus_group_id, focus_group_community))

farm_size <- gather(select(FGD, site_country, focus_group_community, site_name, focus_group_id, Landless = focus_group_percent_households_landless, U1ha = farmSizeU1prop, Large = focus_group_percent_households_large), farmSize, proportion, c(Landless, U1ha, Large))
farm_size$proportion[is.na(farm_size$proportion)] <- 0
farm_size$farmSize[farm_size$farmSize == "U1ha"] <- "Under 1 ha"


crop_utilisation <- gather(select(export_crop_cultivation, site_country, site_name, focus_group_id, focus_group_community, crop_type_name, crop_cultivation_percent_fed, crop_cultivation_percent_burned, crop_cultivation_percent_mulched, crop_cultivation_percent_sold, crop_cultivation_percent_other), use, percentage, c(crop_cultivation_percent_fed, crop_cultivation_percent_burned, crop_cultivation_percent_mulched, crop_cultivation_percent_sold, crop_cultivation_percent_other))
crop_utilisation$use <- gsub("crop_cultivation_percent_", "", crop_utilisation$use)
crop_utilisation$use <- str_to_sentence(crop_utilisation$use)
#crop_utilisation$focus_group_community <- str_to_sentence(crop_utilisation$focus_group_community)
crop_utilisation$cropName <- gsub("\\s*\\([^\\)]+\\)","", crop_utilisation$crop_type_name)

export_livestock_holding$cattleBreeds <- ifelse(export_livestock_holding$livestock_holding_dominant_breed %in% c("L0cal zebu", "Llocal", "Loal", "Local", "Local'", "Local + improved", "Local and exotic", "Local beed", "Local beed (ri)", "Local breed", "Local breed (ri)", "Local breed\\", "Local east africa", "Local east african", "Local improved", "Local zebu", "Local zeu", "Locl", "Locsl", "Zebu", "Zebu (local)", "Zebu/ayrshire", "Indegenous", "Indeginous", "Indigeneous", "Indigenous", "Indigenousl", "Indigineous", "Indiginous", "Indiginous/galla cross"), "Local breed",
                                                ifelse(export_livestock_holding$livestock_holding_dominant_breed %in% c("Arshire", "Arshyre", "Aryshire", "Aryshire cross", "Aryshire/fresian", "Ashiers", "Ayshire"), "Ayshire", 
                                                       ifelse(substr(export_livestock_holding$livestock_holding_dominant_breed, 1, 1) == "F", "Friesian", 
                                                              ifelse(substr(export_livestock_holding$livestock_holding_dominant_breed, 1, 1) == "J", "Jersey", "Other"))))
export_livestock_holding$cattleBreeds[is.na(export_livestock_holding$cattleBreeds)] <- "Other"

export_livestock_holding$TLU <- (export_livestock_holding$livestock_holding_average_weight/250) * export_livestock_holding$livestock_holding_headcount
livestock_holding <- group_by(export_livestock_holding, site_country, site_name, focus_group_id, focus_group_community)
livestock_holding <- summarise(livestock_holding, TLU = sum(TLU, na.rm=T))

export_feed_source_availability$feedCategory <- ifelse(export_feed_source_availability$feed_source_description %in% c("Acacia ponds, bean tops", "Cereal crop residues", "Leguminous crop residues"), "Crop residues", 
                                                        ifelse(export_feed_source_availability$feed_source_description %in% c("Collected fodder", "Fodder", "Green forage (e.g., weeds, fodder crops, leaves"), "Green forage",
                                                               ifelse(export_feed_source_availability$feed_source_description == "Grazing", "Grazing",
                                                               ifelse(export_feed_source_availability$feed_source_description %in% c("Concentrates", "Pastures, fodder, concertrates, animal by products and industrial by products"), "Concentrates", "Other"))))


###########################################

##Prepare data for export and visualisation by putting in a new environment with less verbose table names
for(i in 1:length(tablesExport)){
assign(substr(tablesExport[i], 8, nchar(tablesExport[i])), eval(parse(text = tablesExport[i])), envir = env.export) #Add the table to a new environment and remove 'export_' from the start
}


####
####
#Observe input events to update other input options
#Observe input events to update other input options
  observeEvent(input$SI_Region,
               { 
                 updatePickerInput(session, "SI_Country",
                        choices =sort(unique((dfeast %>%
                                               filter(site_world_region %in% input$SI_Region))$site_country_name)))
               })


 observeEvent(input$SI_Country,
               { 
                              
      updatePickerInput(session, "SI_Site",
                        choices = sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country
          )
        )$site_name)),
        selected = sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country
          )
        )$site_name))[1:2])  
		
		c_daterange <- as.Date((dfeast %>%
                                filter(site_country_name %in% input$SI_Country))$uploaded_at)
		
		updateDateRangeInput(session, "date", start = min(c_daterange)-30,
                           end = max(c_daterange)+1)
						   
               })
			   

siteOut <- eventReactive(input$SI_Site,{                             
	sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country & site_name %in% input$SI_Site
          )
        )$site_name)) 
	
	})
	
countryOut <- eventReactive(input$SI_Site,{                             
	sort(unique((dfeast %>% filter(
          site_country_name %in% input$SI_Country & site_name %in% input$SI_Site
          )
        )$site_country_name)) 
	
	})
 
   observeEvent(input$date,
               { c_daterange <- as.Date((dfeast %>%
                                filter(site_country_name %in% input$SI_Country))$uploaded_at)
                              
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
        )$site_name))[1:2])    
 
               })            

               
         
observe({ #disable download button if no tables selected
    if (input$SI_Tables == "Map of sites"){
      shinyjs::disable("download_visSel")
      shinyjs::disable("download_visAll")
          
        }
else {
      shinyjs::enable("download_visSel")
      shinyjs::enable("download_visAll")
      
        }

  })  



####
#Reactive to prepare filtered visualisations
#Boxplots https://www.r-graph-gallery.com/266-ggplot2-boxplot-with-variable-width
  All_vis <- reactive({

  visOutAll <- NA
        if(input$SI_Tables == "Farm size"){
          farm_size <- farm_size[farm_size$site_country != "Antarctica",]
          farm_size$site_country[farm_size$site_country %in% c("Democratic Republic of the Congo", "Congo The Democratic Republic Of The")] <- "DRC"
          #nlab <- paste0(unique(farm_size$site_country[!is.na(farm_size$site_country)]), "\n(n =", table(farm_size$site_country[!is.na(farm_size$site_country)]),")")
		  nlab <- group_by(farm_size, site_country)
		  nlab <- summarise(nlab, lab = length(farmSize))
		  nlab$lab <- paste0("n=", nlab$lab)
		  nlab$yPos <- rep(0, length(nlab$lab))+c(100,110)

          visOutAll <- ggplot(farm_size[!is.na(farm_size$site_country),], aes(site_country, proportion, colour = farmSize)) + geom_boxplot() + annotate("text", x = nlab$site_country, y = nlab$yPos, label = nlab$lab) + labs(colour = "Farm size", subtitle = "Farm size variability across sites and between countries (% of households in category - median and IQR of sites)") + xlab("Country") + ylab("Farm size (% of category)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(size = 9, angle=45, hjust=1, vjust=1), axis.text.y = element_text(size = 9), plot.background = element_rect(fill = "transparent", colour = NA), legend.background = element_rect(fill = "transparent", colour = NA))
			# + scale_x_discrete(labels=nlab) 
			#geom_text(aes(label = lab, x = site_country, y = 100, colour = "black"))
			
        } else if(input$SI_Tables == "Feed availability"){
      #  if(input$SI_Tables == "Feed availability"){
        #Stacked bar plot of category proportions and line of monthly availability
          feed_availability_prop <- group_by(export_feed_source_availability, site_country, month_id, feedCategory) #site_id, site_name, 
          feed_availability_prop <- summarise(feed_availability_prop, percentage = mean(percentage, na.rm=T))
          feed_availability_prop$site_country[feed_availability_prop$site_country %in% c("Democratic Republic of the Congo", "Congo The Democratic Republic Of The")] <- "DRC"
          feed_availability_score <- group_by(export_feed_source_availability, site_country, month_id) #site_country, site_id,
          feed_availability_score <- summarise(feed_availability_score, feedAvailability = mean(feed_availability, na.rm=T), u75 = quantile(feed_availability, probs = 0.75, na.rm = T), l25 = quantile(feed_availability, probs = 0.25, na.rm = T))
          range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm=T)-min(x, na.rm = T))}
          feed_availability_score$feedAvailability <- range01(feed_availability_score$feedAvailability) #Rescale between 0 and 1
          feed_availability_score$u75 <- range01(feed_availability_score$u75)
          feed_availability_score$l25 <- range01(feed_availability_score$l25)
          feed_availability_score <- feed_availability_score[feed_availability_score$site_country != "Antarctica",]
          feed_availability_score$site_country[feed_availability_score$site_country %in% c("Democratic Republic of the Congo", "Congo The Democratic Republic Of The")] <- "DRC"
          
          #@Temp hack for demonstration
          feed_availability_score$u75 <- ifelse(feed_availability_score$u75 < feed_availability_score$feedAvailability, feed_availability_score$feedAvailability, feed_availability_score$u75)
          feed_availability_score$l25 <- ifelse(feed_availability_score$l25 > feed_availability_score$feedAvailability, feed_availability_score$feedAvailability, feed_availability_score$l25)
          
          feed_availability_score$u75 <- ifelse(feed_availability_score$u75 > 1, 1, feed_availability_score$u75)
          feed_availability_score$l25 <- ifelse(feed_availability_score$l25 < 0, 0, feed_availability_score$l25)
          feed_availability_score$Month <- feed_availability_score$month_id

          feed_availability_prop$Proportion <- feed_availability_prop$percentage
          feed_availability_prop$Month <- factor(month.abb[feed_availability_prop$month_id], levels = month.abb)
		  feed_availability_prop$`Feed category` <- feed_availability_prop$feedCategory
          
          visOutAll <- ggplot(feed_availability_prop) + geom_bar(aes(Month, Proportion, fill = `Feed category`), position = "fill", stat = "identity") + geom_ribbon(data= feed_availability_score, aes(x=month_id , ymin = u75, ymax = l25), fill = "grey90", alpha = 0.8) + geom_line(data= feed_availability_score, aes(x=month_id, y=feedAvailability)) + facet_wrap(~ site_country, scales = "fixed", ncol = 3) + ylab("") + labs(fill = "Feed category", subtitle = "Feed availability (black line with IQR, scaled between 0 and 1)") + xlab("Month") + themePublication() + scaleFillPublication() + theme(axis.text.x = element_text(size = 9, angle=45, hjust=1, vjust=1), axis.text.y = element_text(size = 9), plot.background = element_rect(fill = "transparent", colour = NA), legend.background = element_rect(fill = "transparent", colour = NA)) #Other lab options: , subtitle = , caption =

        } #else if(input$SI_Tables == "Liveweight"){
          #export_livestock_holding$site_country[export_livestock_holding$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          #export_livestock_holding <- export_livestock_holding[export_livestock_holding$animal_species_description == "Cattle",]
          #export_livestock_holding <- export_livestock_holding[!is.na(export_livestock_holding$site_country) & !is.na(export_livestock_holding$cattleBreeds),]

          #visOutAll <- ggplot(export_livestock_holding) + geom_boxplot(aes(cattleBreeds, livestock_holding_average_weight)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Breed") + ylab("Liveweight (kg)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(hjust=1, vjust=1)) #, width = 3/length(unique(export_livestock_holding$cattleBreeds))

        #} else if(input$SI_Tables == "Livestock holdings"){
          #livestock_holding$site_country[livestock_holding$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          #visOutAll <- ggplot(livestock_holding[!is.na(livestock_holding$site_country) & livestock_holding$site_country %in% livestock_holding$site_country[!is.na(livestock_holding$TLU)],]) + geom_boxplot(aes(site_country, TLU)) + xlab("Country") + ylab("Livestock holdings (TLU = 250 kg)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "Distance to market"){
          #export_focus_group$site_country[export_focus_group$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          #visOutAll <- ggplot(export_focus_group[!is.na(export_focus_group$site_country) & export_focus_group$site_country %in% export_focus_group$site_country[!is.na(export_focus_group$focus_group_market_avg_distance_km)],]) + geom_boxplot(aes(site_country, focus_group_market_avg_distance_km)) + xlab("Country") + ylab("Distance to market (km)") + coord_cartesian(ylim = c(0, 20)) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "AI uptake"){
          #export_focus_group$site_country[export_focus_group$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          #visOutAll <- ggplot(export_focus_group[!is.na(export_focus_group$site_country) & export_focus_group$site_country %in% export_focus_group$site_country[!is.na(export_focus_group$focus_group_percent_reproduction_ai)],]) + geom_boxplot(aes(site_country, focus_group_percent_reproduction_ai)) + xlab("Country") + ylab("Cattle keepers using AI (%)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "Fodder yield"){
          #export_fodder_crop_cultivation$site_country[export_fodder_crop_cultivation$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          #visOutAll <- ggplot(export_fodder_crop_cultivation[!is.na(export_fodder_crop_cultivation$site_country) & !is.na(export_fodder_crop_cultivation$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_type_annual_dry_matter_per_hectare)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Fodder") + ylab("Yield (DM ha)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "Fodder area cultivated"){
          #export_fodder_crop_cultivation$site_country[export_fodder_crop_cultivation$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          #visOutAll <- ggplot(export_fodder_crop_cultivation[!is.na(export_fodder_crop_cultivation$site_country) & !is.na(export_fodder_crop_cultivation$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_cultivation_cultiavted_land_ha)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Fodder") + ylab("Area cultivated (ha)") + coord_cartesian(ylim = c(0, 3)) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} 
		else if(input$SI_Tables == "Crop utilisation"){

          crop_utilisation$site_country[crop_utilisation$site_country %in% c("Democratic Republic of the Congo", "Congo The Democratic Republic Of The")] <- "DRC"
		  
		  crop_utilisation <- crop_utilisation[crop_utilisation$cropName %in% c("Cowpea", "Groundnut", "Sorghum", "Banana", "Rice", "Sweet potato", "Cassava", "Soy bean", "Wheat", "Pearl Millet", "Potato"), ]

          crop_utilisation_country <- group_by(crop_utilisation, site_country, cropName, use)
          crop_utilisation_country <- summarise(crop_utilisation_country, Proportion = mean(percentage, na.rm = T))
		  crop_utilisation_country$`Crop name` <- crop_utilisation_country$cropName
		  crop_utilisation_country$Use <- crop_utilisation_country$use
		  crop_utilisation_country$Proportion <- round(crop_utilisation_country$Proportion, 2)
          visOutAll <- ggplot(crop_utilisation_country) + geom_bar(aes(`Crop name`, Proportion, fill = Use), position = "fill", stat = "identity") + facet_wrap(~site_country, scales = "free_x", ncol = 3) + xlab("Crop") + ylab("") + labs(fill = "Usage", subtitle = "Residue usage by crop type (proportion)") + themePublication() + theme(axis.text.x = element_text(size = 9, angle=45, hjust=1, vjust=1), axis.text.y = element_text(size = 9), panel.spacing.y = unit(1, "line"), plot.background = element_rect(fill = "transparent", colour = NA), legend.background = element_rect(fill = "transparent", colour = NA)) + scale_fill_manual(values = c("red", "seagreen3", "orange", "grey", "gold")) 
          
        }


    return(visOutAll)
})

Selected_vis <- reactive({
	req(siteOut)
    feastDat <- NA
    visOut <- ggplot(data.frame()) + theme_void()+ geom_text(aes(0, 0, label = "No data for these sites")) + theme(plot.background = element_rect(fill = "transparent", colour = NA), legend.background = element_rect(fill = "transparent", colour = NA))
	
	  feastDat <- eval(parse(text = tablesInputPlot$plotDFs[tablesInputPlot$tabLab == input$SI_Tables]))
	  
	  #if(is.null(nrow(feastDat[feastDat$site_country %in% countryOut() & feastDat$site_name %in% siteOut(),]))){ 
	  #		visOut <- ggplot(data.frame()) + theme_void()+ geom_text(aes(0, 0, label = "No data for these sites"))
	  #	}
	  
	  if(length(input$SI_Country) > 0 & input$SI_Tables == "Farm size"){
      #feastDat <- eval(parse(text = tablesInputPlot$plotDFs[tablesInputPlot$tabLab == input$SI_Tables]))
      feastDat <- feastDat %>% filter(
          site_country %in% input$SI_Country
        )
		
      feastDat$site_country[feastDat$site_country %in% c("Democratic Republic of the Congo", "Congo The Democratic Republic Of The")] <- "DRC"
	  
            #nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
		  nlab <- group_by(feastDat, site_country)
		  nlab <- summarise(nlab, lab = length(farmSize))
		  nlab$lab <- paste0("n=", nlab$lab)
		  if(length(nlab$lab) < 9) {
			nlab$yPos <- 110}
		  else{
			nlab$yPos <- rep(0, length(nlab$lab))+c(100,110)
		  }
          visOut <- ggplot(feastDat[!is.na(feastDat$site_country),], aes(site_country, proportion, colour = farmSize)) + geom_boxplot(width = 1) + annotate("text", x = nlab$site_country, y = nlab$yPos, label = nlab$lab) + labs(colour = "Farm size", subtitle = "Farm size variability (% of category)") + xlab("Country") + ylab("Farm size (% of category)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(size = 9, angle=45, hjust=1, vjust=1), axis.text.y = element_text(size = 9), plot.background = element_rect(fill = "transparent", colour = NA), legend.background = element_rect(fill = "transparent", colour = NA))
		}		
		
    else if(length(countryOut()) > 0 & length(siteOut()) > 0 & input$SI_Tables != "Farm size" & sum(siteOut() %in% feastDat$site_name[feastDat$site_country %in% countryOut()])>0 & !is.null(nrow(feastDat[feastDat$site_country %in% countryOut() & feastDat$site_name %in% siteOut(),]))){
      
      feastDat <- feastDat %>% filter(
          site_country %in% countryOut() & site_name %in% siteOut()
        )
		
		feastDat$site_country[feastDat$site_country %in% c("Democratic Republic of the Congo", "Congo The Democratic Republic Of The")] <- "DRC"
		
		if(input$SI_Tables == "Feed availability" & nrow(feastDat) > 0){
            #Stacked bar plot of category proportions 
			
			feastDat$site_name <- str_to_sentence(feastDat$site_name)
			

            feed_availability_prop <- group_by(feastDat, site_country, site_name, month_id, feedCategory) #focus_group_id, focus_group_community,
            feed_availability_prop <- summarise(feed_availability_prop, percentage = mean(percentage, na.rm=T), feedAvailability = mean(feed_availability, na.rm = T))
            
            feed_availability_prop <- arrange(feed_availability_prop, site_country, site_name, month_id)
            
            #range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm=T)-min(x, na.rm = T))}
            #feed_availability_prop$feedAvailability <- range01(feed_availability_prop$feedAvailability) #Rescale between 0 and 1
			#feed_availability_prop$feedAvailability <- round(feed_availability_prop$feedAvailability, 1)

            #feed_availability_prop$Proportion <- feed_availability_prop$percentage/10
			feed_availability_prop$percentage <- round(feed_availability_prop$percentage, 0)
			
			feed_availability_prop$Month <- factor(month.abb[feed_availability_prop$month_id], levels = month.abb)
			feed_availability_prop$`Feed category` <- feed_availability_prop$feedCategory
			
			##@! To approximate the proportons of feed category - duplicate rows based on the percentage value, where "percentage" in the DB is between 0 and 10. This percentage value is averaged across respondents
			feed_availability_propOut <- feed_availability_prop[0,]
			for(i in 1:nrow(feed_availability_prop)){
				if(feed_availability_prop$percentage[i] != 0){
					feed_availability_propOut <- rbind(feed_availability_propOut, feed_availability_prop[rep(i, feed_availability_prop$percentage[i]),])
				}
			}
			
			feed_availability_propOut <- group_by(feed_availability_propOut, site_country, site_name, month_id)
			feed_availability_propOut <- add_count(feed_availability_propOut)
			feed_availability_propOut$`Feed availability` <- feed_availability_propOut$feedAvailability/feed_availability_propOut$n
            #visOut <- ggplot(feed_availability_prop) + geom_bar(aes(month_id, Proportion, fill = feedCategory), position = "fill", stat = "identity") + geom_line(data= feed_availability_score, aes(x=month_id, y=feedAvailability)) + facet_wrap(~site_country + site_name, ncol =1) + scale_x_continuous(breaks = c(1:12)) + xlab("Month") + ylab("") + labs(fill = "Feed category") + themePublication() + scale_fill_manual(values = c("Concentrates" = "blue", "Crop residues" = "orange",  "Grazing" = "seagreen3", "Green forage" = "red", "Other" = "gold")) #Other lab options: , subtitle = , caption =
            
			visOut <- ggplot(feed_availability_propOut) + geom_bar(aes(Month, `Feed availability`, fill = `Feed category`), position = "stack", stat = "identity") + facet_wrap(~site_country + site_name, ncol =2)  + ylab("") + xlab("") + labs(fill = "Feed category", subtitle = "Feed availability (scaled between 0 and 10)") + themePublication() + scale_y_continuous(trans = "identity") + scale_fill_manual(values = c("Concentrates" = "blue", "Crop residues" = "orange",  "Grazing" = "seagreen3", "Green forage" = "red", "Other" = "gold")) + theme(axis.text.x = element_text(size = 9, angle=45, hjust=1, vjust=1), axis.text.y = element_text(size = 9), panel.spacing.y = unit(1, "line"), axis.title.y = element_text(hjust=-3), plot.background = element_rect(fill = "transparent", colour = NA), legend.background = element_rect(fill = "transparent", colour = NA)) #Other lab options: , subtitle = , caption =			
			
        } #else if(input$SI_Tables == "Liveweight" & length(input$SI_Site) > 0){
          #  nlab <- paste0(unique(feastDat$cattleBreeds[!is.na(feastDat$cattleBreeds) & !is.na(feastDat$site_country)]), "\n(n =", table(feastDat$cattleBreeds[!is.na(feastDat$cattleBreeds) & !is.na(feastDat$site_country)]), ")")
          #  feastDat <- feastDat[feastDat$animal_species_description == "Cattle",]
          #  visOut <- ggplot(feastDat[!is.na(feastDat$site_country)  & !is.na(feastDat$cattleBreeds) & feastDat$animal_species_description == "Cattle",]) + geom_boxplot(aes(cattleBreeds, livestock_holding_average_weight)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Breed") + ylab("Liveweight (kg)") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(hjust=1, vjust=1)) #, width = (length(unique(feastDat$cattleBreeds)) / length(unique(export_livestock_holding$cattleBreeds)))

        #} else if(input$SI_Tables == "Livestock holdings" & length(input$SI_Site) > 0){
        #    nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
        #    #Keep consistent width with this: width = 0.2 + (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name))) 
        #    visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & feastDat$site_country %in% feastDat$site_country[!is.na(feastDat$TLU)],]) + geom_boxplot(aes(site_country, TLU), width = 0.2 + (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Country") + ylab("Livestock holdings (TLU = 250 kg)") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "Distance to market" & length(input$SI_Site) > 0){
        #    nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
        #    visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & feastDat$site_country %in% feastDat$site_country[!is.na(feastDat$focus_group_market_avg_distance_km)],]) + geom_boxplot(aes(site_country, focus_group_market_avg_distance_km), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Country") + ylab("Distance to market (km)") + scale_x_discrete(labels=nlab) + coord_cartesian(ylim = c(0, 20)) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "AI uptake" & length(input$SI_Site) > 0){
        #    nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
        #    visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & feastDat$site_country %in% feastDat$site_country[!is.na(feastDat$focus_group_percent_reproduction_ai)],]) + geom_boxplot(aes(site_country, focus_group_percent_reproduction_ai), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Country") + ylab("Cattle keepers using AI (%)") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "Fodder yield" & length(input$SI_Site) > 0){
        #    nlab <- paste0(unique(feastDat$fodderName[!is.na(feastDat$fodderName)]), "\n(n =", table(feastDat$fodderName[!is.na(feastDat$fodderName)]), ")")
        #    visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & !is.na(feastDat$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_type_annual_dry_matter_per_hectare), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Fodder") + ylab("Yield (DM ha)") + facet_wrap(~site_country, ncol = 1, scales = "free_x") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} else if(input$SI_Tables == "Fodder area cultivated" & length(input$SI_Site) > 0){
        #    nlab <- paste0(unique(feastDat$fodderName[!is.na(feastDat$fodderName)]), "\n(n =", table(feastDat$fodderName[!is.na(feastDat$fodderName)]), ")")
        #    visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & !is.na(feastDat$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_cultivation_cultiavted_land_ha), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Fodder") + ylab("Area cultivated (ha))") + coord_cartesian(ylim = c(0, 3)) + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        #} 
		else if(input$SI_Tables == "Crop utilisation" & nrow(feastDat) > 0){
          feastDat$site_name <- str_to_sentence(feastDat$site_name)
		  #@@! A hack to deal with sites of the same name and in the same country
		  #feastDat <- feastDat[!duplicated(feastDat[,c("month_id", "site_name", "site_country_id", "cropName", "use")]),]

		  feastDat <- group_by(feastDat, site_country, site_name, cropName, use)
          feastDat <- summarise(feastDat, percentage = mean(percentage, na.rm=T))
          #feastDat$site_country[feastDat$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          
		  feastDat <- feastDat[feastDat$cropName %in% c("Cowpea", "Groundnut", "Sorghum", "Banana", "Rice", "Sweet potato", "Cassava", "Soy bean", "Wheat", "Pearl Millet", "Potato"), ]
          feastDat$`Crop name` <- feastDat$cropName
		  feastDat$Use <- feastDat$use

            feastDat$Proportion <- round(feastDat$percentage, 2)
            visOut <- ggplot(feastDat) + geom_bar(aes(`Crop name`, Proportion, fill = Use), position = "fill", stat = "identity") + facet_wrap(~site_country + site_name, scales = "free_x", ncol = 2)  + ylab("") + xlab("") + labs(fill = "Usage", subtitle = "Residue usage by crop type (proportion)") + themePublication() + theme(axis.text.x = element_text(size = 9, angle=45, hjust=1, vjust=1), axis.text.y = element_text(size = 9), plot.background = element_rect(fill = "transparent", colour = NA), panel.spacing.y = unit(1, "line"), legend.background = element_rect(fill = "transparent", colour = NA)) + scale_fill_manual(values = c("red", "seagreen3", "orange", "grey", "gold")) #  #Other lab options: , subtitle = , caption =
          
          
        }
		

		
    }



    return(visOut)
  })



Facet_dat <- reactive({
    req(input$SI_Tables)
    req(All_vis)
    feastDat <- NA
    nFacets <- NA
    height <- NA
    if(input$SI_Tables != "Map of sites" & !is.na(tablesInputPlot$facetVar[tablesInputPlot$tabLab == input$SI_Tables])){
      feastDat <- eval(parse(text = tablesInputPlot$plotDFs[tablesInputPlot$tabLab == input$SI_Tables]))
      nFacets <- length(unique(feastDat[, tablesInputPlot$facetVar[tablesInputPlot$tabLab == input$SI_Tables]]))
    } else{
      nFacets <- 1
    
    } 
    

    return(nFacets)
})


Selected_facet_dat <- reactive({
    req(input$SI_Site)
    feastDat <- NA
    nFacets <- NA
	feastDat <- eval(parse(text = tablesInputPlot$plotDFs[tablesInputPlot$tabLab == input$SI_Tables]))
    if(length(siteOut()) >0 & input$SI_Tables != "Map of sites" & input$SI_Tables != "Farm size" & !is.na(tablesInputPlot$facetVar[tablesInputPlot$tabLab == input$SI_Tables]) & sum(siteOut() %in% feastDat$site_name[feastDat$site_country %in% countryOut()]) > 0 & !is.null(nrow(feastDat[feastDat$site_country %in% countryOut() & feastDat$site_name %in% siteOut(),]))){

      #deastDat <- if(input$SI_Tables %in% c("AI uptake", "Distance to market") {left_join(feastDat, select(export_crop_cultivation, focus_group_id, focus_group_community))}
      feastDat <- feastDat %>% filter(
          site_country %in% countryOut() & site_name %in% siteOut() #&
          #uploaded_at > as.character(input$date[1]) & uploaded_at < as.character(input$date[2])
      )
          nFacets <- length(unique(feastDat[, tablesInputPlot$facetVar[tablesInputPlot$tabLab == input$SI_Tables]]))

        } else {
        nFacets <- 1
        } 
    #nFacets <- ceiling(nFacets/3)*300
    return(nFacets)
})



icons <- awesomeIcons(
  icon = 'grain',
  iconColor = 'black',
  markerColor = "darkgreen",
  library = "glyphicon",

)

output$map <- renderLeaflet({
    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE, minZoom = 0, maxZoom = 5)) %>% 
    addTiles(urlTemplate = "map/tiles/{z}_{x}_{y}.jpg") %>% 

    addAwesomeMarkers(data = siteGPS, ~GPS_E, ~GPS_N, icon = icons, popup = ~paste0("<b>", site, "</b>", "<br/>Production system: ", prodSystem, "<br/> <a href='", URL, "'target='_blank'>Access site report</a>")) %>% #color= ~pal(n), radius = n fillOpacity = 0.7, stroke=FALSE, label = country, labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    addCircleMarkers(data = mapfeast[trimws(mapfeast$site_country_name) != "Antarctica",], ~lon, ~lat, radius = ~n^(1/1.5), color= "green", popup = ~paste0("<b>", site_country_name, "</b>", "<br/> Sites: ", n))  #color= ~pal(n), radius = n fillOpacity = 0.7, stroke=FALSE, label = country, labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    

  })


observe({

    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

	wAll <- 850
	hAll <- 900 #length(unique(export_project_site$site_country))*300
	if(input$SI_Tables %in% c("Feed availability", "Crop utilisation") & (length(input$SI_Region) == 0 | length(input$SI_Country) == 0)){
	 #output$visAll <- renderPlotly({ggplotly(All_vis() + xlab(""), height = hAll, width = wAll)  %>% config(displayModeBar = F) }) #tooltip = "text", #layout(showlegend = FALSE, xaxis = list(tickfont = list(size = 8)))
     vis <- ggplotly(All_vis() + xlab(""), height = hAll, width = wAll) %>% layout(title = list(text = paste0('<sup>', All_vis()$labels$subtitle, '</sup>')))
	 #vis[['x']][['layout']][['annotations']][[1]][['x']] <- -0.05 #Hack to shift y axis to where it should be
	 output$visAll <- renderPlotly({vis  %>% config(displayModeBar = F) }) #tooltip = "text", #layout(showlegend = FALSE, xaxis = list(tickfont = list(size = 8)))
	 #output$visAll <- output$visAll %>% layout(margin = list(l = 75))
	 #output$visAll <- renderPlotly({ggplotly(All_vis() + guides(fill = F) + ylab("") + xlab(""), height = hAll, width = wAll) %>% layout(showlegend = FALSE) %>% config(displayModeBar = F) }) #tooltip = "text", #layout(showlegend = FALSE, xaxis = list(tickfont = list(size = 8)))
	 #output$visLegend <- renderCachedPlot({Legend_vis()  }, cacheKeyExpr = {input$SI_Tables}) #+ theme(panel.background = element_rect(fill = 'gray18', colour = 'gray18'))
		

  } else if(input$SI_Tables %in% c("Farm size") & (length(input$SI_Region) == 0 | length(input$SI_Country) == 0)){
  	wAll <- 850
	hAll <- 400 # Facet_dat()*400
	output$visAll <- renderPlotly({ggplotly(All_vis() + xlab(""), height = hAll, width = wAll, tooltip = "x") %>% layout(boxmode = "group", title = list(text = paste0('<sup>', All_vis()$labels$subtitle, '</sup>'))) %>% config(displayModeBar = F) }) #@displaymodebar is the tools at the top of the plot, like zoom and export 
	#output$visAll <- renderPlot({All_vis() }, height = hAll, width = wAll) #
	#output$visAll <- renderCachedPlot({All_vis() }, height = 20000, width = 500, sizePolicy = sizeGrowthRatio(height = 20000, width = 500), cache="app", cacheKeyExpr = {list(input$SI_Tables All_vis(), Facet_dat()) }) #, cacheKeyExpr = {list(input$SI_Tables, All_vis()) })
	
  
  } else if(length(input$SI_Region) == 0 | length(input$SI_Country) == 0){
	#wAll <- ifelse(Facet_dat() > 2, 980, ifelse(Facet_dat() == 2, 850, 700))
	#hAll <- ceiling(Facet_dat()/3)*300
	wAll <- 850
	hAll <- 400
	output$visAll <- renderPlotly({ggplotly(All_vis() + xlab(""), height = hAll, width = wAll) %>% config(displayModeBar = F) }) #@displaymodebar is the tools at the top of the plot, like zoom and export 
	#output$visAll <- renderPlot({All_vis() }, height = hAll, width = wAll) #
	#output$visAll <- renderCachedPlot({All_vis() }, height = 20000, width = 500, sizePolicy = sizeGrowthRatio(height = 20000, width = 500), cache="app", cacheKeyExpr = {list(input$SI_Tables All_vis(), Facet_dat()) }) #, cacheKeyExpr = {list(input$SI_Tables, All_vis()) })
	
	}
})

observe({

    query <- parseQueryString(session$clientData$url_search)
    validate(need(!is.null(query$token) && query$token == "d5M6w1MQIah", "Please provide authentication 'token'"))

	wSel <- ifelse(Selected_facet_dat() == 1, 460, ifelse(Selected_facet_dat() == 2, 850, 850))
	hSel <- ceiling(Selected_facet_dat()/2)*315
	if(input$SI_Tables == "Feed availability") {tt <- c("x", "fill")} else {tt <- c("text", "x", "y", "fill")}
	if(input$SI_Tables %in% c("Farm size") & length(input$SI_Region) > 0){
	
	wSel <- ifelse(length(input$SI_Country) < 4, 460, 460+(27 * length(input$SI_Country)))
	output$visSel1 <- renderPlotly({ggplotly(Selected_vis() + xlab(""), height = 400, width = wSel) %>% layout(boxmode = "group", title = list(text = paste0('<sup>', Selected_vis()$labels$subtitle, '</sup>'))) %>% config(displayModeBar = F) }) #
	#output$visSel1 <- renderPlot({Selected_vis()}) 
	
	} else if(input$SI_Tables %in% c("Feed availability", "Crop utilisation") & length(input$SI_Country) > 0 & length(input$SI_Site) > 0){
	 #output$visSel1 <- renderPlotly({ggplotly(Selected_vis() + xlab(""), height = hSel, width = wSel) %>% config(displayModeBar = F) }) #tooltip = "text", 
	 vis <- ggplotly(Selected_vis() + xlab(""), height = hSel, width = wSel, tooltip = tt)  %>% layout(title = list(text = paste0('<sup>', Selected_vis()$labels$subtitle, '</sup>')))
	 #vis[['x']][['layout']][['annotations']][[1]][['x']] <- -0.05 #Hack to shift y axis to where it should be
	 output$visSel1 <- renderPlotly({vis %>% config(displayModeBar = F) })
	 #output$visSel1 <- renderPlot({Selected_vis()})
	 #output$visSel1 <- renderPlotly({ggplotly(Selected_vis() + guides(fill = F) + ylab("") + xlab(""), height = hSel, width = wSel) %>% layout(showlegend = FALSE) %>% config(displayModeBar = F) }) #tooltip = "text", 
	 #output$visLegend <- renderCachedPlot({Legend_vis()  }, cacheKeyExpr = {input$SI_Tables}) #+ theme(panel.background = element_rect(fill = 'gray18', colour = 'gray18'))
		
	} 
	#else if(length(input$SI_Region) > 0 & input$SI_Tables != "Crop utilisation") { 
	
	 #output$visSel1 <- renderPlotly({ggplotly(Selected_vis() + xlab(""), height = hSel, width = wSel) %>% config(displayModeBar = F) }) 

	#}
})




observe({
  
    if(length(input$SI_Region) == 0){

      output$download_visAll <- downloadHandler(
      filename = function() {
        paste("FEAST", input$SI_Tables, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = All_vis(), device = "png", width = 8)
      }
      )
      }
    
})
  
    output$download_visSel <- downloadHandler(filename = function() {
		
        paste("FEAST", input$SI_Tables, ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = Selected_vis(), device = "png", width = 8)
      }
	  
    )
    
  
  

}
