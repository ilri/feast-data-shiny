function(input, output, session) {

#@Raw data prepration is in global file.

dfeast <- dfeast[, c("world_region", "country", "project", "site", "created_at", "exclude")] #Restrict presentation of results to these columns
dfeast$country <- trimws(dfeast$country)
dfeast$site <- trimws(dfeast$site)
dfeast$site <- str_to_sentence(dfeast$site)


####
#Observe input events to update other input options
  observeEvent(input$SI_Region,
               { 
                 updatePickerInput(session, "SI_Country",
                        choices =sort(unique((dfeast %>%
                                               filter(world_region %in% input$SI_Region))$country)))
               })


 
  observe({
    if (length(input$SI_Country) == 0)
    {
        updatePickerInput(session, "SI_Site",
                        choices = "",
        selected = "")
        
        }       
        
        if (length(input$SI_Country) > 0){
        
        c_daterange <- as.Date((dfeast %>%
                                filter(dfeast$country %in% input$SI_Country))$created_at)
      
      updateDateRangeInput(session, "date", start = min(c_daterange)-30,
                           end = max(c_daterange)+1)
                           
      updatePickerInput(session, "SI_Site",
                        choices = sort(unique((dfeast %>% filter(
          country %in% input$SI_Country 
          & created_at > min(c_daterange)-1 & created_at < max(c_daterange)+1
          )
        )$site)),
        selected = sort(unique((dfeast %>% filter(
          country %in% input$SI_Country 
          & created_at > min(c_daterange)-1 & created_at < max(c_daterange)+1
          )
        )$site))
        )
        }
               })   
                             
 
 
   observeEvent(input$date,
               { c_daterange <- as.Date((dfeast %>%
                                filter(dfeast$country %in% input$SI_Country))$created_at)
                              
      updatePickerInput(session, "SI_Site",
                        choices = sort(unique((dfeast %>% filter(
          country %in% input$SI_Country 
          & created_at > as.character(input$date[1]) & created_at < as.character(input$date[2])
          )
        )$site)),
        selected = sort(unique((dfeast %>% filter(
          country %in% input$SI_Country 
          & created_at > as.character(input$date[1]) & created_at < as.character(input$date[2])
          )
        )$site)))        
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

#  observe({ #disable download button if no tables selected
#    if (length(input$SI_Tables) == 0){
#      shinyjs::disable("dldSelDat_R")}
#else {
#      shinyjs::enable("dldSelDat_R")
#     }
#  })  
#
##Hide download button until download data is ready
#if (is.null(dfeast)){
#      shinyjs::disable("dldAllDat_R")}
#else {
#      shinyjs::enable("dldAllDat_R")
#     }


####
#Reactive to prepare filtered visualisations
#Boxplots https://www.r-graph-gallery.com/266-ggplot2-boxplot-with-variable-width
  All_vis <- reactive({
  visOutAll <- NA
        if(input$SI_Tables == "Farm size"){
          farm_size <- farm_size[farm_size$site_country != "Antarctica",]
          farm_size$site_country[farm_size$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          nlab <- paste0(unique(farm_size$site_country[!is.na(farm_size$site_country)]), "\n(n =", table(farm_size$site_country[!is.na(farm_size$site_country)]),")")
          visOutAll <- ggplot(farm_size[!is.na(farm_size$site_country),]) + geom_boxplot(aes(site_country, proportion, colour = farmSize)) + labs(colour = "Farm size") + xlab("Country") + ylab("Percentage of farm households") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Feed availability"){
      #  if(input$SI_Tables == "Feed availability"){
        #Stacked bar plot of category proportions and line of monthly availability
          feed_availability_prop <- group_by(export_feed_source_availability, site_country, month_id, feedCategory) #site_id, site_name, 
          feed_availability_prop <- summarise(feed_availability_prop, percentage = mean(percentage, na.rm=T))
          feed_availability_prop$site_country[feed_availability_prop$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          feed_availability_score <- group_by(export_feed_source_availability, site_country, month_id) #site_country, site_id,
          feed_availability_score <- summarise(feed_availability_score, feedAvailability = mean(feed_availability, na.rm=T), u75 = quantile(feed_availability, probs = 0.75, na.rm = T), l25 = quantile(feed_availability, probs = 0.25, na.rm = T))
          range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm=T)-min(x, na.rm = T))}
          feed_availability_score$feedAvailability <- range01(feed_availability_score$feedAvailability) #Rescale between 0 and 1
          feed_availability_score$u75 <- range01(feed_availability_score$u75)
          feed_availability_score$l25 <- range01(feed_availability_score$l25)
          feed_availability_score <- feed_availability_score[feed_availability_score$site_country != "Antarctica",]
          feed_availability_score$site_country[feed_availability_score$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          
          #@Temp hack for demonstration
          feed_availability_score$u75 <- ifelse(feed_availability_score$u75 < feed_availability_score$feedAvailability, feed_availability_score$feedAvailability, feed_availability_score$u75)
          feed_availability_score$l25 <- ifelse(feed_availability_score$l25 > feed_availability_score$feedAvailability, feed_availability_score$feedAvailability, feed_availability_score$l25)
          
          feed_availability_score$u75 <- ifelse(feed_availability_score$u75 > 1, 1, feed_availability_score$u75)
          feed_availability_score$l25 <- ifelse(feed_availability_score$l25 < 0, 0, feed_availability_score$l25)
          

          feed_availability_prop$Proportion <- feed_availability_prop$percentage
          
          
          visOutAll <- ggplot(feed_availability_prop) + geom_bar(aes(month_id, Proportion, fill = feedCategory), position = "fill", stat = "identity") + geom_ribbon(data= feed_availability_score, aes(x=month_id , ymin = u75, ymax = l25), fill = "grey90", alpha = 0.8) + geom_line(data= feed_availability_score, aes(x=month_id, y=feedAvailability)) + facet_wrap(~ site_country) + scale_x_continuous(breaks = c(1:12)) + ylab("") +  labs(fill = "Feed category") + xlab("Month") + themePublication() + scaleFillPublication() #Other lab options: , subtitle = , caption =

        } else if(input$SI_Tables == "Liveweight"){
          export_livestock_holding$site_country[export_livestock_holding$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          export_livestock_holding <- export_livestock_holding[export_livestock_holding$animal_species_description == "Cattle",]
          export_livestock_holding <- export_livestock_holding[!is.na(export_livestock_holding$site_country) & !is.na(export_livestock_holding$cattleBreeds),]

          visOutAll <- ggplot(export_livestock_holding) + geom_boxplot(aes(cattleBreeds, livestock_holding_average_weight)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Breed") + ylab("Liveweight (kg)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(hjust=1, vjust=1)) #, width = 3/length(unique(export_livestock_holding$cattleBreeds))

        } else if(input$SI_Tables == "Livestock holdings"){
          livestock_holding$site_country[livestock_holding$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          visOutAll <- ggplot(livestock_holding[!is.na(livestock_holding$site_country) & livestock_holding$site_country %in% livestock_holding$site_country[!is.na(livestock_holding$TLU)],]) + geom_boxplot(aes(site_country, TLU)) + xlab("Country") + ylab("Livestock holdings (TLU = 250 kg)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Distance to market"){
          export_focus_group$site_country[export_focus_group$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          visOutAll <- ggplot(export_focus_group[!is.na(export_focus_group$site_country) & export_focus_group$site_country %in% export_focus_group$site_country[!is.na(export_focus_group$focus_group_market_avg_distance_km)],]) + geom_boxplot(aes(site_country, focus_group_market_avg_distance_km)) + xlab("Country") + ylab("Distance to market (km)") + coord_cartesian(ylim = c(0, 20)) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "AI uptake"){
          export_focus_group$site_country[export_focus_group$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          visOutAll <- ggplot(export_focus_group[!is.na(export_focus_group$site_country) & export_focus_group$site_country %in% export_focus_group$site_country[!is.na(export_focus_group$focus_group_percent_reproduction_ai)],]) + geom_boxplot(aes(site_country, focus_group_percent_reproduction_ai)) + xlab("Country") + ylab("Cattle keepers using AI (%)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Fodder yield"){
          export_fodder_crop_cultivation$site_country[export_fodder_crop_cultivation$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          visOutAll <- ggplot(export_fodder_crop_cultivation[!is.na(export_fodder_crop_cultivation$site_country) & !is.na(export_fodder_crop_cultivation$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_type_annual_dry_matter_per_hectare)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Fodder") + ylab("Yield (DM ha)") + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Fodder area cultivated"){
          export_fodder_crop_cultivation$site_country[export_fodder_crop_cultivation$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
          visOutAll <- ggplot(export_fodder_crop_cultivation[!is.na(export_fodder_crop_cultivation$site_country) & !is.na(export_fodder_crop_cultivation$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_cultivation_cultiavted_land_ha)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Fodder") + ylab("Area cultivated (ha)") + coord_cartesian(ylim = c(0, 3)) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Crop utilisation"){

          crop_utilisation$site_country[crop_utilisation$site_country == "Congo The Democratic Republic Of The"] <- "DRC"

          crop_utilisation_country <- group_by(crop_utilisation, site_country, cropName, use)
          crop_utilisation_country <- summarise(crop_utilisation_country, Proportion = mean(percentage, na.rm = T))
          visOutAll <- ggplot(crop_utilisation_country) + geom_bar(aes(cropName, Proportion, fill = use), position = "fill", stat = "identity") + facet_wrap(~site_country, scales = "free_x") + xlab("Crop") + ylab("Proportion used") + labs(fill = "Usage") + themePublication() + theme(axis.text.x = element_text(size = 8, angle=45, hjust=1, vjust=1)) + scale_fill_manual(values = c("red", "seagreen3", "orange", "grey", "gold")) 
          
        }


    return(visOutAll)
})

Selected_vis <- reactive({

    feastDat <- NA
    visOut <- NA
    if(length(input$SI_Site) > 0){
      feastDat <- eval(parse(text = tablesInputPlot$plotDFs[tablesInputPlot$tabLab == input$SI_Tables]))
      feastDat <- feastDat %>% filter(
          site_country %in% input$SI_Country & site_name %in% input$SI_Site 
        )
      feastDat$site_country[feastDat$site_country == "Congo The Democratic Republic Of The"] <- "DRC"

            if(input$SI_Tables == "Farm size" & length(input$SI_Site) > 0){
            nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
          visOut <- ggplot(feastDat[!is.na(feastDat$site_country),]) + geom_boxplot(aes(site_country, proportion, colour = farmSize), width = 1) + labs(colour = "Farm size") + xlab("Country") + ylab("Percentage of farm households") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Feed availability" & length(input$SI_Site) > 0){
            #Stacked bar plot of category proportions and line of monthly availability
            feed_availability_prop <- group_by(feastDat, site_country, site_id, site_name, month_id, feedCategory) #focus_group_id, focus_group_community,
            feed_availability_prop <- summarise(feed_availability_prop, percentage = mean(percentage, na.rm=T))
            feed_availability_score <- group_by(feastDat, site_country, site_id, site_name, month_id) #focus_group_id, focus_group_community,
            feed_availability_score <- summarise(feed_availability_score, feedAvailability = mean(feed_availability, na.rm=T), )
            range01 <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm=T)-min(x, na.rm = T))}
            feed_availability_score$feedAvailability <- range01(feed_availability_score$feedAvailability) #Rescale between 0 and 1
            
            feed_availability_prop <- arrange(feed_availability_prop, site_country, site_name)
            feed_availability_score <- arrange(feed_availability_score, site_country, site_name)
            
            feastDat$site_name <- str_to_sentence(feastDat$site_name)

            feed_availability_prop$Proportion <- feed_availability_prop$percentage
            feed_availability_score$feedAvailability <- round(feed_availability_score$feedAvailability, 1)
            
            visOut <- ggplot(feed_availability_prop) + geom_bar(aes(month_id, Proportion, fill = feedCategory), position = "fill", stat = "identity") + geom_line(data= feed_availability_score, aes(x=month_id, y=feedAvailability)) + facet_wrap(~site_country + site_name, ncol =1) + scale_x_continuous(breaks = c(1:12)) + xlab("Month") + ylab("") + labs(fill = "Feed category") + themePublication() + scale_fill_manual(values = c("Concentrates" = "blue", "Crop residues" = "orange",  "Grazing" = "seagreen3", "Green forage" = "red", "Other" = "gold")) #Other lab options: , subtitle = , caption =
           
        } else if(input$SI_Tables == "Liveweight" & length(input$SI_Site) > 0){
            nlab <- paste0(unique(feastDat$cattleBreeds[!is.na(feastDat$cattleBreeds) & !is.na(feastDat$site_country)]), "\n(n =", table(feastDat$cattleBreeds[!is.na(feastDat$cattleBreeds) & !is.na(feastDat$site_country)]), ")")
            feastDat <- feastDat[feastDat$animal_species_description == "Cattle",]
            visOut <- ggplot(feastDat[!is.na(feastDat$site_country)  & !is.na(feastDat$cattleBreeds) & feastDat$animal_species_description == "Cattle",]) + geom_boxplot(aes(cattleBreeds, livestock_holding_average_weight)) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Breed") + ylab("Liveweight (kg)") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(hjust=1, vjust=1)) #, width = (length(unique(feastDat$cattleBreeds)) / length(unique(export_livestock_holding$cattleBreeds)))

        } else if(input$SI_Tables == "Livestock holdings" & length(input$SI_Site) > 0){
            nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
            #Keep consistent width with this: width = 0.2 + (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name))) 
            visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & feastDat$site_country %in% feastDat$site_country[!is.na(feastDat$TLU)],]) + geom_boxplot(aes(site_country, TLU), width = 0.2 + (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Country") + ylab("Livestock holdings (TLU = 250 kg)") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Distance to market" & length(input$SI_Site) > 0){
            nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
            visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & feastDat$site_country %in% feastDat$site_country[!is.na(feastDat$focus_group_market_avg_distance_km)],]) + geom_boxplot(aes(site_country, focus_group_market_avg_distance_km), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Country") + ylab("Distance to market (km)") + scale_x_discrete(labels=nlab) + coord_cartesian(ylim = c(0, 20)) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "AI uptake" & length(input$SI_Site) > 0){
            nlab <- paste0(unique(feastDat$site_country[!is.na(feastDat$site_country)]), "\n(n =", table(feastDat$site_country[!is.na(feastDat$site_country)]), ")")
            visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & feastDat$site_country %in% feastDat$site_country[!is.na(feastDat$focus_group_percent_reproduction_ai)],]) + geom_boxplot(aes(site_country, focus_group_percent_reproduction_ai), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Country") + ylab("Cattle keepers using AI (%)") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Fodder yield" & length(input$SI_Site) > 0){
            nlab <- paste0(unique(feastDat$fodderName[!is.na(feastDat$fodderName)]), "\n(n =", table(feastDat$fodderName[!is.na(feastDat$fodderName)]), ")")
            visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & !is.na(feastDat$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_type_annual_dry_matter_per_hectare), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + xlab("Fodder") + ylab("Yield (DM ha)") + facet_wrap(~site_country, ncol = 1, scales = "free_x") + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Fodder area cultivated" & length(input$SI_Site) > 0){
            nlab <- paste0(unique(feastDat$fodderName[!is.na(feastDat$fodderName)]), "\n(n =", table(feastDat$fodderName[!is.na(feastDat$fodderName)]), ")")
            visOut <- ggplot(feastDat[!is.na(feastDat$site_country) & !is.na(feastDat$fodder_crop_type_name),]) + geom_boxplot(aes(fodderName, fodder_crop_cultivation_cultiavted_land_ha), width = (length(unique(feastDat$site_country)) / length(unique(export_project_site$site_country_name)))) + facet_wrap(~site_country, ncol = 1, scales = "free_x") + xlab("Fodder") + ylab("Area cultivated (ha))") + coord_cartesian(ylim = c(0, 3)) + scale_x_discrete(labels=nlab) + themePublication() + scaleColourPublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))

        } else if(input$SI_Tables == "Crop utilisation" & length(input$SI_Site) > 0){
          feastDat$site_name <- str_to_sentence(feastDat$site_name)

	  feastDat <- group_by(feastDat, site_country, site_name, cropName, use)
          feastDat <- summarise(feastDat, percentage = mean(percentage, na.rm=T))
          feastDat$site_country[feastDat$site_country == "Congo The Democratic Republic Of The"] <- "DRC"
                        
          
            feastDat$Proportion <- feastDat$percentage
            visOut <- ggplot(feastDat) + geom_bar(aes(cropName, Proportion, fill = use), position = "fill", stat = "identity") + facet_wrap(~site_country + site_name, ncol = 1, scales = "free_x") + xlab("Crop") + ylab("Proportion used") + labs(fill = "Usage") + themePublication() + theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1)) + scale_fill_manual(values = c("red", "seagreen3", "orange", "grey", "gold")) #  #Other lab options: , subtitle = , caption =
          
          
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
    if(length(input$SI_Site) >0 & input$SI_Tables != "Map of sites" & !is.na(tablesInputPlot$facetVar[tablesInputPlot$tabLab == input$SI_Tables])){
      feastDat <- eval(parse(text = tablesInputPlot$plotDFs[tablesInputPlot$tabLab == input$SI_Tables]))
      feastDat <- feastDat %>% filter(
          site_country %in% input$SI_Country & site_name %in% input$SI_Site 
      )
          nFacets <- length(unique(feastDat[, tablesInputPlot$facetVar[tablesInputPlot$tabLab == input$SI_Tables]]))

        } else {
        nFacets <- 1
        } 
    return(nFacets)
})

Legend_vis <- reactive({
  
  legend <- NA
  if(input$SI_Tables == "Feed availability"){  
    feed_availability_legend <- group_by(export_feed_source_availability, site_country, site_id, focus_group_id, focus_group_community, site_name, month_id, feedCategory)
    feed_availability_legend <- summarise(feed_availability_legend, percentage = mean(percentage, na.rm=T), )
    feed_availability_legend <- feed_availability_legend[!duplicated(feed_availability_legend[,"feedCategory"]),] #lust take one ob per level for quick plotting
    
    p <- ggplot(feed_availability_legend) + geom_bar(aes(month_id, percentage, fill = feedCategory), position = "fill", stat = "identity") + ylab("Feed availability (Scaled between 0 and 1) \n and proportion of feed category") + labs(fill = "Feed category") + themePublication() + scale_fill_manual(values = c("Concentrates" = "blue", "Crop residues" = "orange",  "Grazing" = "seagreen3", "Green forage" = "red", "Other" = "gold"))
    tmpLeg <- get_legend(p)
    tmpLeg <- add_sub(tmpLeg, "Feed availability (line with IQR)", angle = 90, x = 0.85, y = 0.6) # \n and percentage of feed category (bars)
    legend <- ggdraw(tmpLeg) 

  } else if(input$SI_Tables == "Crop utilisation"){
    crop_utilisation_legend <- crop_utilisation[!duplicated(crop_utilisation[, "use"]),]
    
    p <- ggplot(crop_utilisation_legend) + geom_bar(aes(cropName, percentage, fill = use), position = "fill", stat = "identity") + labs(fill = "Usage") + themePublication() + scale_fill_manual(values = c("Burned" = "red", "Fed" = "seagreen3", "Mulched" = "orange", "Other" = "grey", "Sold" = "gold")) 
    tmpLeg <-  get_legend(p)
    tmpLeg <- add_sub(tmpLeg, "Proportion used", angle = 90, x = 0.9, y = 0.6,  vpadding = grid::unit(1, "lines"), vjust=0) #vjust = 0.5
    legend <- ggdraw(tmpLeg)

  }
  return(legend)
})


icons <- awesomeIcons(
  icon = 'grain',
  iconColor = 'black',
  markerColor = "darkgreen",
  library = "glyphicon",

)

output$map <- renderLeaflet({

    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE, minZoom = 0, maxZoom = 5)) %>% 
    addTiles(urlTemplate = "map/tiles/{z}_{x}_{y}.jpg") %>% 

    addAwesomeMarkers(data = siteGPS, ~GPS_E, ~GPS_N, icon = icons, popup = ~paste0("<b>", site, "</b>", "<br/>Production system: ", prodSystem, "<br/> <a href='", URL, "'target='_blank'>Access site report</a>")) %>% #color= ~pal(n), radius = n fillOpacity = 0.7, stroke=FALSE, label = country, labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    addCircleMarkers(data = mapfeast, ~lon, ~lat, radius = ~n^(1/1.5), color= "green", popup = ~paste0("<b>", country, "</b>", "<br/> Sites: ", n))  #color= ~pal(n), radius = n fillOpacity = 0.7, stroke=FALSE, label = country, labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    

  })


observe({

	wAll <- 850
	hAll <- 900 #length(unique(export_project_site$site_country))*300
	if(input$SI_Tables %in% c("Feed availability", "Crop utilisation") & length(input$SI_Region) == 0){
	 output$visAll <- renderPlotly({ggplotly(All_vis() + guides(fill = F) + ylab("") + xlab(""), height = hAll, width = wAll) %>% layout(showlegend = FALSE) %>% config(displayModeBar = F) }) #tooltip = "text", #layout(showlegend = FALSE, xaxis = list(tickfont = list(size = 8)))
	 output$visLegend <- renderCachedPlot({Legend_vis()  }, cacheKeyExpr = {input$SI_Tables}) #+ theme(panel.background = element_rect(fill = 'gray18', colour = 'gray18'))
		

  } else if(input$SI_Tables %in% c("Farm size") & length(input$SI_Region) == 0){
  	wAll <- 850
	hAll <- Facet_dat()*400
	output$visAll <- renderPlotly({ggplotly(All_vis() + xlab(""), height = hAll, width = wAll) %>% layout(boxmode = "group") %>% config(displayModeBar = F) }) #@displaymodebar is the tools at the top of the plot, like zoom and export 
	#output$visAll <- renderPlot({All_vis() }, height = hAll, width = wAll) #
	#output$visAll <- renderCachedPlot({All_vis() }, height = 20000, width = 500, sizePolicy = sizeGrowthRatio(height = 20000, width = 500), cache="app", cacheKeyExpr = {list(input$SI_Tables All_vis(), Facet_dat()) }) #, cacheKeyExpr = {list(input$SI_Tables, All_vis()) })
	
  
  }else if(length(input$SI_Region) == 0){
	#wAll <- ifelse(Facet_dat() > 2, 980, ifelse(Facet_dat() == 2, 850, 700))
	#hAll <- ceiling(Facet_dat()/3)*300
	wAll <- 850
	hAll <- Facet_dat()*400
	output$visAll <- renderPlotly({ggplotly(All_vis() + xlab(""), height = hAll, width = wAll) %>% config(displayModeBar = F) }) #@displaymodebar is the tools at the top of the plot, like zoom and export 
	#output$visAll <- renderPlot({All_vis() }, height = hAll, width = wAll) #
	#output$visAll <- renderCachedPlot({All_vis() }, height = 20000, width = 500, sizePolicy = sizeGrowthRatio(height = 20000, width = 500), cache="app", cacheKeyExpr = {list(input$SI_Tables All_vis(), Facet_dat()) }) #, cacheKeyExpr = {list(input$SI_Tables, All_vis()) })
	
	}
})

observe({

	wSel <- 850
	hSel <- Selected_facet_dat()*400
	if(input$SI_Tables %in% c("Feed availability", "Crop utilisation") & length(input$SI_Region) > 0){
	 output$visSel1 <- renderPlotly({ggplotly(Selected_vis() + guides(fill = F) + ylab("") + xlab(""), height = hSel, width = wSel) %>% layout(showlegend = FALSE) %>% config(displayModeBar = F) }) #tooltip = "text", 
	 output$visLegend <- renderCachedPlot({Legend_vis()  }, cacheKeyExpr = {input$SI_Tables}) #+ theme(panel.background = element_rect(fill = 'gray18', colour = 'gray18'))
		
	} else if(input$SI_Tables %in% c("Farm size") & length(input$SI_Region) > 0){
	
	output$visSel1 <- renderPlotly({ggplotly(Selected_vis() + xlab(""), height = hSel, width = wSel) %>% layout(boxmode = "group") %>% config(displayModeBar = F) }) 
	
	} else if(length(input$SI_Region) > 0 & input$SI_Tables != "Crop utilisation") { 
	
	 output$visSel1 <- renderPlotly({ggplotly(Selected_vis() + xlab(""), height = hSel, width = wSel) %>% config(displayModeBar = F) }) 

	}
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
