server <- function(input, output, session) {

#@Raw data prepration is in global file.

dfeast <- dfeast[, c("world_region", "country", "project", "site", "created_at", "exclude")] #Restrict presentation of results to these columns
dfeast$country <- trimws(dfeast$country)
dfeast$site <- trimws(dfeast$site)
dfeast$site <- str_to_sentence(dfeast$site) #@remove excluded data from these selection options
#@Excluded data already removed in Global

####
#Observe input events to update other input options
  observeEvent(input$SI_Region,
               { 
                 updatePickerInput(session, "SI_Country",
                        choices =sort(unique((dfeast %>%
                                               filter(world_region %in% input$SI_Region))$country)))
               })
               

  observeEvent(input$SI_Country,
               { c_daterange <- as.Date((dfeast %>%
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
        )$site)))        
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
      shinyjs::disable("dldSelDat_R")
      
            updatePickerInput(session, "SI_TabViewSel",
                        choices = "", selected = "Project site")
        }
else {
      shinyjs::enable("dldSelDat_R")
      
      updatePickerInput(session, "SI_TabViewSel",
                        choices = input$SI_Tables, selected = input$SI_Tables[1]) #Selected is the first choice of a SI_Tables session
        }

  })  

#Hide download button until download data is ready
if (is.null(dfeast)){
      shinyjs::disable("dldAllDat_R")}
else {
      shinyjs::enable("dldAllDat_R")
     }


####
#Reactive to prepare all table data
All_data <- reactive({
    req(input$SI_TabViewAll)
  
    dataOut <- NA

    dataOut <- eval(parse(text = tablesInputDisp$tabFull[tablesInputDisp$tabLab == input$SI_TabViewAll]))
      #dataOut$site_country[dataOut$site_country == "Congo The Democratic Republic Of The"] <- "DRC"

 
    
    return(dataOut)
  })

#Reactive to prepare selected table data when no tables selected
Selected_dataAllTab <- reactive({
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
    
    return(dataOutSel)
  })
  
#Reactive to prepare selected table data 
Selected_data <- reactive({
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
    
    return(dataOutSel)
  })




####
#Main output tables

output$default_table <- DT::renderDataTable(
            All_data(), rownames = FALSE, selection = 'none',
            options = list(dom = 't',
            paging = FALSE) #Ref@https://datatabl
 )

output$selected_tableAllTab <- DT::renderDataTable(
            Selected_dataAllTab(), rownames = FALSE, selection = 'none',
            options = list(dom = 't',
            paging = FALSE) #Ref@https://datatabl
 )

output$selected_table <- DT::renderDataTable(
            Selected_data(), rownames = FALSE, selection = 'none',
            options = list(dom = 't',
            paging = FALSE) #Ref@https://datatabl
 )





 
############Prepare data for download within download handler

output$dldAllDat_R <- downloadHandler(
  

   filename = function() {
   	if(input$DATA_FormatAll == "CSV"){
   	paste("FeastData", Sys.time(), ".zip", sep = "")
          }
        else{
        paste("FeastData", Sys.time(), ".", input$DATA_FormatAll, sep = "")}
   },
   
   content = function(con) {
     if(input$DATA_FormatAll == "CSV") {
      withProgress(message = 'Your download is being prepared',
      detail = 'Please wait', value = 0, {
		    for(i in 1:length(tablesExport)) {
		      write.csv(eval(parse(text = tablesExport[i])), paste0(cacheDIR, "/", substr(tablesExport[i], 8, nchar(tablesExport[i])),start_time, "FEAST.csv"))
          incProgress(amount = 1/length(tablesExport), message = "Generating your CSVs", detail = paste(i, "of", length(tablesExport))) #Progress indicator increment
		    }
		    csvFiles <- list.files(cacheDIR, full.names = T)[grep(paste0(start_time, "FEAST.csv"), list.files(cacheDIR))]
		    zip(paste0(cacheDIR, "/complete", start_time, "FEAST.zip"), csvFiles) #, flags = "-j"

        file.copy(paste0(cacheDIR, "/complete", start_time, "FEAST.zip"), con)
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
        incProgress(amount = 0.01, message = "Saving workbook", detail = "Grab a cuppa") #Progress indicator increment
			  saveWorkbook(wb, paste0(cacheDIR, "/complete", start_time, "FEAST.xlsx"), overwrite = T)
        incProgress(amount = 0.19, message = "Saving workbook", detail = "Grab a cuppa") #Progress indicator increment

       file.copy(paste0(cacheDIR, "/complete", start_time, "FEAST.xlsx"), con)

       
      })#end of progress indicator
     }
     
     if(input$DATA_FormatAll == "RDATA") {
       #file.copy(paste0(cacheDIR, "/complete", start_time, "FEAST.RDATA"), con)
       save(list = ls(env.export)[!grepl("Sub", ls(env.export))], file = con, envir = env.export) #All files already in env. Imported in global.R #ls(env.export)
     }


   }


   #}
 )

 

output$dldSelDat_R <- downloadHandler(
	
   filename = function() {
   	if(input$DATA_FormatSel == "CSV"){
   	paste("FeastDataSubset", Sys.time(), ".zip", sep = "")
          }
        else{
        paste("FeastDataSubset", Sys.time(), ".", input$DATA_FormatSel, sep = "")}
   },
   
   content = function(con) {
     if(input$DATA_FormatSel == "CSV") {
       withProgress(message = 'Your download is being prepared',
       detail = 'Please wait', value = 0, {
       for(i in 1:length(tablesExport)) {
		    if(tablesExport[i] %in% tablesInputDisp$tabFull[tablesInputDisp$tabLab %in% input$SI_Tables]){ #Only incorporate tables selected by the user. Default is all selected
		      tmpExportTab <- eval(parse(text = tablesExport[i])) #eval(parse()) to return the object of the same name as the string
		    if("site_country" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   	      tmpExportTab <- tmpExportTab %>% filter(site_country %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. 
	  		 }
	  	
	  	  if("site_country_name" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   	    tmpExportTab <- tmpExportTab %>% filter(site_country_name %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. 
	  		}

		write.csv(eval(parse(text = tablesExport[i])), paste0(cacheDIR, "/", substr(tablesExport[i], 8, nchar(tablesExport[i])),start_time, "FEASTsub.csv"))

		}
    incProgress(amount = 1/length(input$SI_Tables), message = "Generating your CSVs") #Progress indicator increment
		}
		csvFiles <- list.files(cacheDIR, full.names = T)[grep(paste0(start_time, "FEASTsub.csv"), list.files(cacheDIR))]
		zip(paste0(cacheDIR, "/selected", start_time, "FEAST.zip"), csvFiles) #, flags = "-j"
       file.copy(paste0(cacheDIR, "/selected", start_time, "FEAST.zip"), con)
      })#end of progress indicator
     }
     
     if(input$DATA_FormatSel == "XLSX") {
      withProgress(message = 'Your download is being prepared',
      detail = 'Please wait', value = 0, {
      wb = createWorkbook()
	    for(i in 1:length(tablesExport)) {#! Fix disjointed tables Export and tabLab. Bring into one DF and use consistently throughout
			  if(tablesExport[i] %in% tablesInputDisp$tabFull[tablesInputDisp$tabLab %in% input$SI_Tables]){ #Only incorporate tables selected by the user. Default is all selected
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
			            
      incProgress(amount = 0.01, message = "Saving workbook", detail = "Grab a cuppa") #Progress indicator increment
			saveWorkbook(wb, paste0(cacheDIR, "/selected", start_time, "FEAST.xlsx"), overwrite = T)
      incProgress(amount = 0.19, message = "Saving workbook", detail = "Grab a cuppa") #Progress indicator increment
      file.copy(paste0(cacheDIR, "/selected", start_time, "FEAST.xlsx"), con)
      })#end of progress indicator 
     }
     
     if(input$DATA_FormatSel == "RDATA") {
      withProgress(message = 'Your download is being prepared',
      detail = 'Please wait', value = 0, {
      dflis <- list()
	#Prepare data 	
	  for(i in 1:length(tablesExport)){ 
	    if(tablesExport[i] %in% tablesInputDisp$tabFull[tablesInputDisp$tabLab %in% input$SI_Tables]){ #Only incorporate tables selected by the user. Default is all selected
	      tmpExportTab <- eval(parse(text = tablesExport[i]))
        if("site_country" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   		  tmpExportTab <- tmpExportTab %>% filter(site_country %in% input$SI_Country & site_name %in% input$SI_Site) #SI_Site already filtered by date. # 
	  		  }

	  		if("site_country_name" %in% colnames(eval(parse(text = tablesExport[i])))){ #site_name and site_country is in all. Just in case
	   		  tmpExportTab <- tmpExportTab %>% filter(site_country_name %in% input$SI_Country & site_name %in% input$SI_Site)  #SI_Site already filtered by date. # 
	  		  }
	    assign(paste0(substr(tablesExport[i], 8, nchar(tablesExport[i])), "Sub"), tmpExportTab, envir = env.export) 		 
		  }
		incProgress(amount = 1/length(input$SI$Tables), message = "Adding dataframes to RDATA file") #Progress indicator increment
		}
    #save(list = ls(env.export)[grepl("Sub", ls(env.export))], file = paste0(cacheDIR, "/selected", start_time, "FEAST.RDATA"), envir = env.export)
	 save(list = ls(env.export)[grepl("Sub", ls(env.export))], file = con, envir = env.export)
      #file.copy(paste0(cacheDIR, "/selected", start_time, "FEAST.RDATA"), con)
       })#end of progress indicator
      }
  }
  )

 
}
