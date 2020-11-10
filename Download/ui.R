ui <- fluidPage(#theme = shinytheme("slate"),
                shinyjs::useShinyjs(), #for download button control https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler
                # Application title
                titlePanel("FEAST data download"), #,style = "color:mediumturquoise"
                
                sidebarPanel(width = 3, #style = "position:fixed;",  #fixed when scrolling main
                tags$style(".well {background-color:white;}"), #White background   
                h6("Select filters for your data download"),
                    selectInput(inputId = "SI_Region", 
                                label = "Region(s)", #substr(dfeast$world_region[1],1,2)),
                                choices = sort(unique(export_project_site$site_world_region_name)), 
                                selected = "", #unique(dfeast$world_region), 
                                multiple = TRUE),                    
                  
                  conditionalPanel(
                    condition = "input.SI_Region != ''", #@ For more options: https://rdrr.io/cran/shinyWidgets/man/pickerOptions.html
                    pickerInput(inputId = "SI_Country", 
                                label = "Country",
                                choices = "",
  				multiple = TRUE, 
  				options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose countries of interest"
      		)
  				)),
  				
  		  conditionalPanel(
                    condition = "input.SI_Country != '' & input.SI_Region != ''",
                    dateRangeInput(inputId = "date", label = strong("Date range"), min = min(dfeast$created_at)-30, max = max(dfeast$created_at)+1)
                    
                    ),
                    
                  conditionalPanel(
                    condition = "input.SI_Country != '' & input.SI_Region != ''",
                    pickerInput(inputId = "SI_Site", 
                                label = "Site(s)",
                                choices = "",
  				multiple = TRUE, 
  				options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose sites of interest"
      		)
  				)),
                    
                  conditionalPanel(
                    condition = "input.SI_Site!= '' & input.SI_Country != '' & input.SI_Region != ''",
                    pickerInput(inputId = "SI_Tables", 
                                label = "Select tables",
                                choices = tablesInputDisp$tabLab, 
                                #selected = tablesUser$tabLab, 
                                multiple = TRUE, 
                                options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose tables of interest")
      		)
                  ),
                  
                  
                  conditionalPanel(
                    condition = "input.SI_Site != '' & input.SI_Country != '' & input.SI_Region != ''",
                    radioGroupButtons(
                    inputId = "DATA_FormatSel", label = "Choose export data format:", 
  		    choices = c("CSV", "XLSX", "RDATA"), 
  		    selected = "RDATA", #select default RDATA as there's the least processing involved
  		    #multiple = F,
 		    justified = TRUE, status = "primary", size = 'xs', #direction = 'vertical', 
  		    checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
),
		    shinyjs::disabled(downloadButton('dldSelDat_R', 'Download selected data'))
		    #actionButton(inputId = "submit", label= "Download selected data")
                    ),
                    
                  conditionalPanel(
                    condition = "input.SI_Site == '' | input.SI_Country == ''",
                    radioGroupButtons(
 		    inputId = "DATA_FormatAll", label = "Choose export data format:", 
  		    choices = c("CSV", "XLSX", "RDATA"), 
  		    selected = "RDATA", #select default RDATA as there's the least processing involved
  		    #multiple = F,
 		    justified = TRUE, status = "primary", size = 'xs', #direction = 'vertical', 
  		    checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
),
		    #Hide button until data prepared
		    shinyjs::disabled(downloadButton('dldAllDat_R', 'Download all publicly available data'))

                    )
                  
                  ), #End of sidebarpanel
                  
                  mainPanel(
                  tags$style(type="text/css", #css to hide any errors in UI @https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs
      		        ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }"),
                  
                  
                  
                   conditionalPanel(
                   	condition = "input.SI_Site == '' | input.SI_Country == ''",
                   	pickerInput(inputId = "SI_TabViewAll", 
                                label = "Select table to view",
                                choices = tablesInputDisp$tabLab, 
                                selected = "Project site", 
                                multiple = FALSE, 
                                options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose table to view")
        			),
        				
                   	DT::dataTableOutput("default_table")
                    	),

                		   conditionalPanel(
		   condition = "input.SI_Tables == '' & input.SI_Site != '' & input.SI_Country != '' & input.SI_Region != ''",
		   	pickerInput(inputId = "SI_TabViewSelCountry", 
                                label = "Select table to view",
                                choices = tablesInputDisp$tabLab,
                                selected = "Project site", 
                                multiple = FALSE, 
                                options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose table to view")
        			),
                   	
                   	DT::dataTableOutput("selected_tableAllTab")
                    	),
                    	
		   conditionalPanel(
		   condition = "input.SI_Tables != '' & input.SI_Site != '' & input.SI_Country != '' & input.SI_Region != ''",
		   	pickerInput(inputId = "SI_TabViewSel", 
                                label = "Select table to view",
                                choices = "", 
                                selected = "Project_site", 
                                multiple = FALSE, 
                                options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose table to view")
        			),
                   	
                   	DT::dataTableOutput("selected_table")
                    	)

    		)
    	
                
)
