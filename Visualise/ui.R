shinyjs::useShinyjs() #for download button control https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler


                
header <- dashboardHeader(
		    #title = tags$a(href='https://feastdata.ilri.org/', tags$img(src='www/brand.png'))
		    title = "FEAST data visualisations"#,
		    


) #End dashboard header

sidebar <- dashboardSidebar(

                
                pickerInput(inputId = "SI_Tables", 
                                label = "Select visualisation",
                                choices = tablesInputPlot$tabLab, 
                                selected = "Map of sites", 
                                multiple = FALSE,
                                options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose visualisation of interest")
      		          ),
      		          
		sidebarMenu(
		menuItem("Filter data", tabName = "filter", icon = icon("th"), startExpanded = TRUE,
                             

                    selectInput(inputId = "SI_Region", 
                                label = "Region(s)",
                                choices = sort(unique(dfeast$world_region)), 
                                selected = "",  
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
  				            )
                    ),
  				
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
  				))
	                                
	),#End menu item 1	
	

		conditionalPanel(
              		condition ="input.SI_Tables == 'Feed availability' | input.SI_Tables == 'Crop utilisation'",
              				
              			plotOutput("visLegend"#, width = "100%", height = "60px"#,
                	))
		
		#)#end menu item 2
	)#endsidebarMenu

) #End dashboardsidebar







body <- dashboardBody(
                  tags$style(type="text/css", #css to hide any errors in UI @https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs
      		        ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }"),
                  
                  tags$head(includeCSS('www/style.css')), #@https://stackoverflow.com/questions/41240878/r-shiny-fixed-sidebar-and-main-header-in-a-shiny-dashboard
                  
		        conditionalPanel(
                       	condition ="input.SI_Site == ''",
                       	#splitLayout(cellWidths = c("50%", "50%"),
				fluidRow(
					column(1, offset = -1,
												 
                       	downloadButton(outputId = "download_visAll", label = "")),
					#column(1, 
                       	dropdown(
              		actionButton(inputId = "tweetFigSel", label= "", icon("twitter")),
              		actionButton(inputId = "linFigAll", label= "", icon("linkedin")),
              		actionButton(inputId = "gooFigAll", label= "", icon("google-plus")),
              		actionButton(inputId = "fbFigSel", label= "", icon("facebook")),
              		size = "s",
              		icon = icon("share-alt", class = "opt"), 
              		up = FALSE)
              		)#)
              		
            		),
            		
            		conditionalPanel(
                       	condition ="input.SI_Site != '' & input.SI_Country != ''" ,
                       	fluidRow(
				column(1, offset = -1,
                       	downloadButton(outputId = "download_visSel", label = "")),
				#column(1,
                   	dropdown(
              		actionButton(inputId = "tweetFigAll", label= "", icon("twitter")),
              		actionButton(inputId = "linFigAll", label= "", icon("linkedin")),
              		actionButton(inputId = "gooFigAll", label= "", icon("google-plus")),
              		actionButton(inputId = "fbFigAll", label= "", icon("facebook")),
              		size = "s",
              		icon = icon("share-alt", class = "opt"), 
              		up = FALSE)
              		)#)
            		),
                       	
                  
                  conditionalPanel(
                  
                   	condition = "input.SI_Tables == 'Map of sites'",
       	  
	                    leafletOutput("map", width = "100%", height = "450px")#,
                  ),

                  conditionalPanel(
                       	condition ="input.SI_Tables != 'Map of sites' & input.SI_Region == '' | input.SI_Country == ''",
                       	
                   	    plotlyOutput("visAll")
                      ),
                      
                      conditionalPanel(
                       	condition ="input.SI_Tables != 'Map of sites' & input.SI_Region != '' & input.SI_Country != ''" , #& input.SI_Site != '' &  #& input.SI_Tables != 'Feed availability'
                   	    plotlyOutput("visSel1")
                    	)#,


                
) #End of dashboardbody

ui <- dashboardPage(header, sidebar, body, skin = "green")
