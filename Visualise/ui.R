shinyjs::useShinyjs() #for download button control https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler


                
header <- dashboardHeader(disable = FALSE#,
		    ##title = tags$a(href='https://feastdata.ilri.org/', tags$img(src='www/brand.png'))
		    #title = "FEAST data visualisations"#,
		    


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
                                choices = sort(unique(dfeast$site_world_region[dfeast$site_world_region != "Antarctica"])), 
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
                    condition = "input.SI_Country != '' & input.SI_Region != '' & input.SI_Tables != 'Farm size' & input.SI_Tables != 'Map of sites'",
                    dateRangeInput(inputId = "date", label = strong("Date range"), min = min(dfeast$created_at)-30, max = max(dfeast$created_at)+1)
                    
                    ),
                    
                  conditionalPanel(
                    condition = "input.SI_Country != '' & input.SI_Region != '' & input.SI_Tables != 'Farm size' & input.SI_Tables != 'Map of sites'",
					#h5("Select up to 2 sites"),
                    pickerInput(inputId = "SI_Site", 
                                label = "Select site",
                                choices = "",
  				multiple = TRUE, 
  				options = pickerOptions(
        				actionsBox = TRUE,
        				noneSelectedText = "Choose sites of interest", maxOptions = 6, maxOptionsText = "Maximum 6", selectOnTab = T)
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

	disconnectMessage(
    text = "Please try again",
    refresh = "Refresh",
    background = "#bdbaba",
	size = 15,
    colour = "#666",
    overlayColour = "grey",
    overlayOpacity = 0.3,
    refreshColour = "#1c9114"
  ),
                  tags$style(type="text/css", #css to hide any errors in UI @https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs
      		        ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }", 
				  ".recalculating { opacity: 1.0; }"), #So not grey when database is reloading https://stackoverflow.com/questions/28094844/invalidate-later-in-shiny),
                  tags$head(tags$style("#pred{color: black;
                                /* font-size: 20px;/*
                                 font-family: 'Oxygen', Helvetica, Arial, sans-serif;
                                 }")),
				  tags$head(tags$style(HTML("* {font-family: 'Oxygen', Helvetica, Arial, sans-serif};
                                 "))),
                  tags$head(tags$style(HTML("
					/* navbar (rest of the header) */
                            body {
                              font-family: 'Oxygen', Helvetica, Arial, sans-serif;font-size: 15px;color: #666;
                            }
                            .skin-green .sidebar a {
                              color : #666;
                            }
                            .dropdown-menu>.active>a, .dropdown-menu>.active>a:focus, .dropdown-menu>.active>a:hover {
                                background: #ddd;
                                color : #666;
                            }
                            .dropdown-menu>li>a:focus, .dropdown-menu>li>a:hover {
                                color: #fff;
                                background-color: #5897fb;
                            }
                            .skin-green .sidebar-menu>li.active>a, .skin-green .sidebar-menu>li:hover>a {
                                border-left-color: #3c9b2a;
                            }
							.skin-green .main-header .navbar {
                              background: linear-gradient(#65a946, #1c9114);
                            }
                            .skin-green .main-header .logo, .skin-green .main-header .navbar .sidebar-toggle:hover {
                              background: linear-gradient(#009400, #1c9114);
                            }
							.progress-bar{background-color:#1c9114;
							}
							.shiny-notification {
							  height: 90px;
							  width: 210px;
							  position:fixed;
							  top: calc(15% - 50px);;
							  left: calc(100% - 400px);;
							}
                            .dropdown-menu>li>a {
                                white-space: break-spaces;
                            }
                            label {
                                font-weight: normal;
                            }
                            .shiny-notification {
                                left: 50%;
                                position: absolute;
                                top: 110px;
                            }
                            #shiny-notification-panel {
                                position: static;
                                bottom: auto;
                                right: auto;
                            }
							  "))),   
                  #tags$head(includeCSS('www/style.css')), #@https://stackoverflow.com/questions/41240878/r-shiny-fixed-sidebar-and-main-header-in-a-shiny-dashboard
                  
				  
		        conditionalPanel(
                       	condition ="input.SI_Tables != 'Map of sites' & input.SI_Tables != 'Feed availability' & input.SI_Tables != 'Crop utilisation' & input.SI_Region == ''",
                       	#splitLayout(cellWidths = c("50%", "50%"),
				fluidRow(
					column(1, offset = -1,
												 
                       	downloadButton(outputId = "download_visAll", label = "")),
					##column(1, 
                   #    	dropdown(
              	#	actionButton(inputId = "tweetFigSel", label= "", icon("twitter")), https://stackoverflow.com/questions/62052804/is-there-a-way-to-add-share-buttons-to-make-plots-shareable-in-shiny/62152186#62152186
              	#	actionButton(inputId = "linFigAll", label= "", icon("linkedin")),
              	#	actionButton(inputId = "gooFigAll", label= "", icon("google-plus")),
              	#	actionButton(inputId = "fbFigSel", label= "", icon("facebook")),
              	#	size = "s",
              	#	icon = icon("share-alt", class = "opt"), 
              	#	up = FALSE)
              		)##)
              		
            		),
            		
            		conditionalPanel(
                       	condition ="input.SI_Tables != 'Map of sites' & input.SI_Site != '' & input.SI_Country != '' & input.SI_Region != ''" ,
                       	fluidRow(
				column(1, offset = -1,
                       	downloadButton(outputId = "download_visSel", label = ""))#,
				##column(1,
                   #	dropdown(
              	#	actionButton(inputId = "tweetFigAll", label= "", icon("twitter")),
              	#	actionButton(inputId = "linFigAll", label= "", icon("linkedin")),
              	#	actionButton(inputId = "gooFigAll", label= "", icon("google-plus")),
              	#	actionButton(inputId = "fbFigAll", label= "", icon("facebook")),
              	#	size = "s",
              	#	icon = icon("share-alt", class = "opt"), 
              	#	up = FALSE)
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

ui <- dashboardPage(header, sidebar, body, skin = "green", 
	tags$head(tags$style("#pred{color: black;
                                /* font-size: 20px;/*
                                 font-family: 'Oxygen', Helvetica, Arial, sans-serif;
                                 }"))
								 )
