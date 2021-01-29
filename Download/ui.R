tableVec <- c("Project site", "Focus group", "Respondent", "Coop membership", "Core context attribute score", "Crop cultivation", "Decision making by household", "Feed labor division", "Feed source availability", "Focus group monthly statistics",  "Fodder crop cultivation", "Income activity", "Labour activity", "Livestock holding", "Livestock sale", "Purchased feed", "Respondent monthly statistics", "Womens income activity")

header <- dashboardHeader(disable = FALSE#,
		    #title = tags$a(href='https://feastdata.ilri.org/', tags$img(src='www/brand.png'))
		    #title = "FEAST data download"#,
) #End dashboard header

sidebar <- dashboardSidebar(
				  shinyjs::useShinyjs(), #for download button control https://stackoverflow.com/questions/53616176/shiny-use-validate-inside-downloadhandler
                  #tags$style(type="text/css", "* {color: black !important; font-family: Arial, sans-serif !important}"),
                  tags$style(type="text/css", "#dldAllDat_R {color: black}"), #background-color:orange; ;font-family: Courier New
                  tags$style(type="text/css", "#dldSelDat_R {color: black}"),
                  #tags$style(type="text/css", ".dropdown-menu {color: black}"),
                  #tags$style(type="text/css", ".filter-option {color: black}"),
                  #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = 'www/style.css')),
                  
                  
                  fluidRow(
                  column(12, align="center",
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
		    shinyjs::hidden(downloadButton('dldSelDat_R', 'Download selected data', class = "dld"))
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
		    shinyjs::disabled(downloadButton('dldAllDat_R', 'Download all data', class = "dld"))
					)
          )), #End fluid page and column
                #h6("Select filters for your data download"),
#    sidebarMenu(
#		menuItem("Filter data", tabName = "filter", icon = icon("th"), startExpanded = TRUE,
				
                    selectInput(inputId = "SI_Region", 
                                label = "Region(s)", #substr(dfeast$world_region[1],1,2)),
                                choices = unique(dfeast$site_world_region), #c("East Asia and Pacific", "South Asia", "Sub-Saharan Africa"), 
                                selected = "", #unique(dfeast$world_region), 
                                multiple = TRUE),                    
                  
                  conditionalPanel(
                    condition = "input.SI_Region != ''", #@ For more options: https://rdrr.io/cran/shinyWidgets/man/pickerOptions.html
                    pickerInput(inputId = "SI_Country", 
                                label = "Country",
                                choices = "",
  				multiple = TRUE, 
  				options = list(
        				`actions-box` = TRUE, dropupAuto = FALSE,
        				`none-selected-text` = "Choose countries of interest"
      		)
  				)),
  				
  		  conditionalPanel(
                    condition = "input.SI_Country != '' & input.SI_Region != ''",
                    dateRangeInput(inputId = "date", label = "Date range")
                    
                    ),
                    
                  conditionalPanel(
                    condition = "input.SI_Country != '' & input.SI_Region != ''",
                    div(class = "site-down",
                    pickerInput(inputId = "SI_Site", 
                                label = "Site(s)",
                                choices = "",
                        multiple = TRUE, 
                        options = pickerOptions(
                            actionsBox = TRUE, dropupAuto = FALSE, windowPadding = "[100,0,0,0]",
                            `none-selected-text` = "Choose sites of interest", selectOnTab = T
                        )
                    ))),

		            conditionalPanel(
                    condition = "input.SI_Site!= '' & input.SI_Country != '' & input.SI_Region != ''",
                    

                    
                    pickerInput(inputId = "SI_Tables", 
                                label = "Select tables",
                                choices = tableVec, #tablesInputDisp$tabLab, 
                                #selected = tableVec, 
                                multiple = TRUE, 
                                options = pickerOptions(
        				actionsBox = TRUE, dropupAuto = FALSE, size = 10, windowPadding = "[100,0,0,0]", 
        				`none-selected-text` = "Choose tables of interest")
      	              	)
                    )
					
#          ) #End menu item
          
                  
#        ) #End menu item
) #End of sidebar
                  
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
                  ".shiny-output-error:before { visibility: hidden; }"),
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
                            .skin-green .sidebar a, .dropdown-menu>li>a {
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
                            table.dataTable tbody th, table.dataTable tbody td {
                                padding: 8px;
                            }
                            .btn-primary {
                                border-color: #009400;
                                background: linear-gradient(#65a946, #1c9114);
                            }
                            .btn-primary.active, .btn-primary:active, .open>.dropdown-toggle.btn-primary {
                                border-color: #009400;
                                background: linear-gradient(#009400, #1c9114);
                            }
                            .btn-primary.active.focus, .btn-primary.active:focus, .btn-primary.active:hover, .btn-primary:active.focus, .btn-primary:active:focus, .btn-primary:active:hover, .open>.dropdown-toggle.btn-primary.focus, .open>.dropdown-toggle.btn-primary:focus, .open>.dropdown-toggle.btn-primary:hover {
                                border-color: #009400;
                                background: linear-gradient(#009400, #1c9114);
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
                            .site-down .dropdown-menu {
                                width: 193px;
                            }
							  "))),   
				  
                  #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = 'www/style.css')),
                  #tags$head(includeCSS('www/style.css')), #@https://stackoverflow.com/questions/41240878/r-shiny-fixed-sidebar-and-main-header-in-a-shiny-dashboard
              
                  
                  
                   conditionalPanel(
                   	condition = "input.SI_Site == '' | input.SI_Country == ''",
                   	pickerInput(inputId = "SI_TabViewAll", 
                                label = "Select table to view",
                                choices = tableVec, #tablesInputDisp$tabLab, 
                                selected = "Project site", 
                                multiple = FALSE, 
                                options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose table to view")
        			),
        			#	    div(style = 'overflow-x: scroll !important',
                   	DT::dataTableOutput("default_table")
                    	),

                		   conditionalPanel(
		   condition = "input.SI_Tables == '' & input.SI_Site != '' & input.SI_Country != '' & input.SI_Region != ''",
		   	pickerInput(inputId = "SI_TabViewSelCountry", 
                                label = "Select table to view",
                                choices = tableVec, #tablesInputDisp$tabLab,
                                selected = "Project site", 
                                multiple = FALSE, 
                                options = list(
        				`actions-box` = TRUE,
        				`none-selected-text` = "Choose table to view")
        			),
                   	
                    #div(style = 'overflow-x: scroll !important',
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

    		#)
    	
)

ui <- dashboardPage(header, sidebar, body, skin = "green")
