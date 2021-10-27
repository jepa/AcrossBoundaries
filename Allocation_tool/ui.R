#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","plotly","formattable","shinydashboard"))

spp_survey <- read.csv("./data/spp_region.csv") %>% 
  filter(
    # spp %in% c ("Centropristis striata","Gadus morhua"),
    region %in% c("Northeast US Fall" , "Northeast US Spring")) #No more seasons



dashboardPage(
  dashboardHeader(title = "Across Boundaries Project"),
  # Sidebar  ####
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Information", tabName = "info", icon = icon("info")),
      menuItem("Tool", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Instructions", tabName = "instructions", icon = icon("gear"))
    )
  ),
  # Body ####
  dashboardBody(
    tabItems(
      ## Information Body ####
      tabItem(tabName = "home",
              column(12,
                     align = "center",
                     h1("Welcome to the quota allocation tool!"),
              ),
              column(
                width = 8,
                offset = 2,
                align = "justified",
                h4("This is a dynamic tool. It was build with the objective of exploring different
                  management apporaches to address changes in stocks distribution across management boundaries
                    in response to a chanign climate. The tool focuses on three stocks of the Northeast United 
                    States and is part of the project, Managing across boundaries: preventing interjurisdictional
                     conflicts arising from shifting fish stocks")
              ),
              column(
                width = 8,
                offset = 2,
                alijn = "justified",
                h4("Navigate below to know more about the project. Or go to Tool in the left pannel to access 
                     the dynamic tool")
              ), # Close fluid row
              fluidRow(
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  title = "Black sea bass",
                  status = "primary", # blue color
                  a(img(src= 'https://media.fisheries.noaa.gov/styles/original/s3/dam-migration/640x427-black-sea-bass.png?itok=7Ax8suz_',
                        height = 200,
                        width = 300),
                    title = "Go to NOAA",
                    href="https://www.fisheries.noaa.gov/species/black-sea-bass"),
                  h5("Centropristis striata")
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  title = "Scup",
                  status = "primary", # blue color
                  a(img(src= 'https://media.fisheries.noaa.gov/styles/original/s3/dam-migration/scup.png?itok=noMUoDe0',
                        height = 200,
                        width = 300),
                    href="https://www.fisheries.noaa.gov/species/scup"),
                  h5("Stenotomus chrysops")
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  title = "Summer flounder",
                  status = "primary", # blue color
                  a(img(src= 'https://media.fisheries.noaa.gov/styles/original/s3/2021-03/640x427-summer-flounder.png?itok=B6G4jkW7',
                        height = 200,
                        width = 300),
                    href="https://www.fisheries.noaa.gov/species/summer-flounder"),
                  h5("Paralichthys dentatus")
                )
              ), #Close species fluid row
              # Lenfest
              column(
                width = 8,
                offset = 2,
                alijn = "justified",
                p("This is a project funded by The Lenfest Ocean Program. Know more about the Lenfest program", a(img(src= 'logo_lenfest.jpeg',
                                                                                                                      height = 30,
                                                                                                                      width = 50),
                                                                                                                  href="https://www.lenfestocean.org/en/about-us")
                )
              ) # closes colum
      ), # Close Home body
      ## Information Body ####
      tabItem(tabName = "info",
              column(12,
                     align = "center",
                     h1("Managing across boundaries: preventing interjurisdictional conflicts arising from shifting fish stocks"),
              ),
              br(),
              br(),
              fluidRow(
                column(
                  width = 8,
                  offset = 2,
                  align = "center",
                  collapsible = TRUE,
                  h4(" This is a dynamic tool that allows to explore different
                  management apporaches to address changes in stocks distribution across management boundaries
                    in response to a chanign climate."),
                  br(),
                  h5("Navigate below to know more about the project. Or go to Tool in the left pannel to access 
                     the dynamic tool")
                )
              ),
              fluidRow(
                box(
                  title = "Project background",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  includeMarkdown("./scripts/about.Rmd") # Call the About info
                )
              ),
              br(),
              p("This is a project funded by The Lenfest Ocean Program. Know more about the Lenfest program", a(img(src= 'logo_lenfest.jpeg',
                                                                                                                    height = 30,
                                                                                                                    width = 50),
                                                                                                                href="https://www.lenfestocean.org/en/about-us")
              )
      ), # Close info body
      ## Dashboard Body ####
      tabItem(tabName = "dashboard",
              h1("Control panel"),
              fluidRow(
                # Select species box
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Select species",
                  status = "primary", # blue color
                  selectizeInput(
                    'SppSelection',"", choices = c("Centropristis striata","Paralichthys dentatus","Stenotomus chrysops"),
                    options = list(
                      placeholder = 'Type or click to search for species',
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = 'Select survey season(s)',
                  status = "primary",
                  selectizeInput(
                    'SurveySelection', 
                    "", 
                    choices = unique(spp_survey$region),
                    options = list(
                      placeholder = 'Type or click to select a season',
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Select regulatory unit (s)",
                  status = "primary",
                  selectizeInput(
                    'SpatSelection',
                    "", choices = c("State waters","Fishing ports","Both apporaches"),
                    options = list(
                      placeholder = 'Type or click to select an approach',
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  )
                ),
                box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Select year range of distribution",
                  status = "primary",
                  sliderInput(inputId = "YearSelection",
                              label = "",
                              min = 1972,
                              max = 2019,
                              step = 1,
                              sep = "",
                              animate = FALSE,
                              value = c(2010,2019)
                  )
                ),
                box(
                  width = 6,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Choose port treshold",
                  status = "primary",
                  sliderInput(inputId = "treshold",
                              label = "",
                              min =75,
                              max = 100,
                              step = 1,
                              sep = "",
                              animate = FALSE,
                              value = 75
                  ),
                  tags$p("Note: The treshold input will only apply for the Port Aproach. It determines the top ports to be included in the analysis."),
                ),
                br(),
                h1("Results"),
                fluidRow(
                  ## Regulatory units map -----------------
                  box(width = 6,
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = "Regulatory Units",
                      status = "success", # green color
                      plotOutput("RegUnit")
                  ),
                  ## Distribution map -----------------
                  box(width = 6, 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = "Stock's distribution",
                      status = "success", # green color
                      plotOutput("distPlot"))
                ),
                fluidRow(
                  ## Proportion map -----------------
                  box(width = 6, 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = "State's proportion",
                      status = "success", # green color
                      plotOutput("propPlot")),
                  ## Area plot -----------------
                  box(width = 6, 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = "Difference between historic and current distribution",
                      status = "success", # green color
                      plotOutput("propDiffPlot"))
                ),# row
                fluidRow(
                  box(width = 12, 
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = "Historical proportion of the stock",
                      status = "success", # green color
                      formattableOutput("Allocation_tbl")
                  )
                ) # row
              )
      ),
      tabItem(tabName = "instructions",
              p("plop")
      )
    )# Close instructions
  )
)