#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","plotly","formattable","shinydashboard","shinyjs"))

spp_survey <- read.csv("./data/spp_region.csv") %>% 
  filter(
    # spp %in% c ("Centropristis striata","Gadus morhua"),
    region %in% c("Northeast US Fall" , "Northeast US Spring")) #No more seasons



dashboardPage(
  dashboardHeader(title = "Across Boundaries Project"),
  #______________________________________________________________________#
  # Sidebar  ####
  #______________________________________________________________________#
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      br(),
      menuItem("Information", tabName = "info", icon = icon("info")),
      br(),
      menuItem("Tool", tabName = "dashboard", icon = icon("dashboard")),
      br(),
      menuItem("Instructions", tabName = "instructions", icon = icon("gear")),
      br(),
      menuItem(
        "Download report",
        tabName = "download",
        icon = icon("download"),
        textInput(
          inputId = "filename",
          placeholder = "Name download file",
          label = ""
        ),
        div(
          downloadButton(
            outputId = "downloadData",
            label = "Download report",
            icon = icon("download"),
            style = "color: black; margin-left: 15px; margin-bottom: 5px;"
          )
        ),
        div(
          downloadButton(
            outputId = "downloadMicroData",
            label = "Download processed data",
            icon = icon("download"),
            style = "color: black; margin-left: 15px; margin-bottom: 5px;"
          )
        )
      ),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
    ),
    hr(style = "border-top: 3px dashed lightgrey;"),
    p("For any inquery regarding this tool, please email us at 
    jpalacios2[at]wisc.edu"),
    a(img(src= 'https://github.githubassets.com/images/modules/logos_page/Octocat.png',
          height = 50,
          width = 50),
      href="https://github.com/jepa/AcrossBoundaries")
  ),
  #______________________________________________________________________#
  # Body ####
  #______________________________________________________________________#
  dashboardBody(
    tabItems(
      #______________________________________________________________________#
      ## Home ####
      #______________________________________________________________________#
      tabItem(tabName = "home",
              column(12,
                     align = "center",
                     h1("Welcome to the Across Boundaries quota allocation tool!"),
              ),
              column(
                width = 10,
                offset = 1,
                align = "justified",
                h4("This tool was build with the objective of exploring different
                  management apporaches to address changes in stocks distribution across management boundaries
                    in response to a changing climate. The tool focuses on three stocks of the Northeast United 
                    States and is part of the project", em("Managing across boundaries: preventing interjurisdictional
                     conflicts arising from shifting fish stocks"))
              ),
              column(
                width = 10,
                offset = 1,
                align = "justified",
                h4("Below you can find information on the stocks available to explore in the tool.
                  Navigate to",em("Tool"),"in the left pannel to access 
                   the dynamic tool", em("Information"), "to know more about the project and", em("Instructions"),
                  "to get familiarized with the tool.")
              ), # Close fluid row
              column(
                width = 12,
                align = "justified",
                h2("Info on availabel stocks"),
                h4("Click in each picture to go to NOAAs site for the management of the stock")
              ),
              fluidRow(
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  title = "Black sea bass",
                  background = "black",
                  a(img(src= 'https://media.fisheries.noaa.gov/styles/original/s3/dam-migration/640x427-black-sea-bass.png?itok=7Ax8suz_',
                        height = 200,
                        width = 300),
                    title = "Go to NOAA",
                    href="https://www.fisheries.noaa.gov/species/black-sea-bass"),
                  h5(em("Centropristis striata"))
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  title = "Scup",
                  background = "black",
                  a(img(src= 'https://media.fisheries.noaa.gov/styles/original/s3/dam-migration/scup.png?itok=noMUoDe0',
                        height = 200,
                        width = 300),
                    href="https://www.fisheries.noaa.gov/species/scup"),
                  h5(em("Stenotomus chrysops"))
                ),
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  title = "Summer flounder",
                  background = "black",
                  a(img(src= 'https://media.fisheries.noaa.gov/styles/original/s3/2021-03/640x427-summer-flounder.png?itok=B6G4jkW7',
                        height = 200,
                        width = 300),
                    href="https://www.fisheries.noaa.gov/species/summer-flounder"),
                  h5(em("Paralichthys dentatus"))
                ),
              ), #Close species fluid row
              fluidRow(
                p("This is a project funded by The Lenfest Ocean Program. Know more about the Lenfest program", a(img(src= 'logo_lenfest.jpeg',
                                                                                                                      height = 30,
                                                                                                                      width = 50),
                                                                                                                  href="https://www.lenfestocean.org/en/about-us"),
                  "This tool is open access and all code can be found in",a(img(src= 'https://github.githubassets.com/images/modules/logos_page/GitHub-Logo.png',
                                                                                height = 15,
                                                                                width = 50),
                                                                            href="https://github.com/jepa/AcrossBoundaries")
                )
              ) # Close footer fluid row
      ), # Close Home body
      #______________________________________________________________________#
      ## Information ####
      #______________________________________________________________________#
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
                  h4("In this section you will find information about the project including some background information, the project's obectives,
                     as well as the partners involved in developing the tool and the research")
                )
              ),
              fluidRow(
                box(
                  title = "Project background",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  h4("Climate change is driving shifts in fish stock distributions that will affect resource availability across different jurisdictions around the world. Elinor Ostrom’s fundamental principles of common pool resource governance include a requirement for clear boundaries over resource units. However, when stock ranges shift, governance systems and boundaries are no longer aligned. This misalignment has already created conflicts around access and the re-distribution of benefits flowing from the stocks. This is a global challenge that is likely to increase as the effects of climate change intensify in the coming years. Specific problems that arise from the movement of stocks across jurisdictions include: overfishing; conflicts over access; unfairness to stakeholders who have borne the costs of resource stewardship but cannot capture the benefits due to stock movement; and high costs and conflicts associated with re-negotiating allocation, which is typically the most contentious aspect of fishery management.")
                )
              ),
              fluidRow(
                box(
                  title = "Project Objectives",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4("The objective of this tool is to evaluate policy options for managing fish stocks that are shifting across management boundaries, and to assess the socio-economic benefits and tradeoffs of these options for two important fisheries in the U.S. Mid-Atlantic,"),
                  
                  
                  h4("1. Would  adaptive allocation  have reduced fuel use and carbon emissions, had it been applied when black sea bass and summer flounder stocks started to shift?"),
                  
                  h4("2. Would adaptive allocation have a positive economic impact on these fisheries, had it been applied when black sea bass and summer flounder stocks started to shift?"),
                  
                  h4("3.	How can conflicts around allocation be reduced in the context of shifting stocks?"),
                  
                  h4("4.	Where else in the U.S.  could this type of solution be applied to alleviate the challenges that arise when stocks shift?")
                )
              ),
              fluidRow(
                box(
                  title = "Partners",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  h4(a("- Arielle Levine, San Diego State University", href="https://geography.sdsu.edu/people/bios/levine")),
                  h4(a("- Chris Dumas, University of North Carolina Wilmington", href = "https://csbapp.uncw.edu/data/fs/vita.aspx?id=8307)")),
                  h4(a("- Juliano Palacios-Abrantes, University of Wisconsin-Madison", href = "https://limnology.wisc.edu/staff/palacios-abrantes-juliano/)")),
                  h4(a("- Katie Longo, Marine Stewardship Council", href = "https://www.researchgate.net/profile/Catherine-Longo")),
                  h4(a("- Lisa Wainger, University of Maryland Center for Environmental Science", href = "https://www.umces.edu/lisa-wainger)")),
                  h4(a("- Olaf Jensen, University of Wisconsin, Madison", href = "https://limnology.wisc.edu/staff/jensen-olaf/)")),
                  h4(a("- Rod Fujita, Environmental Defense Fund", href ="https://www.edf.org/people/rod-m-fujita")),
                  h4(a("- Scott Crosson, National Oceanic and Atmospheric Administration", href = "https://www.fisheries.noaa.gov/contact/scott-crosson-phd)"))
                ) # close last box
              )
      ), # Close info body
      #______________________________________________________________________#
      ## Tool ####
      #______________________________________________________________________#
      tabItem(tabName = "dashboard",
              useShinyjs(),
              h1("Control panel"),
              h4(strong("Read me:"),"Please select from the following options in the blue boxes.", em("Note that all options need to be selected in order for results to apear")),
              #______________________________________________________________________#
              ### Control panel ####
              #______________________________________________________________________#
              fluidRow(
                # Select species box
                box(
                  width = 4,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  title = "Select species",
                  status = "primary", # blue color
                  # p("Inst. Select here the species you want to explore"),
                  selectizeInput(
                    "SppSelection","",
                    choices = c("Centropristis striata","Paralichthys dentatus","Stenotomus chrysops"),
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
                    ),
                    selected = unique(spp_survey$region)[1]
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
                    ),
                    selected = "State waters"
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
                )
              ),
              column(12,
                     align = "center",
                     h4(strong("Read me:"), "Before running the query, select the outputs you want to show by clicking in the + sign of each box. Note that this step might take a couple of seconds"),
              actionButton("do",
                           "Run Query",
                           icon = icon("gear"),
                           class = "btn-warning"
                           ),
              actionButton("reset",
                           "Clear all",
                           icon = icon("clean"),
                           class = "btn-warning"
              ),
              ),
              hr(style = "border-top: 3px dashed lightgrey;"),
              h1("Results"),
              h4("Here you will see the results from your query.", em("Note some graphs can take some time to apear depending on your internet connection")),
              #______________________________________________________________________#
              ### Results ####
              #______________________________________________________________________#
              fluidRow(
                #### Regulatory units map -----------------
                box(width = 6,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Regulatory Units",
                    status = "success", # green color
                    plotOutput("RegUnit")
                ),
                #### Distribution map -----------------
                box(width = 6, 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Stock's distribution",
                    status = "success", # green color
                    plotOutput("distPlot")
                )
              ), # close firs fluid row
              fluidRow(
                #### Proportion map -----------------
                box(width = 6, 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "State's proportion",
                    status = "success", # green color
                    plotOutput("propPlot")),
                #### Area plot -----------------
                box(width = 6, 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Difference between historic and current distribution",
                    status = "success", # green color
                    plotOutput("propDiffPlot")
                )
              ),# # close second fluid row
              fluidRow(
                box(width = 12, 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "Historical proportion of the stock",
                    status = "success", # green color
                    formattableOutput("Allocation_tbl")
                )
              ) # close third fluid row
      ), # Close tool body
      #______________________________________________________________________#
      ## Instructions ####
      #______________________________________________________________________#
      tabItem(tabName = "instructions",
              p("plop")
      )
      
    )
  )# Close instructions
)
# )