#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries and data
MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny"))

spp_survey <- read.csv("./data/spp_region.csv") %>% 
    filter(
        # spp %in% c ("Centropristis striata","Gadus morhua"),
        region %in% c("Northeast US Fall" , "Northeast US Spring")) #No more seasons




# Define UI for application that draws a histogram
shinyUI(
    fluidPage( 
        # remove shiny "red" warning messages on GUI
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        navbarPage(id = "MMM_Nav_Bar",
                   # Set a theme for the tool
                   theme = shinythemes::shinytheme("spacelab"),
                   # Page title
                   "Across Boundaries",
                   tabPanel("About",
                            fluidRow(
                                column(
                                    12,
                                    align = "center",
                                    h1("Managing across boundaries: preventing interjurisdictional conflicts arising from shifting fish stocks"),
                                ),
                                br(),
                                column(
                                    10,
                                    align = "justified",
                                    p(h3("Project background")),
                                    p(
                                        "Climate change is driving shifts in fish stock distributions that will affect resource availability across
                                    different jurisdictions around the world. Elinor Ostrom’s fundamental principles of common pool
                                    resource governance include a requirement for clear boundaries over resource units. 
                                    However, when stock ranges shift, governance systems and boundaries are no longer aligned. 
                                    This misalignment has already created conflicts around access and the re-distribution of benefits 
                                    flowing from the stocks. This is a global challenge that is likely to increase as the effects of 
                                    climate change intensify in the coming years. 
                                    
                                    Specific problems that arise from the movement of stocks across jurisdictions include: 
                                    overfishing; conflicts over access; unfairness to stakeholders who have borne the costs of resource 
                                    stewardship but cannot capture the benefits due to stock movement; and high costs and conflicts associated 
                                    with re-negotiating allocation, which is typically the most contentious aspect of fishery management."
                                    ),
                                    p(h3("Objective")),
                                    p("The objective of this tool is to evaluate policy options for managing
                                    fish stocks that are shifting across management boundaries, and to assess 
                                    the socio-economic benefits and tradeoffs of these options for two important 
                                    fisheries in the U.S. Mid-Atlantic."
                                    )
                                ),
                                column(
                                    6,
                                    p(h3("Partners")),
                                    p(a("Arielle Levine, San Diego State University",href = "https://geography.sdsu.edu/people/bios/levine")),
                                    p(a("Rod Fujita, Environmental Defense Fund", href = "https://www.edf.org/people/rod-m-fujita")),
                                    p(a("Katie Longo, Marine Stewardship Council", href = "https://www.researchgate.net/profile/Catherine-Longo")),
                                    p(a("Olaf Jensen, University of Wisconsin, Madison", href = "https://limnology.wisc.edu/staff/jensen-olaf/")),
                                    p(a("Scott Crosson, NOAA", href = "https://www.fisheries.noaa.gov/contact/scott-crosson-phd")),
                                    p(a("Chris Dumas, University of North Carolina Wilmington", href ="https://csbapp.uncw.edu/data/fs/vita.aspx?id=8307")),
                                    p(a("Juliano Palacios-Abrantes, University of Wisconsin, Madison", href = "https://limnology.wisc.edu/staff/palacios-abrantes-juliano/"))
                                ),
                                column(
                                    5,
                                    img(src= 'logo_all.png',
                                        height = 300,
                                        width = 500),
                                ),
                                br(),
                                column(
                                    12,
                                    align = "center",
                                    h1("This project is part of the Leanfest"),
                                ),
                            ) # close fluid row
                   ), # tabPanel close
                   # Application title
                   tabPanel("Stock allocation formula",
                            fluidRow(
                                column(
                                    12,
                                    align = "justified",
                                    h1("Instructions"),
                                    p("Wellcome to our interactive tool. In here you will be able to visualize how marine species have been
                                      shifting, or not, along the United States East coast since 1971 to date. Please go to the Control panel
                                      section, where you can choose the species, the survey, the time period, the allocation equation and the
                                      type of result"),
                                    h2("Outputs"),
                                    p("There are currently 3 output options to choose from; Survey Point, this will return the raw survey points
                                      from the NCSF dataset; Distribution Map, this will return a distribution map of the stock based on a 
                                      Triangular Irregular Surface method; and Allocation Area that will produce the proportion of the stock's
                                      distribution that each state had from the years selected. In all cases the tool will show the first and last 
                                      5 years of data"),
                                ),
                                br(),
                                br(),
                                column(
                                    12,
                                    align = "justified",
                                    h1("Control panel"),
                                ),
                                # Sidebar with a slider input for number of bins
                                sidebarLayout(
                                    sidebarPanel(
                                        # Select which Gender(s) to plot
                                        checkboxGroupInput(inputId = "SurveySelection",
                                                           label = "Select Survey(s)",
                                                           choices = unique(spp_survey$region)#,
                                                           # selected = "Fall"
                                        ),
                                        # Select Species
                                        selectInput(inputId = "SppSelection",
                                                    label = "Select Species",
                                                    choices = unique(spp_survey$spp),
                                                    selected = "Centropristis striata",
                                                    width = "220px"
                                        ),
                                        # Set Time Range
                                        sliderInput("bins",
                                                    "Scale of Hist./Dist. allocation (%)",
                                                    min = 0,
                                                    max = 100,
                                                    value = 20),
                                        # Choose the years you want to plot
                                        sliderInput(inputId = "YearSelection",
                                                    label = "Select Year Range",
                                                    min = 1972,
                                                    max = 2019,
                                                    step = 1,
                                                    sep = "",
                                                    animate = FALSE,
                                                    value = c(1972,2019),
                                                    width = "220px"),
                                        # Select which plot you want to create
                                        checkboxGroupInput(inputId = "PlotStyle",
                                                           label = "Select Result",
                                                           choices = c("Survey Point" = 1, 
                                                                       "Distribution Map" = 2,
                                                                       "Allocation Area" = 3),
                                                           selected = 1
                                        )
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                        p(h4(strong("Result plot"))),
                                        plotOutput("distPlot"),
                                        # plotlyOutput("distPlot"),
                                        p(h4(strong("Allocation Table"))),
                                        dataTableOutput("Allocation_tbl")
                                    )
                                )
                            )
                   )
        )
    )
)