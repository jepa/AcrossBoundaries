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
shinyUI(fluidPage(
    tabPanel("Home",
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
                     offset = 2,
                     p(h3("The project")),
                     p(
                         "To develop and test a scientific tool for evaluating policy options for managing fish stocks that are shifting across management boundaries, and to assess the socio-economic benefits and tradeoffs of these options for two important fisheries in the U.S. Mid-Atlantic."
                     )
                 ),
                 column(
                     10,
                     align = "justified",
                     offset = 2,
                     p(strong(h4("PROJECT PARTNERS"))),
                     p("Arielle Levine, San Diego State University (Prospective Grantee)"),
                     p("Rod Fujita, Environmental Defense Fund (Project Director)"),
                     p("Katie Longo, Marine Stewardship Council"),
                     p("Olaf Jensen, University of Wisconsin, Madison"),
                     p("Lisa Wainger, University of Maryland Center for Environmental Science"),
                     p("Ellen Bolen, Virginia Marine Resources Commission"),
                     p("Scott Crosson, NOAA"),
                     p("Juliano Palacios-Abrantes, University of Wisconsin, Madison")
                 )
             ) # close fluid row
    ), # tabPanel close
    # Application title
    tabPanel("Stock allocation formula",
             br(),
             br(),
             column(
                 12,
                 align = "justified",
                 h3("Stock allocation formula"),
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
            p(h4(strong("Allocation Table"))),
            dataTableOutput("Allocation_tbl")
        )
    )
    )
))
