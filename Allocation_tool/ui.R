#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries and data
MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","plotly","formattable","shinydashboard"))

spp_survey <- read.csv("./data/spp_region.csv") %>% 
    filter(
        # spp %in% c ("Centropristis striata","Gadus morhua"),
        region %in% c("Northeast US Fall" , "Northeast US Spring")) #No more seasons




# Define UI for application that draws a histogram

dashboardPage(
    dashboardHeader(title = "Across Boundaries Project"),
    dashboardSidebar(
        selectizeInput(
            'SppSelection', '1. Species', choices = c("Centropristis striata","Paralichthys dentatus","Stenotomus chrysops"),
            options = list(
                placeholder = 'Type to search for species',
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        selectizeInput(
            'SurveySelection', 
            '2. Select Survey Season(s)', 
            choices = unique(spp_survey$region),
            options = list(
                placeholder = 'Type to select a season',
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        selectizeInput(
            'SpatSelection',
            '2. Select Regulatory Unit', choices = c("State waters","Fishing ports","Both apporaches"),
            options = list(
                placeholder = 'Select an approach',
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        sliderInput(inputId = "YearSelection",
                    label = "Select year range of distribution",
                    min = 1972,
                    max = 2019,
                    step = 1,
                    sep = "",
                    animate = FALSE,
                    value = c(2010,2019)
        ),
        sliderInput(inputId = "treshold",
                    label = "Choose port trshold",
                    min =75,
                    max = 100,
                    step = 1,
                    sep = "",
                    animate = FALSE,
                    value = 75
        ),
        tags$p("Note: The treshold input will only apply for the Port Aproach. It determines the top ports to be included in the analysis."),
        
        actionButton("add", "Press me!"),
        actionButton("reset", "Reset")#,
    ),
    dashboardBody(
        fluidRow(
            valueBoxOutput("calories"),
            valueBoxOutput("over_nutrient"),
            valueBoxOutput("rich_nutrient")
        ),
        fluidRow(
            ## Regulatory units map -----------------
            box(title = "Regulatory Units",
                solidHeader = T,
                width = 6,
                collapsible = T,
                plotOutput("RegUnit")
            ),
        ## Distribution map -----------------
        box(title = "Distribution", solidHeader = T,
            width = 6, collapsible = T,
            plotOutput("distPlot"))
        ),
        fluidRow(
            ## Proportion map -----------------
            box(title = "Proportion", solidHeader = T,
                width = 6, collapsible = T,
                plotOutput("propPlot")),
            ## Area plot -----------------
            box(title = "Proportion Difference", solidHeader = T,
                width = 6, collapsible = T,
                plotOutput("propDiffPlot"))
        ),# row
        fluidRow(
            box(title = "Proportion", solidHeader = T,
                width = 12, collapsible = T,
                formattableOutput("Allocation_tbl")
            )
        ) # row
    ) # body
)



#                                         checkboxGroupInput(inputId = "PlotStyle",
#                                                            label = "Select Result",
#                                                            choices = c(
#                                                                "Survey Points" = 1,
#                                                                "Latitudinal Shift" = 2,
#                                                                "Distribution" = 3
#                                                            ),
#                                                            selected = 1
#                                         )
#                                     ),
#                                     mainPanel(
#                                         
#                                     ) # main panel
#                                 ),
#                                 br(),
#                                 column(
#                                     12,
#                                     align = "justified",
#                                     h1("Determine the reugulatory unit approach")
#                                 ),
#                                 # ----------------------- #
#                                 ## Regulatory Unit ####
#                                 # ----------------------- #
#                                 # Sidebar with a slider input for number of bins
#                                 sidebarLayout(
#                                     sidebarPanel(
#                                         # Select sf Area
#                                         selectInput(inputId = "SpatSelection",
#                                                     label = "Select Management Unit",
#                                                     choices = c("State waters" = 1,
#                                                                 "Fishing ports" = 2,
#                                                                 "Both apporaches" = 3),
#                                                     # selected = "State waters",
#                                                     width = "220px"
#                                         ),
#                                         p("If you chosed the port approach, determine a treshold"),
#                                         # Set Time Range
#                                         sliderInput("PortTreshold",
#                                                     "Port threshold",
#                                                     min = 5,
#                                                     max = 95,
#                                                     value = 75)
#                                     ), # Close side panel
#                                     # Show a plot of the generated distribution
#                                     mainPanel(plotOutput("RegUnit")) # main panel
#                                 ), # Side bar
#                                 # ----------- #
#                                 # Allocations results
#                                 # ----------- #
#                                 br(),
#                                 column(
#                                     12,
#                                     align = "justified",
#                                     h1("Allocation Results")
#                                 ),
#                                 # Sidebar with a slider input for number of bins
#                                 sidebarLayout(
#                                     sidebarPanel(
#                                         # Select which plot you want to create
#                                         checkboxGroupInput(inputId = "ResultType",
#                                                            label = "Select Result",
#                                                            choices = c(
#                                                                "Proportion change" = 1,
#                                                                "Proportion change" = 2,
#                                                                "Allocation Area" = 3
#                                                            ),
#                                                            selected = 1
#                                         )
#                                     ),
#                                     # Show a plot of the generated distribution
#                                     mainPanel(plotOutput("QuotaResult")) # main panel
#                                 ),
#                                 
#                                 # ----------- #
#                                 # Instructions
#                                 # ----------- #
#                                 column(
#                                     5,
#                                     align = "justified",
#                                     h3("Instructions"),
#                                     p("This is our interactive tool. In here you will be able to visualize how marine species have been
#                                       shifting, or not, along the United States East coast since 1971 to date. Please go to the Control panel
#                                       section, where you can choose the species, the survey, the time period, the allocation equation and the
#                                       type of outputs")
#                                 ),
#                                 column(
#                                     5,
#                                     offset = 1,
#                                     h3("Outputs"),
#                                     p("There are currently 3 output options to choose from; Survey Point, this will return the raw survey points
#                                       from the NCSF dataset; Distribution Map, this will return a distribution map of the stock based on a 
#                                       Triangular Irregular Surface method; and Allocation Area that will produce the proportion of the stock's
#                                       distribution that each state had from the years selected. In all cases the tool will show the first and last 
#                                       5 years of data")
#                                 )
#                             )
#                             
#                    )
#                    
#         )
#     )
# )


# Condition between plot and plotly
# conditionalPanel(
#     condition = "SpatSelection == Both apporaches",
#     box(title = "Distribution", solidHeader = T,
#         width = 6, collapsible = T,
#         plotOutput("DiffpropPlot")
#     )
# ),
# conditionalPanel(
#     condition = "SpatSelection != Both apporaches",
#     box(title = "Distribution", solidHeader = T,
#         width = 6, collapsible = T,
#         plotOutput("distPlot")
#     )
# )