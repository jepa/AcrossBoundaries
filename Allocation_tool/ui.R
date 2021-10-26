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