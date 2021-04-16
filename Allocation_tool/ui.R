#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Stock allocation formula"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # Select which Gender(s) to plot
            checkboxGroupInput(inputId = "SurveySelection",
                               label = "Select Survey(s)",
                               choices = c("Fall" = "M", "Summer" = "F"),
                               selected = "Fall"),
            # Select Species
            selectInput(inputId = "SppSelection",
                        label = "Select Species",
                        # choices = levels(Species),
                        choices = c("Black Sea Bass","Y. Flounder","Spp"),
                        selected = "Balck Sea Bass",
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
                        min = 1971,
                        max = 2015,
                        step = 1,
                        sep = "",
                        animate = FALSE,
                        value = c(1971,2015),
                        width = "220px"),
        # Select which plot you want to create
        checkboxGroupInput(inputId = "PlotStyle",
                           label = "Select Result(s)",
                           choices = c("Allocation Area" = "M", 
                                       "Survey Point" = "F", 
                                       "Distribution Map" = "F",
                                       "Distribution Point" = "g",
                                       "States' Table" = "ST"),
                           selected = "ST")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
