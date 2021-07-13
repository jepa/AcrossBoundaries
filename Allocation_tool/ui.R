#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries and data
MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","plotly"))

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
                   # ---------------------------- #
                   # About tab ######
                   # ---------------------------- #
                   tabPanel("About",
                            fluidRow(
                                column(12,
                                    align = "center",
                                    h1("Managing across boundaries: preventing interjurisdictional conflicts arising from shifting fish stocks"),
                                ),
                                br(),
                                column(
                                    8,
                                    offset = 2,
                                    align = "justified",
                                includeMarkdown("./scripts/about.Rmd") # Call the About info
                                ),
                                br(),
                                column(
                                    10,
                                    align = "center",
                                    h5("This is a project funded by The Lenfest Ocean Program"),
                                    p("Know more about the Lenfest program", a(img(src= 'logo_lenfest.jpeg',
                                                                                   height = 30,
                                                                                   width = 50),
                                                                               href="https://www.lenfestocean.org/en/about-us")
                                    )
                                ),
                            ) # close fluid row
                   ), # tabPanel About close
                   # ----------------------- #
                   # General info ####
                   # ----------------------- #
                   navbarMenu("Information",
                              tabPanel("Shared stocks",
                                       column(8,
                                              offset = 2,
                                              align = "justified",
                                              p(h3("Shared fisheries management")),
                                              p(
                                                  "From 1973 to 1982, members of the United Nations (UN) held a series of meetings to discuss 
                                               regulations regarding the high seas, a region of international common property, at that time, 
                                               consisting of waters beyond 12 nautical miles (22.22 km) from shore [@Munro:2004ug].
                                               Among the main outcomes of these meetings was the establishment of the UN Convention on the Law of the Sea (UNCLOS).
                                               Under UNCLOS coastal states were allowed to claim special rights over the exploration and
                                               exploitation of marine resources up to 200 nautical miles (370 km) from their coasts.
                                               Area within which such special rights were claimed delineate today the states' Exclusive Economic Zone (EEZ) [@UnitedNations2018].
                                               
                                               The inception of the concept of shared stocks in fisheries management called for the establishment of new fisheries governance 
                                               approaches and organizations. Many straddling and highly migratory stocks are managed by Regional Fisheries Management Organizations 
                                               (RFMOs) [@MonllorHurtado:2017cm; @Song:2017va; @CullisSuzuki:2010fi]. At the EEZ level, transboundary stocks can be jointly managed 
                                               by neighbouring countries under bi-lateral agreements or unilaterally without legal-binding agreements with other countries."
                                              ),
                                              p(h3("Classification of shared stocks")),
                                              p(
                                                  "Generally speaking, shared fish stocks are those populations that move between different jurisdictions in the course of their life.
                                               The Food and Agricultural Organization (FAO) recognizes four types of shared stocks:"
                                              ),
                                              p(
                                                  "(i) transboundary stocks are those that are shared between neighboring coastal nations e.g., Pacific halibut
                                               (*Hippoglossus stenolepis*) shared between Canada and the United States"
                                              ),
                                              p(
                                                  "(ii) straddling stocks are those that occur in two or more adjacent national jurisdictions and the high seas e.g.,
                                               Chilean jack mackerel (*Trachurus murphyi*) off the coast of Chile and Peru but also the high seas"
                                              ),
                                              p(
                                                  "(iii) highly migratory stocks are those that are found in the EEZs of coastal nations that are not necessarily 
                                               adjoining, and the high seas (e.g., Bluefin tuna *Thunnus thynnus* do circum-Atlantic migrations from the 
                                               Mediterranean to the Gulf of Mexico)"
                                              ),
                                              p("(iv) discrete high seas stocks whose distribution is limited to the high seas.")
                                       ),
                                       column(8,
                                           offset = 2,
                                           align = "center",
                                           img(src= 'shared_stocks.png',
                                               height = 400,
                                               width = 800),
                                       ),
                                       column(8,
                                              offset = 2,
                                              align = "justified",
                                              p(h3("Shared stocks between U.S. States")),
                                              p(
                                                  "In some U.S. fisheries, federal and interstate management authorities allocate fishing access to individual states, 
                                                  with each state receiving a fixed fraction of the total allowable harvest. These allocations are typically based on 
                                                  the state’s historical harvest during some previous period. But as fish ranges shift, these state allocations become 
                                                  increasingly disconnected from the actual distribution of the fish. A more adaptive policy would allocate harvest based 
                                                  on current geographic distributions of fish, with some consideration given to other factors such as catch history, 
                                                  socio-economic impact, and the rate at which fish are re-allocated.  States whose waters contain a higher abundance of 
                                                  any given stock would generally receive a higher allocation of access. This would be expected to reduce trip times, fishing costs, 
                                                  and the carbon footprint of the industry, and it may allow fishers to deliver a fresher, higher quality product to market. 
                                                  But shifting allocation too quickly could create economic and social dislocation, as shoreside processing facilities and fishing 
                                                  communities would be hard-pressed to keep up with such rapid changes.  Adaptive allocation triggered by observed changes in biomass
                                                  distribution and modified by consideration of other factors, according to an agreed upon rule,  
                                                  could reduce conflict and transaction costs associated with repeatedly re-negotiating the allocations. "
                                              )
                                       )
                              ),
                              tabPanel("Climate change",
                                       column(
                                           8,
                                           offset = 2,
                                           align = "justified",
                                       includeMarkdown("./scripts/climate_change.Rmd")
                                       )
                                       ),
                              tabPanel("Dynamic management")
                              
                   ), # close information tab
                   # ----------------------- #
                   # Tool UI ####
                   # ----------------------- #
                   tabPanel("Quota allocation",
                            fluidRow(
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
                                                           choices = unique(spp_survey$region),
                                                           selected = unique(spp_survey$region)[1]
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
                                                           choices = c(
                                                               "Latitudinal Shift" = 1,
                                                               "Survey Point" = 2, 
                                                               "Distribution Map" = 3,
                                                               "Allocation Area" = 4
                                                               ),
                                                           selected = 1
                                        ),
                                    #Set RMean
                                    sliderInput(inputId = "Rmean",
                                                label = "Running average for allocation area",
                                                min = 1,
                                                max = 20,
                                                value = 1)
                                    ), # Close side panel
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                        # Condition between plot and plotly
                                        conditionalPanel(
                                            condition = "input.PlotStyle != 4",
                                        plotOutput("distPlot")
                                        ),
                                        conditionalPanel(
                                            condition = "input.PlotStyle == 4",
                                            plotlyOutput("areaPlot")
                                        )
                                    )
                                ),
                                br(),
                                br(),
                                conditionalPanel(
                                    condition = "input.PlotStyle == 4",
                                    p(h3(strong("Quota allocation table"))),
                                    dataTableOutput("Allocation_tbl")
                                ),
                                # ----------- #
                                # Instructions
                                # ----------- #
                                column(
                                    5,
                                    align = "justified",
                                    h3("Instructions"),
                                    p("This is our interactive tool. In here you will be able to visualize how marine species have been
                                      shifting, or not, along the United States East coast since 1971 to date. Please go to the Control panel
                                      section, where you can choose the species, the survey, the time period, the allocation equation and the
                                      type of outputs")
                                ),
                                column(
                                    5,
                                    offset = 1,
                                    h3("Outputs"),
                                    p("There are currently 3 output options to choose from; Survey Point, this will return the raw survey points
                                      from the NCSF dataset; Distribution Map, this will return a distribution map of the stock based on a 
                                      Triangular Irregular Surface method; and Allocation Area that will produce the proportion of the stock's
                                      distribution that each state had from the years selected. In all cases the tool will show the first and last 
                                      5 years of data")
                                )
                            )
                            
                   )
        )
    )
)