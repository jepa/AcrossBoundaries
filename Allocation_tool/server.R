#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","DT"))

us_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))


# First let's define a bounding Box to extrapolate from
    bbox <- c(
        "xmin" = -76,
        "ymin" = 35,
        "xmax" = -65,
        "ymax" = 45
    )

# Expand the grid
grd_template <- expand.grid(
    lon = seq(from = bbox["xmin"], to = bbox["xmax"], by = 0.5),
    lat = seq(from = bbox["ymin"], to = bbox["ymax"], by = 0.5)
)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    datasetInput <- reactive({
        
        data <- readRDS("/Volumes/Enterprise/Data/pinskylab-OceanAdapt-966adf0/data_clean/dat_exploded.rds") %>% 
            filter(
                # spp %in% c ("Centropristis striata","Gadus morhua"),
                   region %in% c("Northeast US Fall" , "Northeast US Spring")
                   ) #No more seasons
        
    })
    
    # Main plot
    output$distPlot <- renderPlot({
        # Set the filters
        species <- input$SppSelection
        survey <- input$SurveySelection
        # years <- input$YearSelection
        
        # Set the plot data
        plot_data <- datasetInput() %>%
            filter(spp %in% species,
                   region %in% survey)#,
        # year %in% years)
        
        # Density map
        if(input$PlotStyle == 1){
            
            ggplot(us_map) +
                geom_sf() +
                geom_point(data = subset(plot_data, wtcpue = 0),
                           aes(
                               x = lon,
                               y = lat
                           ),
                           color = "grey95",
                           size = 1
                ) +
                geom_point(data = subset(plot_data, wtcpue > 0),
                           aes(
                               x = lon,
                               y = lat,
                               color = log10(wtcpue)
                           ),
                           size = 1
                ) +
                scale_color_distiller(palette = "Spectral", 
                                      guide_legend(title = "WCPUE per Haul (log10)")) + 
                coord_sf(xlim = c(-76, -65),ylim = c(35, 45)) +
                MyFunctions::my_ggtheme_m() +
                facet_wrap(~region)
                
            
        }else{
            
            # Survey map
            if(input$PlotStyle == 2){
                
                # Set the filters
                species <- input$SppSelection
                survey <- input$SurveySelection
                # years <- input$YearSelection
                
                # Set the plot data
                plot_data <- datasetInput() %>%
                    filter(spp %in% species,
                           region %in% survey) %>% 
                    group_by(lon,lat) %>% 
                    summarise(wtcpue = sum(wtcpue,na.rm = T)) %>% 
                    filter(wtcpue > 0)
                
                # Triangular Irregular Surface
                fit_tin <- interp::interp( # using {interp}
                    x = plot_data$lon,           # the function actually accepts coordinate vectors
                    y = plot_data$lat,
                    z = plot_data$wtcpue,
                    xo = grd_template$lon,     # here we already define the target grid
                    yo = grd_template$lat,
                    output = "points"
                ) %>% 
                    bind_cols() %>% 
                    filter(!is.na(z))
                
                # The actual map
                
                ggplot(us_map) +
                    geom_sf()+
                    geom_tile( data = fit_tin,
                               aes(
                                   x = x,
                                   y = y,
                                   fill =z,
                                   colour = z
                               )
                    ) +
                    geom_point(data = subset(plot_data, wtcpue > 0),
                               aes(
                                   x = lon,
                                   y = lat
                               ),
                               alpha = 0.2
                    ) +
                    scale_color_distiller(palette = "Spectral", 
                                          guide_legend(title = "WCPUE per Haul")) + 
                    scale_fill_distiller(palette = "Spectral", 
                                         guide_legend(title = "WCPUE per Haul")) +
                    coord_sf(xlim = c(-76, -65),ylim = c(35, 45)) +
                    
                    MyFunctions::my_ggtheme_m() +
                    ggtitle("Distribution estimated by using Triangular Irregular Surface method")
                
            }
        }
        
    })
    
    
    # Table 
    output$Allocation_tbl <- renderDataTable({
        
        species <- input$SppSelection
        survey <- input$SurveySelection
        # years <- input$YearSelection
        
        # Set the plot data
        print_data <- datasetInput() %>%
            filter(spp %in% species,
                   region %in% survey) %>%
            group_by(year,spp,region) %>%
            summarise(
                mean = round(mean(wtcpue, na.rm= T),2)
            ) %>%
            filter(year %in% c(seq(1971,1975,1),seq(2010,2015,1))) %>%
            spread(year,mean)
        
        
        datatable(print_data,
                  rownames = FALSE,
                  filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 5,
                                 autoWidth = TRUE,
                                 lengthMenu = c(10, 15, 20, 50)
                  )
        )
    })
    
})
