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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    datasetInput <- reactive({
        
        data <- readRDS("/Volumes/Enterprise/Data/pinskylab-OceanAdapt-966adf0/data_clean/dat_exploded.rds") %>% 
            filter(spp %in% c ("Centropristis striata","Gadus morhua"),
                   region %in% c("Northeast US Fall" , "Northeast US Spring")) #No more seasons
        
    })
    
    # Main plot
    output$distPlot <- renderPlot({
        
        # Set the filters
        species <- input$SppSelection
        # species <- c("Centropristis striata","gadus morhua")
        # survey <- c("Northeast US Fall" , "Northeast US Spring")
        survey <- input$SurveySelection
        # years <- input$YearSelection
        
        # Set the plot data
        plot_data <- data %>%
            filter(spp %in% species,
                   region %in% survey)#,
        # year %in% years)
        
        # Density map
        if(input$PlotStyle == 2){
            
            # Set manual breaks for bins
            bins <- seq(1970,2020,10)
            
            map_data <- plot_data %>%
                group_by(spp,year,lat,lon,depth) %>%
                summarise_if(is_numeric,mean,na.rm=T) %>%
                mutate(
                    year = as.numeric(year),
                    period = cut(year, breaks = bins)
                ) %>%
                group_by(period,lat,lon) %>%
                summarise_if(is.numeric,mean,na.rm=T) %>%
                filter(wtcpue>0)
            
            
            ggplot(us_map) +
                geom_sf() +
                stat_density_2d(data = map_data, geom = "polygon",
                                aes(x = lon, y = lat, fill = ..level..)) +
                geom_point(data = map_data,
                           aes(
                               x = lon,
                               y = lat
                           ),
                           size = 0.1
                ) +
                facet_wrap(~period,
                           nrow = 1) +
                scale_fill_distiller(palette = "Spectral",
                                     guide_legend(title = "Probability \ndensity")) +
                coord_sf(xlim = c(-76, -65),ylim = c(35, 45)) +
                MyFunctions::my_ggtheme_m()
        }else{
            
            # Survey map
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
