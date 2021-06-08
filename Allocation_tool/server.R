#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ---------------------------- #
# Setup
# ---------------------------- #

# Get functions and shapefiles
library(MyFunctions)
MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","DT","plotly","wesanderson"))

us_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

state_pallet <- c(wes_palette(n = 5, name = "Darjeeling1"),
                  wes_palette(n = 5, name = "Darjeeling2"),
                  wes_palette(n = 3, name = "Royal1")
)


# ---------------------------- #
# Tool start ####
# ---------------------------- #
shinyServer(function(input, output) {
    
    # ---------------------------- #
    # Biological Survey Data ####
    # ---------------------------- #
    raw_data <- reactive({
        
        data <- readRDS("/Volumes/Enterprise/Data/pinskylab-OceanAdapt-966adf0/data_clean/dat_exploded.rds") %>% 
            filter(
                # spp %in% c ("Centropristis striata","Gadus morhua"),
                region %in% c("Northeast US Fall" , "Northeast US Spring")
            ) #No more seasons
        
    })
    
    
    # ---------------------------- #
    # Extrapolated data
    # Triangular Irregular Surface  
    # ---------------------------- #
    tif_data <- reactive({
        
        data <- MyFunctions::my_path("R","Partial","interpolated_data.csv",read = T)

    })
    
    
    
    # ---------------------------- #
    # Main plot ####
    # ---------------------------- #
    output$distPlot <- renderPlot({
        # Set the filters
        species <- input$SppSelection #"Gadus morhua"
        survey <- input$SurveySelection #"Northeast US Fall"
        years <- seq(input$YearSelection[1],input$YearSelection[2],1) #seq(1971,2019,1)
        
        # Set the plot data
        plot_data <- raw_data() %>%
            filter(spp %in% species,
                   region %in% survey,
                   year %in% years)
        
        
        # Set the plot data
        tif_plot_data <- tif_data() %>%
            filter(spp %in% species,
                   region %in% survey,
                   year %in% years)
        
        # ---------------------------- #
        ## Latitudinal plot ######
        # ---------------------------- #
        if(input$PlotStyle == 1){
            
            # Set the plot data
            avr_lat <- plot_data %>% 
                filter(wtcpue > 0) %>% 
                group_by(year,spp) %>%
                summarise(mean_lat = mean(lat,na.rm = T)) %>% 
                group_by(spp) %>%
                mutate(Rmean = zoo::rollmean(x = mean_lat,
                                             5,
                                             align = "right",
                                             fill = mean_lat)
                )
            
            ggplot(avr_lat) +
                geom_line(
                    aes(
                        x = as.numeric(year),
                        y = Rmean
                    )
                ) +
                geom_point(
                    aes(
                        x = as.numeric(year),
                        y = mean_lat
                    )
                ) +
                xlab("Year") +
                ylab("Latitude") +
                MyFunctions::my_ggtheme_p() +
                ggtitle(paste("Average shift of",unique(plot_data$spp),"(5 years running mean)"))
            
        }else{
            
            # ---------------------------- #
            # Density map ######
            # ---------------------------- #
            if(input$PlotStyle == 2){
                
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
                    facet_wrap(~region) +
                    ggtitle(paste(min(years),max(years)))
            }else{
                
                # ---------------------------- #
                # DIstribution map ######
                # ---------------------------- #
                if(input$PlotStyle == 3){
                    
                    
                    # Set the plot data
                    # plot_data <- plot_data %>%
                    #     group_by(lon,lat) %>%
                    #     summarise(wtcpue = sum(wtcpue,na.rm = T)) %>%
                    #     filter(wtcpue > 0)
                    
                    # Triangular Irregular Surface
                    # fit_tin <- interp::interp( # using {interp}
                    #     x = plot_data$lon,           # the function actually accepts coordinate vectors
                    #     y = plot_data$lat,
                    #     z = plot_data$wtcpue,
                    #     xo = grd_template$lon,     # here we already define the target grid
                    #     yo = grd_template$lat,
                    #     output = "points"
                    # ) %>%
                    #     bind_cols() %>%
                    #     filter(!is.na(z))
                    
                    # The actual map
                    
                    ggplot(us_map) +
                        geom_sf()+
                        geom_tile( data = tif_plot_data,
                                   aes(
                                       x = lon,
                                       y = lat,
                                       fill =value,
                                       colour = value
                                   )
                        ) +
                        geom_point(data = subset(plot_data, wtcpue > 0),
                                   aes(
                                       x = lon,
                                       y = lat
                                   ),
                                   shape = 3,
                                   alpha = 0.05
                        ) +
                        scale_color_distiller(palette = "Spectral",
                                              guide_legend(title = "WCPUE per Haul")) +
                        scale_fill_distiller(palette = "Spectral",
                                             guide_legend(title = "WCPUE per Haul")) +
                        coord_sf(xlim = c(-76, -65),ylim = c(35, 45)) +
                        
                        MyFunctions::my_ggtheme_m() +
                        ggtitle("Distribution estimated by using Triangular Irregular Surface method")
                    
                }#else{
                #     
                #     # ---------------------------- #
                #     # Area plot ######
                #     # ---------------------------- #
                #     if(input$PlotStyle == 4){
                #         
                #         area_data <- plot_data %>% 
                #             mutate(grp = round(lat)) %>% 
                #             group_by(grp,year) %>% 
                #             summarise(tot = sum(wtcpue,na.rm=T))
                #         
                #         
                #         year_tot <- area_data %>% 
                #             group_by(year) %>% 
                #             summarise(tot_year = sum(tot,na.rm=T))
                #         
                #         area_data_p <- area_data %>% 
                #             left_join(year_tot) %>% 
                #             mutate(per = tot/tot_year)
                #         
                #         
                #         ggplot(area_data_p) +
                #             geom_area(
                #                 aes(
                #                     x = as.numeric(year),
                #                     y = per*100,
                #                     fill = as.factor(grp)
                #                 )
                #             ) +
                #             xlab("Year") +
                #             ylab("Distribution Percentage (%)") +
                #             MyFunctions::my_ggtheme_p() +
                #             viridis::scale_fill_viridis(discrete = T,
                #                                         direction = -1,
                #                                         guide_legend(title = "State")
                #             )
                #     }
                # }
                
            }
        }
        
    })
    
    
    # ---------------------------- #
    # Area plot ####
    # ---------------------------- #
    output$areaPlot <- renderPlotly({
        
        if(input$PlotStyle == 4){
            # Set the filters
            species <- input$SppSelection #"Gadus morhua"
            survey <- input$SurveySelection #"Northeast US Fall"
            years <- seq(input$YearSelection[1],input$YearSelection[2],1) #seq(1971,2019,1)
         
        
        total_fited <- tif_data() %>% 
                filter(spp %in% species,
                       region %in% survey,
                       year %in% years
                       ) %>% 
            group_by(year,region) %>% 
            summarise(total_value = sum(value,na.rm=T))
        
        # group by state
        state_fit <- tif_data() %>% 
            group_by(state,year,region) %>% 
            summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>% 
            left_join(total_fited,
                      by = c("year","region")) %>%
            mutate(percentage = state_value/total_value*100) %>% 
            group_by(state,region)
        
        p <- ggplot(state_fit) +
            geom_area(
                aes(
                    x = year,
                    y = round(percentage),
                    fill = state
                )
            ) +
            ylab("Percentage (%)") +
            scale_fill_manual(values = state_pallet) +
            MyFunctions::my_ggtheme_p() +
            theme(legend.position = "top") #+
            # facet_wrap(~region, ncol = 1)
        
        ggplotly(p,
                 dynamicTicks = TRUE,
                 height = 600) %>% 
            layout(hovermode = "x") %>% 
            # add_trace() %>% 
            rangeslider()
        }
    })
        
    
    # ---------------------------- #
    # Allocation Table ####
    # ---------------------------- #
    output$Allocation_tbl <- renderDataTable({
        
        if(input$PlotStyle == 4){

        species <- input$SppSelection
        survey <- input$SurveySelection
        years <- input$YearSelection

        # Set the plot data
        total_fited <- tif_data() %>%
        filter(#spp %in% species,
               region %in% survey,
               #year %in% years
        ) %>% 
        group_by(year,region) %>%
            summarise(total_value = sum(value,na.rm=T))

        # group by state
        state_fit <- tif_data() %>%
            filter(#spp %in% species,
                region %in% survey,
                #year %in% years
                ) %>% 
            group_by(state,year,region) %>%
            summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>%
            left_join(total_fited,
                      by = c("year","region")) %>%
            mutate(percentage = round(state_value/total_value*100)) %>%
            group_by(state,region) %>%
            filter(year %in% c(seq(1973,1977,1),seq(2015,2019,1))) %>%
            ungroup() %>% 
            select(state,year,percentage) %>% 
            spread(year,percentage)

        # Print data table
        datatable(state_fit,
                  rownames = FALSE,
                  filter = 'top',
                  escape = FALSE,
                  options = list(pageLength = 12,
                                 autoWidth = TRUE,
                                 lengthMenu = c(10, 15, 20, 50)
                  )
        )
        }
    })
#     
}) # app closure
