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
MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","DT","plotly","wesanderson","zoo","formattable"))

us_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

state_pallet <- c(wes_palette(n = 5, name = "Darjeeling1"),
                  wes_palette(n = 5, name = "Darjeeling2"),
                  wes_palette(n = 3, name = "Royal1")
)

# Set the filters
state_order <- my_path("D", "Spatial/grid_eez_sw", name = "grid_eez_sw_df.csv", read = T) %>%
    group_by(state) %>%
    summarise(
        order = mean(lat)
    ) %>%
    mutate(abrev = c("CT","DE","ME","MD","MA","NH","NJ","NY","NC","RI","VA")
    ) %>%
    arrange(desc(order)) %>%
    mutate(order= c("b","a",letters[3:11])) %>% # weirdly maine goes below
    arrange(order)

# ---------------------------- #
# Grid data from the SW and FP approaches
# ---------------------------- #
# Load grid of state waters
grid_eez_sw_sf <- st_read(my_path("D","Spatial/grid_eez_sw", name = "grid_eez_sw_sf.shp")) %>%
    select(index,state,geometry) %>%
    mutate(method = "state_waters",
           state = str_to_sentence(state)) %>%
    left_join(state_order)

grid_eez_sw_sf$group <- factor(grid_eez_sw_sf$abrev,      # Reordering group factor levels
                               levels = state_order$abrev)

# Load grid of fishing ports
grid_eez_fp_sf <-  st_read(my_path("D","Spatial/grid_eez_fp", name = "grid_eez_fp.shp")) %>%
    select(index,abrev=lndng_s,geometry) %>%
    mutate(method = "fishing_port"#,
           # state = str_to_sentence(landing_port)
    )%>%
    left_join(state_order)

grid_eez_fp_sf$group <- factor(grid_eez_fp_sf$abrev,      # Reordering group factor levels
                               levels = state_order$abrev)

grid_eez_fp_sf <- arrange(grid_eez_fp_sf,group)



# ---------------------------- #
# Tool start ####
# ---------------------------- #
shinyServer(function(input, output) {
    
    # ---------------------------- #
    # Biological Survey Data ####
    # ---------------------------- #
    raw_data <- reactive({
        
        name <- paste0("obs_",str_replace(input$SppSelection," ","_"),".csv")
        
        data <- my_path("D","Spp/Observation",name = name, read = T)
        
        data <- readRDS("/Volumes/Enterprise/Data/AcrossBoundaries/Data/pinskylab-OceanAdapt-966adf0/data_clean/dat_exploded.rds") %>%
            filter(
                spp %in% c("Centropristis striata","Paralichthys dentatus","Stenotomus chrysops"),
                region %in% c("Northeast US Fall" , "Northeast US Spring")
            ) #No more seasons
        
    })
    
    
    # ---------------------------- #
    # Extrapolated data
    # Triangular Irregular Surface  
    # ---------------------------- #
    tif_data <- reactive({
        
        name <- paste0("tif_",str_replace(input$SppSelection," ","_"),".csv")
        
        data <- my_path("R","Partial/Interpolation/",name = name, read = T)
        
        # For testing
        # data <- my_path("R","Partial/Interpolation/",name = "tif_Centropristis_striata.csv", read = T)
        # tif_plot_data <- data %>%
        # filter(
        # region %in% "Northeast US Fall")
        
    })
    
    
    
    
    # ---------------------------- #
    # Quota allocation plots ####
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
                   year %in% years) %>% 
            mutate(
                cpue_log10 = log10(value)
            ) %>% 
            gather("type","cpue",value:cpue_log10)
        
        # ---------------------------- #
        ## Latitudinal plot ######
        # ---------------------------- #
        if(input$PlotStyle == 2){
            
            # Set the plot data
            avr_lat <- plot_data %>% 
                filter(wtcpue > 0) %>% 
                group_by(year,spp) %>%
                summarise(mean_lat = mean(lat,na.rm = T), .groups = "drop") %>% 
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
                    MyFunctions::my_ggtheme_m(leg_pos = "right") +
                    facet_wrap(~region) +
                    ggtitle(paste(min(years),max(years)))
            }else{
                
                # ---------------------------- #
                # DIstribution map ######
                # ---------------------------- #
                if(input$PlotStyle == 3){
                    
                    # The actual map
                    
                    ggplot(us_map) +
                        geom_sf()+
                        geom_tile( data = tif_plot_data,
                                   aes(
                                       x = lon,
                                       y = lat,
                                       fill = cpue,
                                       colour = cpue
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
                        facet_wrap(~type, ncol = 2) +
                        scale_color_distiller(palette = "Spectral",
                                              guide_legend(title = "WCPUE per Haul")) +
                        scale_fill_distiller(palette = "Spectral",
                                             guide_legend(title = "WCPUE per Haul")) +
                        coord_sf(xlim = c(-76, -65),ylim = c(35, 45)) +
                        
                        MyFunctions::my_ggtheme_m(leg_pos = "right") +
                        ggtitle("Distribution estimated by using Triangular Irregular Surface method")
                    
                }
            }
        }
    })
    
    
    # ---------------------------- #
    # Distribution proportion map ######
    # ---------------------------- #
    #                 if(input$PlotStyle == 5){
    #                     
    #                     periods <-tibble(
    #                         order = c(rep("a",12),
    #                                   rep("b",12),
    #                                   rep("c",12),
    #                                   rep("d",11)
    #                         ),
    #                         label = c(rep("1973-1984",12),
    #                                   rep("1985-1996",12),
    #                                   rep("1997-2008",12),
    #                                   rep("2009-2019",11)
    #                         ),
    #                         year = c(seq(1973,1984,1),
    #                                  seq(1985,1996,1),
    #                                  seq(1997,2008,1),
    #                                  seq(2009,2019,1)
    #                         )
    #                     )
    #                     
    #                     total_fited <- tif_data() %>% 
    #                         group_by(year,region,spp) %>% 
    #                         summarise(total_value = sum(value,na.rm=T),.groups = "drop")
    #                     
    #                     
    #                     state_fit <- tif_data() %>% 
    #                         group_by(state,year,region,spp) %>% 
    #                         summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>% 
    #                         left_join(total_fited,
    #                                   by = c("year","region","spp")) %>%
    #                         mutate(percentage = state_value/total_value*100) %>% 
    #                         left_join(periods,
    #                                   by = "year") %>% 
    #                         group_by(state,order,label,region,spp) %>% 
    #                         summarise(mean_per = round(mean(percentage)),.groups = "drop") %>% 
    #                         #Only show results for spring
    #                         filter(str_detect(region,"Spring")) %>% 
    #                         mutate(spp = gsub(" ","\n",spp))
    #                     
    #                     # The plot
    #                     us_map %>% 
    #                         filter(ID %in% c("maine", "new hampshire", "massachusetts", "connecticut", 
    #                                          "rhode island", "new york", "new jersey", "delaware", "maryland",
    #                                          "virginia", "north carolina")) %>% 
    #                         rename(state = ID) %>% 
    #                         left_join(state_fit,
    #                                   by = "state") %>% 
    #                         ggplot() +
    #                         geom_sf(aes(fill = mean_per)) +
    #                         viridis::scale_fill_viridis("Average proportion\nper State", alpha = 0.8) +
    #                         facet_wrap(~label, nrow = 2) +
    #                         labs(x = "Longitude", 
    #                              y = "Latitude") +
    #                         my_ggtheme_p(facet_tx_s = 18,
    #                                      leg_pos = "bottom",
    #                                      axx_tx_ang = 45,
    #                                      ax_tx_s = 12,
    #                                      ax_tl_s = 18,
    #                                      hjust = 1) +
    #                         theme(legend.key.width = unit(1,"line"))
    #                     
    #                 }else{
    #                     
    #                     # ---------------------------- #
    #                     # Relative Change map from today ######
    #                     # ---------------------------- #
    #                     if(input$PlotStyle == 7){
    #                         
    #                         total_fited <- tif_plot_data %>% 
    #                             left_join(grids_data(),
    #                                       by = c("index","lon","lat")
    #                             ) %>%
    #                             filter(!is.na(spatial)) %>% 
    #                             group_by(year,region,spp,spatial) %>% 
    #                             summarise(total_value = sum(value,na.rm=T),.groups = "drop")
    #                         
    #                         state_fit <- tif_plot_data %>% 
    #                             left_join(grids_data(),
    #                                       by = c("index","lon","lat")
    #                             ) %>%
    #                             group_by(state,year,region,spp,spatial) %>% 
    #                             summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>% 
    #                             left_join(total_fited,
    #                                       by = c("year","region","spp","spatial")) %>%
    #                             mutate(percentage = state_value/total_value*100,
    #                                    label = ifelse(year >= 1980 & year <= 2001,"Reference",
    #                                                   ifelse(year > 2001,"Today",NA)
    #                                    )
    #                             ) %>% 
    #                             group_by(state,label,region,spp,spatial) %>% 
    #                             summarise(mean_per = round(mean(percentage)),.groups = "drop") %>% 
    #                             filter(!is.na(label)) %>% 
    #                             spread(spatial,mean_per) %>% 
    #                             mutate(difference = (fp-sw)/((fp+sw)/2)*100,
    #                                    difference = ifelse(difference > 100,100,
    #                                                        ifelse(difference < -100,-100,difference)
    #                                    )
    #                             ) %>% 
    #                             gather("spatial","mean_per",fp:difference) %>% 
    #                             mutate(spp = gsub(" ","\n",spp),
    #                                    spatial = ifelse(spatial == "fp","Fishing ports",
    #                                                     ifelse(spatial == "sw","State Waters","Difference")
    #                                    )
    #                             )
    #                         
    #                         map_plot <- land_sf %>%
    #                             left_join(state_fit,
    #                                       by = "state") %>%
    #                             filter(spatial != "Difference") %>%
    #                             ggplot() +
    #                             geom_sf(aes(fill = mean_per)) +
    #                             viridis::scale_fill_viridis("Average proportion per State", alpha = 0.8) +
    #                             facet_grid(spatial~ spp+label) +
    #                             labs(x = "",
    #                                  y = "") +
    #                             my_ggtheme_p(facet_tx_s = 20,
    #                                          leg_pos = "bottom",
    #                                          axx_tx_ang = 45,
    #                                          ax_tx_s = 12,
    #                                          ax_tl_s = 18,
    #                                          hjust = 1) +
    #                             theme(legend.key.width = unit(4,"line"))
    #                         
    #                         
    #                         diff_plot <- land_sf %>%
    #                             left_join(state_fit,
    #                                       by = "state") %>%
    #                             filter(spatial == "Difference") %>%
    #                             ggplot() +
    #                             geom_sf(aes(fill = mean_per)) +
    #                             viridis::scale_fill_viridis("Percentage difference", alpha = 0.8) +
    #                             facet_grid(spatial~ spp+label) +
    #                             labs(x = "",
    #                                  y = "") +
    #                             my_ggtheme_p(facet_tx_s = 20,
    #                                          leg_pos = "bottom",
    #                                          axx_tx_ang = 45,
    #                                          ax_tx_s = 12,
    #                                          ax_tl_s = 18,
    #                                          hjust = 1) +
    #                             theme(legend.key.width = unit(4,"line"),
    #                                   strip.background = element_blank(),
    #                                   strip.text.x = element_blank()
    #                             )
    #                         
    #                         
    #                         # Cowplot option
    #                         ggdraw() +
    #                             # Revenue circular
    #                             draw_plot(map_plot, x = 0, y = 0.2, width = 1, height = 0.8) +
    #                             # Catch circular
    #                             draw_plot(diff_plot, x = 0, y = 0, width = 1, height = 0.4) +
    #                             draw_plot_label(label = c("Latitude", "Longitude"),
    #                                             size = 18,
    #                                             angle = c(90,0),
    #                                             x = c(0,0.45),
    #                                             y = c(0.45,0.15)
    #                             )
    #                         
    #                         
    #                     }
    #                 }
    #             }
    #         }
    #     }
    # })
    
    
    ## ---------------------------- #
    # Dynamic plots (plotly) ####
    ## ---------------------------- #
    output$areaPlot <- renderPlotly({
        
        # ---------------------------- #
        ## Area plot ######
        # ---------------------------- #
        
        if(input$PlotStyle == 4){
            # Set the filters
            species <- input$SppSelection #"Gadus morhua"
            survey <- input$SurveySelection #"Northeast US Fall"
            years <- seq(input$YearSelection[1],input$YearSelection[2],1) #seq(1971,2019,1)
            # rmena_value <- ifelse(input$Rmean == 0,)
            
            
            total_fited <- tif_data() %>% 
                filter(spp %in% species,
                       region %in% survey,
                       year %in% years
                ) %>% 
                group_by(year,region) %>% 
                summarise(total_value = sum(value,na.rm=T), .groups = "drop")
            
            # group by state
            state_fit <- tif_data() %>% 
                group_by(state,year,region) %>% 
                summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>% 
                left_join(total_fited,
                          by = c("year","region")) %>%
                mutate(percentage = state_value/total_value*100) %>% 
                group_by(state,region) %>% 
                mutate(RMean = zoo::rollmean(x = percentage,
                                             input$Rmean,
                                             fill = NA,
                                             na.rm=T)
                )
            
            p <- ggplot(state_fit) +
                geom_area(
                    aes(
                        x = year,
                        y = round(RMean),
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
        }else{
            # ---------------------------- #
            # Distribution proportion map ######
            # ---------------------------- #
            if(input$PlotStyle == 6){
                
                periods <-tibble(
                    order = c(rep("a",12),
                              rep("b",12),
                              rep("c",12),
                              rep("d",11)
                    ),
                    label = c(rep("1973-1984",12),
                              rep("1985-1996",12),
                              rep("1997-2008",12),
                              rep("2009-2019",11)
                    ),
                    year = c(seq(1973,1984,1),
                             seq(1985,1996,1),
                             seq(1997,2008,1),
                             seq(2009,2019,1)
                    )
                )
                
                total_fited <- tif_data() %>% 
                    group_by(year,region,spp) %>% 
                    summarise(total_value = sum(value,na.rm=T),.groups = "drop")
                
                
                state_fit <- tif_data() %>% 
                    group_by(state,year,region,spp) %>% 
                    summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>% 
                    left_join(total_fited,
                              by = c("year","region","spp")) %>%
                    mutate(percentage = state_value/total_value*100) %>% 
                    left_join(periods,
                              by = "year") %>% 
                    group_by(state,order,label,region,spp) %>% 
                    summarise(mean_per = round(mean(percentage)),.groups = "drop") %>% 
                    #Only show results for spring
                    filter(str_detect(region,"Spring")) %>% 
                    mutate(spp = gsub(" ","\n",spp))
                
                # The plot
                p <- us_map %>% 
                    filter(ID %in% c("maine", "new hampshire", "massachusetts", "connecticut", 
                                     "rhode island", "new york", "new jersey", "delaware", "maryland",
                                     "virginia", "north carolina")) %>% 
                    rename(state = ID) %>% 
                    left_join(state_fit,
                              by = "state") %>% 
                    ggplot() +
                    geom_sf(aes(fill = mean_per,
                                text = paste(state, mean_per,"% of proportion")
                    )) +
                    viridis::scale_fill_viridis("Average proportion\nper State", alpha = 0.8) +
                    facet_wrap(~label, nrow = 2) +
                    labs(x = "Longitude", 
                         y = "Latitude") +
                    my_ggtheme_p(facet_tx_s = 18,
                                 leg_pos = "bottom",
                                 axx_tx_ang = 45,
                                 ax_tx_s = 12,
                                 ax_tl_s = 18,
                                 hjust = 1) +
                    theme(legend.key.width = unit(1,"line"))
                
                ggplotly(p,
                         tooltip = "text",
                         dynamicTicks = FALSE) 
                
            }
        }
        
    })
    
    
    # ---------------------------- #
    # Allocation Table ####
    # ---------------------------- #
    # output$Allocation_tbl <- renderDataTable({
    output$Allocation_tbl <- renderFormattable({
        
        if(input$PlotStyle == 4){
            
            species <- input$SppSelection
            survey <- input$SurveySelection
            years <- input$YearSelection
            
            # Set the plot data
            total_fited <- tif_data() %>%
                filter(#spp %in% species,
                    region %in% survey#,
                    #year %in% years
                ) %>% 
                group_by(year,region) %>%
                summarise(total_value = sum(value,na.rm=T), .groups = "drop")
            
            # group by state
            state_fit <- tif_data() %>%
                filter(#spp %in% species,
                    region %in% survey
                    #year %in% years
                ) %>% 
                group_by(state,year,region) %>%
                summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>%
                left_join(total_fited,
                          by = c("year","region")) %>%
                mutate(percentage = round(state_value/total_value*100)) %>%
                group_by(state,region) %>%
                filter(year %in% c(seq(1973,1977,1),seq(2014,2019,1))) %>%
                ungroup() %>% 
                select(state,year,percentage) %>% 
                spread(year,percentage) %>% 
                mutate(
                    State = str_to_sentence(state),
                    Change = ifelse(`2019` > `1973`,"Increase","Decrease")
                ) %>% 
                select(State,everything(),-state)
            
            #Print formattable
            formattable(state_fit, 
                        align =c("l",rep("c",10),"r"),
                        list(
                            area(col = c(`1973`: `2019`)) ~ normalize_bar("pink", 0.2),
                            Change = formatter("span", 
                                               x ~ icontext(ifelse(x == "Increase", "arrow-up", "arrow-down"), ifelse(x == "Increase", "Increase", "Decrease")), 
                                               style = x ~ style(color = ifelse(x == "Increase", "green", "red")))
                        )
            ) # close formattable
        }
    })
    #     
    
    # ---------------------------- #
    # Regulatory Units ####
    # ---------------------------- #
    output$RegUnit <- renderPlot({
        
        # ---------------------------- #
        ## State Waters ######
        # ---------------------------- #
        if(input$SpatSelection == 1){
            
            gridExtra::grid.arrange(
                # Overall (overlapping) position
                ggplot(grid_eez_sw_sf) +
                    geom_sf(aes(color =group , fill = group), alpha = 0.3) +
                    geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                           "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                           "virginia", "north carolina"))
                    ) +
                    scale_color_manual(values = state_pallet) +
                    scale_fill_manual(values = state_pallet) +
                    my_ggtheme_p(leg_pos = "",
                                 ax_tx_s = 13) +
                    coord_sf(ylim = c(30,48)) +
                    scale_y_continuous(breaks = c(30,35,40,45))+
                    labs(x = "", y = "", title = "") +
                    theme(plot.title = element_text(size = 20)),
                # Showing each state separately
                ggplot(grid_eez_sw_sf) +
                    geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                           "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                           "virginia", "north carolina"))
                    ) +
                    geom_sf(aes(color = group),size = 0.1, alpha = 0.3) +
                    facet_wrap(~ group) +
                    theme(legend.position = "top") +
                    scale_color_manual(values = state_pallet,
                                       labels = grid_eez_sw_sf %>% arrange(order) %>%  pull(state) %>% unique()) +
                    ggtitle("") +
                    my_ggtheme_p(leg_pos = "",
                                 ax_tx_s = 11,
                                 axx_tx_ang = 45,
                                 hjust = 1
                    ),
                nrow = 1)
            
        }else{
            # ---------------------------- #
            # Fishing Ports ######
            # ---------------------------- #
            if(input$SpatSelection == 2){
                
                gridExtra::grid.arrange(
                    # Overall (overlapping) position
                    ggplot(grid_eez_fp_sf) +
                        geom_sf(aes(color =group , fill = group), alpha = 0.3) +
                        geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                               "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                               "virginia", "north carolina"))
                        ) +
                        scale_color_manual(values = state_pallet) +
                        scale_fill_manual(values = state_pallet) +
                        my_ggtheme_p(leg_pos = "",
                                     ax_tx_s = 13) +
                        coord_sf(ylim = c(30,48)) +
                        scale_y_continuous(breaks = c(30,35,40,45))+
                        labs(x = "", y = "", title = "") +
                        theme(plot.title = element_text(size = 20)),
                    # Showing each state separately
                    ggplot(grid_eez_fp_sf) +
                        geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                               "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                               "virginia", "north carolina"))
                        ) +
                        geom_sf(aes(color = group),size = 0.1, alpha = 0.3) +
                        facet_wrap(~ group) +
                        theme(legend.position = "top") +
                        scale_color_manual(values = state_pallet,
                                           labels = grid_eez_fp_sf %>% arrange(order) %>%  pull(state) %>% unique()) +
                        ggtitle("") +
                        my_ggtheme_p(leg_pos = "",
                                     ax_tx_s = 11,
                                     axx_tx_ang = 45,
                                     hjust = 1
                        ),
                    nrow = 1)
                
                
            }else{
                # ---------------------------- #
                # Both maps ######
                # ---------------------------- #
                if(input$SpatSelection == 3){
                    
                    sw_map <- gridExtra::grid.arrange(
                        # Overall (overlapping) position
                        ggplot(grid_eez_sw_sf) +
                            geom_sf(aes(color =group , fill = group), alpha = 0.3) +
                            geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                                   "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                                   "virginia", "north carolina"))
                            ) +
                            scale_color_manual(values = state_pallet) +
                            scale_fill_manual(values = state_pallet) +
                            my_ggtheme_p(leg_pos = "",
                                         ax_tx_s = 13) +
                            coord_sf(ylim = c(30,48)) +
                            scale_y_continuous(breaks = c(30,35,40,45))+
                            labs(x = "", y = "", title = "State waters approach") +
                            theme(plot.title = element_text(size = 20)),
                        # Showing each state separately
                        ggplot(grid_eez_sw_sf) +
                            geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                                   "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                                   "virginia", "north carolina"))
                            ) +
                            geom_sf(aes(color = group),size = 0.1, alpha = 0.3) +
                            facet_wrap(~ group) +
                            theme(legend.position = "top") +
                            scale_color_manual(values = state_pallet,
                                               labels = grid_eez_sw_sf %>% arrange(order) %>%  pull(state) %>% unique()) +
                            ggtitle("") +
                            my_ggtheme_m(map_type = "global", leg_pos = ""),
                        nrow = 1)
                    
                    fp_map <- gridExtra::grid.arrange(
                        # Overall (overlapping) position
                        ggplot(grid_eez_fp_sf) +
                            geom_sf(aes(color =group , fill = group), alpha = 0.3) +
                            geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                                   "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                                   "virginia", "north carolina"))
                            ) +
                            scale_color_manual(values = state_pallet) +
                            scale_fill_manual(values = state_pallet) +
                            my_ggtheme_p(leg_pos = "",
                                         ax_tx_s = 13) +
                            coord_sf(ylim = c(30,48)) +
                            scale_y_continuous(breaks = c(30,35,40,45))+
                            labs(x = "", y = "", title = "Fishing ports approach") +
                            theme(plot.title = element_text(size = 20)),
                        # Showing each state separately
                        ggplot(grid_eez_fp_sf) +
                            geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                                   "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                                   "virginia", "north carolina"))
                            ) +
                            geom_sf(aes(color = group),size = 0.1, alpha = 0.3) +
                            facet_wrap(~ group) +
                            theme(legend.position = "top") +
                            scale_color_manual(values = state_pallet,
                                               labels = grid_eez_fp_sf %>% arrange(order) %>%  pull(state) %>% unique()) +
                            ggtitle("") +
                            my_ggtheme_m(map_type = "global", leg_pos = ""),
                        nrow = 1)
                    
                    # Show plots
                    gridExtra::grid.arrange(sw_map,fp_map,
                                            bottom = "Longitude",
                                            left = "Latitude")
                }
            }
        }
    })
}) # app closure

