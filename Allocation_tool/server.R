#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://shiny.rstudio.com/gallery/nutrition-calculator.html

# ---------------------------- #
# Setup
# ---------------------------- #

# Get functions and shapefiles
library(MyFunctions)
MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","DT","plotly","wesanderson","zoo","formattable","viridis","shinydashboard","rmarkdown"))

# state_pallet <- c(wes_palette(n = 5, name = "Darjeeling1"),
#                   wes_palette(n = 5, name = "Darjeeling2"),
#                   wes_palette(n = 3, name = "Royal1")
# )


# Create specific state pallet from our friend Wes Andersson
state_pallet <- c(wes_palette(n = 5, name = "Darjeeling1"),
                  wes_palette(n = 5, name = "Darjeeling2"),
                  "#FD6467"
)

# Set the filters
state_order <- my_path("D", "Partial/grid_sw", name = "grid_eez_sw_df.csv", read = T) %>%
    group_by(state) %>%
    summarise(
        order = mean(lat)
    ) %>%
    mutate(abrev = c("CT","DE","ME","MD","MA","NH","NJ","NY","NC","RI","VA")
    ) %>%
    arrange(desc(order)) %>%
    mutate(order= c("b","a",letters[3:11])) %>% # weirdly maine goes below
    arrange(order)

us_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>% 
    mutate(
        state = str_to_sentence(ID)
    ) %>% 
    left_join(state_order,
              by = "state")

# ---------------------------- #
# Grid data ####
# ---------------------------- #

# grids_sw <- bind_rows(
#     my_path("D", "Partial/grid_sw", name = "grid_eez_sw_df.csv", read = T) %>% 
#         mutate(spp = "Paralichthys dentatus"),
#     my_path("D", "Partial/grid_sw", name = "grid_eez_sw_df.csv", read = T) %>% 
#         mutate(spp = "Stenotomus chrysops"),
#     my_path("D", "Partial/grid_sw", name = "grid_eez_sw_df.csv", read = T) %>% 
#         mutate(spp = "Centropristis striata")
# ) %>% 
#     left_join(state_order) %>% 
#     select(index,state,abrev,lat,lon,spp) %>% 
#     mutate(spatial = "sw",
#            port_name = NA
#     )
# 
# grids_fp <- my_path("D", "Partial/grid_fp", name = "grid_eez_fp_df.csv", read = T
# ) %>%
#     mutate(spatial = "fp",
#            spp = ifelse(spp == "summer flounder","Paralichthys dentatus", 
#                         ifelse(spp == "scup", "Stenotomus chrysops", 
#                                "Centropristis striata"
#                         )
#            )
#     )
# 
# 
# # Join both approaches
# grids <- bind_rows(grids_sw,grids_fp)

# unique(grids_fp$state)

grids <- my_path("D", "Partial/grid_sw", name = "grid_eez_sw_df.csv", read = T) %>% 
    mutate(
        spatial = "sw"
    ) %>% 
    left_join(state_order) %>% 
    select(-order) %>% 
    bind_rows(
        my_path("D", "Partial/grid_fp", name = "grid_eez_fp_df.csv", read = T)
    ) %>% 
    select(-spp) %>% 
    mutate(
        spatial = ifelse(is.na(spatial),"fp",spatial)
    ) %>% 
    distinct(.keep_all = T) %>% 
    rename(landing_port = port_name) %>% 
    left_join(state_order)


# ---------------------------- #
# Tool start ####
# ---------------------------- #
shinyServer(function(input, output,session) {
    
    
    # ---------------------------- #
    ### Run and Clear all button ####
    # ---------------------------- #
    
    # Action button. App will only run after all options are selected
    observeEvent(input$do, {
        
        # Clear all plots 
        observeEvent(input$reset,{
            
            # Remove outputs
            output$RegUnit <- renderPlot(NULL)
            output$Allocation_tbl <- renderFormattable(NULL)
            output$distPlot <- renderPlot(NULL)
            output$propPlot <- renderPlot(NULL)
            output$propDiffPlot <- renderPlot(NULL)
            output$Allocation_tbl <- renderFormattable(NULL)
        })
        
        # ---------------------------- #
        # Data ####
        # ---------------------------- #
        
        raw_data <- reactive({
            
            name <- paste0("obs_",str_replace(input$SppSelection," ","_"),".csv")
            
            data <- my_path("D","Spp/Observation",name = name, read = T)

              # Test Data
            # raw_data <- readRDS("/Volumes/Enterprise/Data/AcrossBoundaries/Data/pinskylab-OceanAdapt-966adf0/data_clean/dat_exploded.rds") %>%
            #     filter(
            #         spp %in% c("Centropristis striata","Paralichthys dentatus","Stenotomus chrysops"),
            #         region %in% c("Northeast US Fall" , "Northeast US Spring")
            #     ) #No more seasons
            
        })
        
        
        # ---------------------------- #
        # Extrapolated data
        # Triangular Irregular Surface  
        # ---------------------------- #
        
        tif_data <- reactive({
            
            name <- paste0("tif_",str_replace(input$SppSelection," ","_"),".csv")
            
            # Set the filters
            species <- input$SppSelection #"Centropristis striata"
            # survey <-input$SurveySelection # "Northeast US Fall"
            # reg_area <- input$SpatSelection # "State waters"
            
            
            data <- my_path("R","Partial/Interpolation/",name = name, read = T) %>% 
                left_join(grids,
                          by = c("index","lon","lat")
                ) %>%
                filter(!is.na(spatial)) %>% 
                mutate(
                    spatial = ifelse(spatial == "fp","Fishing ports",
                                     ifelse(spatial == "sw","State waters","Difference")
                    )
                ) %>% 
                filter(spp %in% species)
            
            # For testing
            # data <- my_path("R","Partial/Interpolation/",name = "tif_Centropristis_striata.csv", read = T)
            
            
            # tif_data <- data %>%
            #     left_join(grids,
            #               by = c("index","lat","lon")
            #     ) %>%
            #     filter(!is.na(spatial)) %>%
            #     mutate(
            #         spatial = ifelse(spatial == "fp","Fishing ports",
            #                          ifelse(spatial == "sw","State waters","Difference")
            #         )
            #     ) %>%
            #     filter(#spp %in% species,
            #            region %in% survey,
            #            # spatial %in% reg_area
            #            )
            
        })
        
        
        # ---------------------------- #
        # Regulatory Units Table ####
        # ---------------------------- #
        output$gridN <- renderFormattable({
            
            customGreen = "#71CA97"
            
            # ---------------------------- #
            # State waters ######
            # ---------------------------- #
            
            if(input$SpatSelection == "State waters"){
                
                n_grids <- grids %>%
                    filter(spatial == "sw") %>% 
                    group_by(state) %>% 
                    summarise(Grids = length(unique(index))) %>% 
                    arrange(Grids)
                
                
                x <- formattable(n_grids,
                                 list(
                                     Grids = color_bar(customGreen)
                                 )
                )
                
            } 
            # ---------------------------- #
            # Fishing Ports ######
            # ---------------------------- #
            if(input$SpatSelection == "Fishing ports"){
                
                
                n_grids <- grids %>%
                    filter(spatial == "fp") %>% 
                    group_by(state) %>% 
                    summarise(Grids = length(unique(index)),
                              Ports = length(unique(landing_port))
                    ) %>% 
                    arrange(Ports) 
                
                x <- formattable(n_grids,
                                 align =c("l",rep("c",9),"r"),
                                 list(
                                     Ports = color_bar(customGreen)
                                 )
                )
                
            }
            
            if(input$SpatSelection == "Both apporaches"){
                
                
                n_grids <-
                    grids %>%
                    group_by(state,spatial) %>% 
                    summarise(Grids = length(unique(index)),
                              Ports = length(unique(landing_port))
                    ) %>% 
                    gather("Category","type",Grids,Ports) %>% 
                    spread(spatial,type) %>% 
                    filter(Category == "Grids") %>% 
                    arrange(Category,state) %>% 
                    mutate(
                        Difference = abs(replace_na(fp,0) - sw)
                    ) %>% 
                    left_join(state_order) %>% 
                    select(State = state,
                           "Fishing ports" = fp,
                           "State waters" = sw,
                           Difference) %>% 
                    arrange(desc(Difference))
                
                
                
                x <- formattable(n_grids,
                                 align =c("l",rep("c",9),"r"),
                                 list(
                                     Difference = color_bar(customGreen)
                                 )
                )
                
            }
            x
            
        })
        
        
        
        # ---------------------------- #
        # Regulatory Units Maps ####
        # ---------------------------- #
        
        # ---------------------------- #
        ## Both approaches ####
        # ---------------------------- #
        output$RegUnit <- renderPlot({
            
            if(input$SpatSelection == "Both apporaches"){
                
                # State waters data
                grids_sw_data <- grids %>% 
                    filter(spatial == "sw")
                
                grids_sw_data$group <- factor(grids_sw_data$abrev,      # Reordering group factor levels
                                              levels = state_order$abrev)
                
                # Fishing port data
                grids_fp_data <- grids %>% 
                    filter(spatial == "fp") #%>% 
                    # group_by(index,state,abrev,lat,lon,spatial,port_name) %>% 
                    # summarise(n())
                
                grids_fp_data$group <- factor(grids_fp_data$abrev,      # Reordering group factor levels
                                              levels = state_order$abrev)
                
                
                
                # Maps
                # Overall (overlapping) position
                sw_overlapping_map <- ggplot(grids_sw_data) +
                        geom_tile(aes(x = lon,
                                      y = lat,
                                      color = group , 
                                      fill = group), 
                                  alpha = 0.3) +
                        geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                               "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                               "virginia", "north carolina"))
                        ) +
                        scale_color_manual(values = state_pallet) +
                        scale_fill_manual(values = state_pallet) +
                        my_ggtheme_p(leg_pos = "",
                                     ax_tx_s = 10) +
                        coord_sf(ylim = c(30,48)) +
                        scale_y_continuous(breaks = c(30,35,40,45))+
                        labs(x = "", y = "", title = "") +
                        theme(plot.title = element_text(size = 20))
                    
                
                # Showing each state separately
                sw_by_state_map <- ggplot(grids_sw_data) +
                        geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                               "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                               "virginia", "north carolina"))
                        ) +
                        geom_tile(aes(x = lon,
                                      y = lat,
                                      color =group , 
                                      fill = group), 
                                  alpha = 0.3) +
                        labs(x = "", y = "", title = "") +
                        facet_wrap(~ group) +
                        scale_color_manual(values = state_pallet,
                                           labels = grids_sw_data %>% arrange(group) %>%  pull(group) %>% unique()) +
                        ggtitle("") +
                        my_ggtheme_m(leg_pos = "",
                                     hjust = 1
                        ) +
                        theme(axis.text = element_blank())

                
                # Fishing ports map
            fp_overlapping_map <- ggplot(grids_fp_data) +
                        geom_tile(aes(x = lon,
                                      y = lat,
                                      color = group , 
                                      fill = group), 
                                  alpha = 0.3) +
                        geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                               "rhode island", "new york", "new jersey", "delaware", 
                                                               "maryland","virginia", "north carolina"))
                        ) +
                scale_color_manual(values = c(state_pallet[3:7],state_pallet[9:11])) +
                scale_fill_manual(values = c(state_pallet[3:7],state_pallet[9:11])) +
                        my_ggtheme_p(leg_pos = "",
                                     ax_tx_s = 11) +
                        coord_sf(ylim = c(30,48)) +
                        scale_y_continuous(breaks = c(30,35,40,45))+
                        labs(x = "", y = "", title = "") +
                        theme(plot.title = element_text(size = 20))
            
            
            fp_by_state_map <- ggplot(grids_fp_data) +
                        geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                               "rhode island", "new york", "new jersey", "delaware",
                                                               "maryland","virginia", "north carolina"))
                        ) +
                        geom_tile(aes(x = lon,
                                      y = lat,
                                      color =group , 
                                      fill = group), 
                                  alpha = 0.3) +
                        labs(x = "", y = "", title = "") +
                        facet_wrap(~ group) +
                        theme(legend.position = "top") +
                scale_color_manual(values = c(state_pallet[3:7],state_pallet[9:11])) +
                scale_fill_manual(values = c(state_pallet[3:7],state_pallet[9:11])) +
                        scale_y_continuous(breaks = c(30,35,40,45))+
                        ggtitle("") +
                        my_ggtheme_m(leg_pos = "")
            
            
            if(input$map_type == "Both"){
                gridExtra::grid.arrange(sw_overlapping_map,fp_overlapping_map,
                                        sw_by_state_map,fp_by_state_map,
                                        nrow = 2)
            }else{
                
                if(input$map_type == "Overlapping"){
                    gridExtra::grid.arrange(sw_overlapping_map,fp_overlapping_map,
                                            nrow = 1)
                }else{
                    
                    if(input$map_type == "By State"){
                        gridExtra::grid.arrange(
                                                sw_by_state_map,fp_by_state_map,
                                                nrow = 1)
                    }
                }
            } # Close output options
                # Show plots
                
                
                
            }else{
                
                
                # ---------------------------- #
                ## Specific approahc ######
                # ---------------------------- #
                if(input$SpatSelection == "State waters"){
                    grids_data <- grids %>% 
                        filter(spatial == "sw")
                    
                }else{
                
                if(input$SpatSelection == "Fishing ports"){
                    grids_data <- grids %>% 
                        filter(spatial == "fp") #%>% 
                        # group_by(index,state,abrev,lat,lon,spatial,port_name) %>% 
                        # summarise(n())
                }
                }
                
                grids_data$group <- factor(grids_data$abrev,      # Reordering group factor levels
                                           levels = state_order$abrev)
                
                
                
                # Overall (overlapping) position
                overlap_map <- ggplot(grids_data) +
                    geom_tile(aes(x = lon,
                                  y = lat,
                                  color = group , 
                                  fill = group), 
                              alpha = 0.3) +
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
                    theme(plot.title = element_text(size = 20))
                
                
                by_state_map <- ggplot(grids_data) +
                    geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                           "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                           "virginia", "north carolina"))
                    ) +
                    geom_tile(aes(x = lon,
                                  y = lat,
                                  color =group , 
                                  fill = group), 
                              alpha = 0.3) +
                    facet_wrap(~ group) +
                    scale_color_manual(values = state_pallet,
                                       labels = grids_data %>% arrange(group) %>%  pull(group) %>% unique()) +
                    ggtitle("") +
                    my_ggtheme_p(leg_pos = "",
                                 ax_tx_s = 11,
                                 axx_tx_ang = 45,
                                 hjust = 1
                    )
                
                
                # Return select output
                if(input$map_type == "Both"){
                    gridExtra::grid.arrange(overlap_map,by_state_map,nrow = 1)
                }else{
                    
                    if(input$map_type == "Overlapping"){
                        overlap_map 
                    }else{
                        
                        if(input$map_type == "By State"){
                            by_state_map
                        }
                    }
                } # Close output options
            }
            
        })
        
        
        # ---------------------------- #
        ### TIF distribution map ####
        # ---------------------------- #
        output$distPlot <- renderPlot({
            
            # Set the filters
            # years <- seq(1971,2019,1) # for testing
            years <- seq(input$YearSelection[1],input$YearSelection[2],1) 
            
            if(input$SurveySelection == "Both surveys"){
                tif_data <- tif_data()
                raw_data <- raw_data()
            }else{
                
                tif_data <- tif_data() %>%
                    filter(region %in% input$SurveySelection)
                
                raw_data <- raw_data() %>%
                    filter(region %in% input$SurveySelection)
            }
            
            
            # Set the plot data
            plot_data <- raw_data %>%
                # For testing
                # filter(spp %in% "Centropristis striata",
                       # year %in% years) %>% 
                # End testing
                filter(spp %in% input$SppSelection,
                       year %in% years) %>%
                group_by(region,spp,lon,lat) %>% 
                summarise(mean_wtcpue = mean(wtcpue,na.rm = T), .groups = "drop")
            
            
            # Set the plot data
            tif_plot_data <- tif_data %>%
                group_by(index,region,spp,lon,lat) %>% 
                summarise(mean_cpue = mean(value,na.rm = T), .groups = "drop") %>% 
                mutate(cpue_log10 = log10(mean_cpue)) %>% 
                gather("type","cpue",mean_cpue,cpue_log10)
            
            
            plot <- ggplot(us_map) +
                geom_sf()+
                geom_tile( data = subset(tif_plot_data, type == input$output),
                           aes(
                               x = lon,
                               y = lat,
                               fill = cpue,
                               colour = cpue
                           )
                ) +
                geom_point(data = subset(plot_data, mean_wtcpue > 0),
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
                
                MyFunctions::my_ggtheme_m(leg_pos = "bottom") +
                ggtitle("Distribution estimated by using Triangular Irregular Surface method")
            
            
            # if(input$SurveySelection != "Both surveys"){
            #     plot + facet_wrap(~type, ncol = 2)
            # }else{
                if(input$SurveySelection == "Both surveys"){
                    plot + facet_wrap(~region, ncol = 2)
                }else{
                    plot
                }
            # }
        })
        
        # ---------------------------- #
        ### Proportion map ######
        # ---------------------------- #
        output$propPlot <- renderPlot({
            
            
            total_fited <- tif_data() %>% 
                group_by(year,region,spp,spatial) %>% 
                summarise(total_value = sum(value,na.rm=T),.groups = "drop")
            
            
            state_fit <- tif_data() %>% 
                group_by(state,year,region,spp,spatial) %>% 
                summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>% 
                # View()
                left_join(total_fited,
                          by = c("year","region","spp","spatial")) %>%
                mutate(percentage = state_value/total_value*100,
                       label = ifelse(year >= 1980 & year <= 2001,"Reference",
                                      ifelse(year > 2001,"Today",NA)
                       )
                ) %>% 
                group_by(state,label,region,spp,spatial) %>% 
                summarise(mean_per = round(mean(percentage)),.groups = "drop") %>% 
                #Only show results for spring
                filter(!is.na(label))
            
            max <- max(state_fit$mean_per,na.rm=T)
            min <- min(state_fit$mean_per,na.rm=T)
            
            # The Map
            
            p <- us_map %>% 
                filter(ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                 "rhode island", "new york", "new jersey", "delaware", "maryland",
                                 "virginia", "north carolina")) %>%
                mutate(state = str_to_sentence(ID)) %>%
                full_join(state_fit,
                          by = "state") %>% 
                filter(!is.na(label)) %>% 
                ggplot() +
                geom_sf(aes(fill = mean_per)) +
                scale_fill_viridis("Distribution proportion (%)",
                                   alpha = 0.8, 
                                   breaks = seq(0,max,2),
                                   limits=(c(0,max))
                ) +
                # facet_wrap(~label) +
                labs(x = "", 
                     y = "") +
                my_ggtheme_p(facet_tx_s = 20,
                             leg_pos = "bottom",
                             axx_tx_ang = 45,
                             ax_tx_s = 12,
                             ax_tl_s = 18,
                             hjust = 1) +
                theme(legend.key.width = unit(3,"line")) 
            
            
            if(input$SurveySelection != "Both apporaches"){
                
                p+facet_wrap(~label)
                
            }else{
                
                p+facet_wrap(~label+region)
            }
            
        })
        
        
        # ---------------------------- #
        ### Difference map ######
        # ---------------------------- #
        output$propDiffPlot <- renderPlot({
            
            total_fited <- tif_data() %>% 
                group_by(year,region,spp,spatial) %>% 
                summarise(total_value = sum(value,na.rm=T),.groups = "drop")
            
            
            state_fit <- tif_data() %>% 
                group_by(state,year,region,spp,spatial) %>% 
                summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>% 
                left_join(total_fited,
                          by = c("year","region","spp","spatial")) %>%
                mutate(percentage = state_value/total_value*100,
                       label = ifelse(year >= 1980 & year <= 2001,"Reference",
                                      ifelse(year > 2001,"Today",NA)
                       )
                ) %>% 
                group_by(state,label,region,spp,spatial) %>% 
                summarise(mean_per = round(mean(percentage)),.groups = "drop") %>% 
                #Only show results for spring
                filter(!is.na(label)) %>% 
                replace(is.na(.),0) %>% #replace NAs by 0
                spread(label,mean_per) %>% 
                mutate(Difference = Today-Reference)
            
            # The Map
            us_map %>% 
                filter(ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                 "rhode island", "new york", "new jersey", "delaware", "maryland",
                                 "virginia", "north carolina")) %>%
                mutate(state = str_to_sentence(ID)) %>%
                left_join(state_fit,
                          by = "state") %>% 
                filter(!is.na(region)) %>% 
                ggplot() +
                geom_sf(aes(fill = Difference)) +
                scale_fill_gradient2("Difference (%)") +
                labs(x = "", 
                     y = "") +
                my_ggtheme_p(facet_tx_s = 20,
                             leg_pos = "bottom",
                             axx_tx_ang = 45,
                             ax_tx_s = 12,
                             ax_tl_s = 18,
                             hjust = 1) +
                theme(legend.key.width = unit(3,"line")) +
                facet_wrap(~region)
            
        })
        
        # ---------------------------- #
        ### Allocation Table ####
        # https://www.littlemissdata.com/blog/prettytables
        # ---------------------------- #
        
        output$Allocation_tbl <- renderFormattable({
            
            total_fited <- tif_data() %>%
                group_by(year,region,spp,spatial) %>%
                summarise(total_value = sum(value,na.rm=T),.groups = "drop")
            
            
            state_table_fit <- tif_data() %>%
                group_by(state,year,region,spp,spatial) %>%
                summarise(state_value = sum(value,na.rm= T), .groups = "drop") %>%
                left_join(total_fited,
                          by = c("year","region","spp","spatial")) %>%
                mutate(percentage = state_value/total_value*100#,
                       # label = ifelse(year >= 1980 & year <= 2001,"Reference",
                       # ifelse(year > 2001,"Today",NA)
                       # )
                ) %>%
                group_by(state,year,region,spp,spatial) %>%
                summarise(mean_per = round(mean(percentage)),.groups = "drop") %>%
                #Only show results for spring
                filter(!is.na(mean_per)) %>%
                select(state,year,mean_per) %>%
                spread(year,mean_per) %>%
                mutate(Change = ifelse(`1973` < `2019`,"Increase",
                                       ifelse(`1973` > `2019`,"Decrease","No change")),
                       Difference =  `2019`-`1973`
                ) %>%
                left_join(state_order) %>% 
                arrange(order) %>% 
                mutate(State = paste0(state," (",abrev,")")) %>% 
                select(State,2:6,43:47,Difference,Change, -order)
            
            customGreen = "#71CA97"
            
            #Print format table
            formattable(state_table_fit,
                        align =c("l",rep("c",10),"r"),
                        list(
                            # area(`1973`:`2019`) ~ normalize_bar("pink", 0.2),
                            `1973`= color_bar(customGreen),
                            `2019` = color_bar(customGreen),
                            Change = formatter("span",
                                               x ~ icontext(ifelse(x == "Increase", "arrow-up",
                                                                   ifelse(x == "Decrease", "arrow-down", NA)
                                               ), 
                                               ifelse(x == "Increase", "Increase",
                                                      ifelse(x == "Decrease", "Decrease","No change"))
                                               ),
                                               style = x ~ style(color = ifelse(x == "Increase", "green",
                                                                                ifelse(x == "Decrease", "red", "grey"))))
                        )
            )
        })  # close formattable
        
    }) # close action button
    
    
    # ---------------------------- #
    ### Download report ####
    # ---------------------------- #
    
    output$downloadReport = downloadHandler(
        filename<- function(){
            paste0(input$filename,"_",Sys.Date(),switch(
                input$format, PDF = '.pdf', Word = '.docx', HRML = '.html'
            ))
        },
        # Report content
        content = function(file) {
            if (input$format=="PDF"){
                #### Progressing indicator
                withProgress(message = 'Download in progress',
                             detail = 'This may take a while...', value = 0, {
                                 for (i in 1:15) {
                                     incProgress(1/15)
                                     Sys.sleep(0.01)
                                 }
                                 
                                 ## End of progression
                                 src <- normalizePath('summary_report.Rmd')
                                 
                                 # temporarily switch to the temp dir, in case you do not have write
                                 # permission to the current working directory
                                 owd <- setwd(tempdir())
                                 on.exit(setwd(owd))
                                 file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                                 
                                 
                                 out <- render('summary_report.Rmd', pdf_document())
                                 file.rename(out, file)
                                 
                             })
                
            }else{
                withProgress(message = 'Download in progress',
                             detail = 'This may take a while...', value = 0, {
                                 for (i in 1:15) {
                                     incProgress(1/15)
                                     Sys.sleep(0.01)
                                 }
                                 
                                 ## End of progression
                                 src <- normalizePath('summary_report.Rmd')
                                 
                                 # temporarily switch to the temp dir, in case you do not have write
                                 # permission to the current working directory
                                 owd <- setwd(tempdir())
                                 on.exit(setwd(owd))
                                 file.copy(src, 'summary_report.Rmd', overwrite = TRUE)
                                 
                                 out <- render('summary_report.Rmd', word_document())
                                 file.rename(out, file)
                             })
            }
            
        }) # Close downloadReport
    
    
    # ---------------------------- #
    # Tool Demo ####
    # ---------------------------- #
    
    observeEvent(input$do_demo, {
        
        years <- seq(2010,2015,1)
        
        # ---------------------------- #
        # Regulatory Units Map ####
        # ---------------------------- #
        
        output$DemoRegUnit <- renderPlot({
            
            grids_data <- grids %>% 
                filter(spatial == "sw")
            
            grids_data$group <- factor(grids_data$abrev,      # Reordering group factor levels
                                       levels = state_order$abrev)
            
            gridExtra::grid.arrange(
                # Overall (overlapping) position
                ggplot(grids_data) +
                    geom_tile(aes(x = lon,
                                  y = lat,
                                  color = group , 
                                  fill = group), 
                              alpha = 0.3) +
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
                ggplot(grids_data) +
                    geom_sf(data = subset(us_map,ID %in% c("maine", "new hampshire", "massachusetts", "connecticut",
                                                           "rhode island", "new york", "new jersey", "delaware", "maryland",
                                                           "virginia", "north carolina"))
                    ) +
                    geom_tile(aes(x = lon,
                                  y = lat,
                                  color =group , 
                                  fill = group), 
                              alpha = 0.3) +
                    facet_wrap(~ group) +
                    theme(legend.position = "top") +
                    scale_color_manual(values = state_pallet,
                                       labels = grids_data %>% arrange(group) %>%  pull(group) %>% unique()) +
                    ggtitle("") +
                    my_ggtheme_p(leg_pos = "",
                                 ax_tx_s = 11,
                                 axx_tx_ang = 45,
                                 hjust = 1
                    ),
                nrow = 1)
            
        }) # Close DemoRegUnit
        
        
        # ---------------------------- #
        # Regulatory Units Table ####
        # ---------------------------- #
        output$DemogridN <- renderFormattable({
            
            customGreen = "#71CA97"
            
            n_grids <- grids %>%
                filter(spatial == "sw") %>% 
                group_by(state) %>% 
                summarise(Grids = length(unique(index))) %>% 
                arrange(Grids)
            
            
            x <- formattable(n_grids,
                             list(
                                 Grids = color_bar(customGreen)
                             )
            )
            
            
        }) # CLose DemogridN
        
    }) # Close  demoButton   
    
    # ---------------------------- #
    # Tool end ####
    # ---------------------------- #
}) # app closure