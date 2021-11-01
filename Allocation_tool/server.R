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
MyFunctions::my_lib(c("ggmap","sf","tidyverse","tools","readr","data.table","maps","shiny","DT","plotly","wesanderson","zoo","formattable","viridis","shinydashboard"))

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
# Grid data ####
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

grids <- my_path("D", "Spatial/grid_eez_fp", name = "grid_eez_fp_df.csv", read = T) %>%
    mutate(
        spatial = "fp"
    ) %>% 
    bind_rows(
        my_path("D", "Spatial/grid_eez_sw", name = "grid_eez_sw_df.csv", read = T)
    ) %>% 
    select(-spp) %>% 
    mutate(
        spatial = ifelse(is.na(spatial),"sw",spatial)
    ) %>% 
    distinct(.keep_all = T)


# ---------------------------- #
# Quota - reference years ####
# ---------------------------- #
# Summer flounder
# ref_yrs <- c(1980,1989)

# Black sea bass
# https://static1.squarespace.com/static/511cdc7fe4b00307a2628ac6/t/5176dea1e4b083b631f27236/1366744737698/SFSCBSB_Amend_13_Vol_1compressed.pdf 
# a) a federal coastwide quota with satate-by-state allocation system Alternative 5f; setion 2.1.5.6 (Pages 3, 14, and 304-Table 5)
# ref_yrs <- c(1980:2001)


# Scup



# ---------------------------- #
# Tool start ####
# ---------------------------- #
shinyServer(function(input, output,session) {
    
    # Action button. App will only run after all options are selected
    observeEvent(input$do, {
        
        # ---------------------------- #
        # Biological Survey Data ####
        # ---------------------------- #
        raw_data <- reactive({
            
            name <- paste0("obs_",str_replace(input$SppSelection," ","_"),".csv")
            
            data <- my_path("D","Spp/Observation",name = name, read = T)
            
            # data <- readRDS("/Volumes/Enterprise/Data/AcrossBoundaries/Data/pinskylab-OceanAdapt-966adf0/data_clean/dat_exploded.rds") %>%
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
            survey <-input$SurveySelection # "Northeast US Fall"
            reg_area <- input$SpatSelection # "State waters"
            
            
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
                filter(spp %in% species,
                       region %in% survey,
                       spatial %in% reg_area)
            
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
        # Regulatory Units ####
        # ---------------------------- #
        output$gridN <- renderFormattable({
            
            
            # ---------------------------- #
            # State waters ######
            # ---------------------------- #
            
            if(input$SpatSelection == "State waters"){
                
                n_grids <- grids %>%
                    filter(spatial == "sw") %>% 
                    group_by(state) %>% 
                    summarise(n_grids = length(unique(index))) %>% 
                    arrange(n_grids) %>% 
                    spread(state,n_grids)
                
                
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
                    arrange(Ports) %>% 
                    gather("Category","type",Grids,Ports) %>% 
                    spread(state,type)
                
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
                    filter(Category != "Grids") %>% 
                    arrange(Category,state) %>% 
                    mutate(
                        Difference = abs(replace_na(fp,0) - sw)
                    ) %>% 
                    left_join(state_order) %>% 
                    arrange(order) %>% 
                    select(State = state,
                           ISO = abrev,
                           "Fishing ports" = fp,
                           "State waters" = sw,
                           Difference)
                    
                
            }
            
            formattable(n_grids,
                        align =c("l",rep("c",9),"r"),
                        list(
                        Difference = color_bar(customGreen)
                        )
            )

        })
        
        
        
        # ---------------------------- #
        # Regulatory Units ####
        # ---------------------------- #
        output$RegUnit <- renderPlot({
            
            # ---------------------------- #
            ## State Waters ######
            # ---------------------------- #
            if(input$SpatSelection == "State waters"){
                
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
                if(input$SpatSelection == "Fishing ports"){
                    
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
                    if(input$SpatSelection == "Both apporaches"){
                        
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
        
        # ---------------------------- #
        ### TIF distribution map ####
        # ---------------------------- #
        output$distPlot <- renderPlot({
            
            # Set the filters
            years <- seq(input$YearSelection[1],input$YearSelection[2],1) #seq(1971,2019,1)
            
            # Set the plot data
            plot_data <- raw_data() %>%
                filter(spp %in% input$SppSelection,
                       year %in% years)
            
            
            # Set the plot data
            tif_plot_data <- tif_data() %>%
                mutate(
                    cpue_log10 = log10(value)
                ) %>% 
                gather("type","cpue",value,cpue_log10)
            
            
            plot <- ggplot(us_map) +
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
                scale_color_distiller(palette = "Spectral",
                                      guide_legend(title = "WCPUE per Haul")) +
                scale_fill_distiller(palette = "Spectral",
                                     guide_legend(title = "WCPUE per Haul")) +
                coord_sf(xlim = c(-76, -65),ylim = c(35, 45)) +
                
                MyFunctions::my_ggtheme_m(leg_pos = "bottom") +
                ggtitle("Distribution estimated by using Triangular Irregular Surface method")
            
            
            if(input$SpatSelection != "Both apporaches"){
                plot + facet_wrap(~type, ncol = 2)
            }else{
                if(input$SpatSelection == "Both apporaches"){
                    plot + facet_wrap(~type+region, ncol = 2)
                }
            }
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
                                       ifelse(`1973` > `2019`,"Decrease","No change"))
                       )%>%
                left_join(state_order) %>% 
                arrange(order) %>% 
                mutate(State = paste0(state," (",abrev,")")) %>% 
                select(State,2:6,43:47,Change, -order)
            
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
    # Tool end ####
    # ---------------------------- #
}) # app closure