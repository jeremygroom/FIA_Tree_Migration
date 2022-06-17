



Map1_UI <- function(id) {
  tagList(
    useShinyjs(),                 # allows toggleState function for Plotly
    fluidRow(column(width = 10, offset = 1, align = "center", h1("Map and plots of temperature, precipitation, and species' ranges"))),
    fluidRow(column(width = 8, offset = 2, align = "center",
                    box(width = 12,
                        p("These figures all display the raw FIA plot values.  Note that under \" select metric\" users can choose a change option
                          for precipitation and temperature.  This is the difference in plots' metrics, the second 10 years - the first 10 years.  
                          Under \"Select species data to display\" the default option is \"all\".  This provides all FIA plot PRISM data in Oregon,
                          Washington, and California (Note that most FIA plots are not visited because they do not contain forest).", style = "font-size:20px"),
                        p("In the box to the right of the map figure are three other figures.  All three respond to the metric and species selected, plus the \"Occupancy
                        or density results\ selection.  Once you select a species, note that by clicking on the legend you can turn the display of different categories
                          on or off.  The upper-left plot displays the distribution of precipitation and temperature data.  The upper-right plot shows
                          a first or second 10-year metric on the X axis and the corresponding change metric on the Y axis. It also includes smoothers (
                          loess or GAM) for each legend option.  The purpose of this figure is to evaluate the stableness of a metric's change across the X axis values.
                          The purpose of the lower-left plot is to display the (non-weighted) mean and density of data from plot categories.", style = "font-size:20px")))),
    
    
    fluidRow(box(width = 6, offset = 3,
                 column(width = 6, selectInput(NS(id, "metric"), label = h3("Select metric"),
                                               choices = list("Precipitation, First 10 years" = 1, 
                                                              "Precipitation, Second 10 years" = 2,
                                                              "Change in Precipitation" = 3,
                                                              "Temperature, First 10 years" = 4,
                                                              "Temperature, Second 10 years" = 5,
                                                              "Change in Temperature" = 6))),
                 column(width = 6, selectInput(NS(id, "sppname"), label = h3("Select species data to display"),
                                               choices = map.choices)))),   # "map.choices" is a long list (see global.r)
    
    
    
    # Tables UI
    fluidRow(box(width = 4, offset = 1,
                 radioButtons(NS(id, "map_scale"), label = h4("Extent of map"),
                              choices = list(
                                "Full map extent" = 1,
                                "Species range extent" = 2)),   
                 fluidRow(width = 11, plotOutput(NS(id, "plot_map1"), height = 800))),
             box(width = 8, 
                 column(width = 6, h3("Data Distribution by Precipitation and Temperature")),
                 column(width = 6, h3("How consistent was the metric change across the first or second visit values?")),
                 fluidRow(
                   column(width = 6, radioButtons(NS(id, "occ_num"), label = h4("Occupancy or density results"),
                                                  choices = list(
                                                    "Occupancy" = 1,
                                                    "Change in density" = 2))
                   ),
                   column(width = 6, radioButtons(NS(id, "ci"), label = h4("Show Loess Smoother CI?"),
                                                  choices = list(
                                                    "Yes" = 1,
                                                    "No" = 2))
                   )
                 ),
                 fluidRow(column(width = 6, plotlyOutput(NS(id, "change_plot"), height = 400, width = 600)),
                          column(width = 6, plotlyOutput(NS(id, "abs_change_plot"), height = 400, width = 600))
                 ),
                 fluidRow(column(width = 6, h3("Density plots of changes in metrics with means"))),
                 fluidRow(column(width = 6, plotOutput(NS(id, "hist_plot"), height = 600, width = 600)))
             )
    )
  )
}



Map1_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    
    
    # Want to turn off change in density (tree number) option if that analysis was not conducted for a species.
       # First the occupancy option is selected.
    observe({    
      if (as.numeric(input$sppname) %in% spp.absent.gt$id) {
        updateRadioButtons(session, inputId = "occ_num", selected = 1)
      }
    })
       # Then the radio button is deactivated (grayed out). 
    observe({  
      toggleState("occ_num", condition = (as.numeric(input$sppname) %in% spp.absent.gt$id == FALSE))
    })     
    
    
    ## This reactive gathers together all data used.
    map.data <- reactive({
      
        # Need to set which columns are used according to the options selected.
      TC.col.use.index <- switch(as.numeric(input$metric), 3, 4, 7, 1, 2, 6)
      mapvar <- colnames(TC1.2.[TC.col.use.index]) 
      
      if (input$sppname == 1) {  # If the "all species" option is selected, the occupancy/map data are selected and the density (number) data are set to NULL.
        mapdat.out <- TC1.2.     # Toggling between the two options will not change the plots for "all species"
        num.dat2 <- NULL
        
      } else {  # Else, a single species was selected.
        
        spp.code <- spp.names2$spp.codes[spp.names2$id == as.numeric(input$sppname)]  # Sets the code number for the selected species
        
        # I need to join the occupancy data between the two visits.
        spp.dat1 <- occ_orig %>% filter(State_Plot != 0) %>% dplyr::select(State_Plot, paste0("X", spp.code)) %>%  # Reduces the table to include just StatePlot and species code of interest. 
          left_join(occ_revis[occ_revis$State_Plot != 0, c(1, which(names(occ_revis) == paste0("X", spp.code)))], by = "State_Plot") 
        names(spp.dat1)[2:3] <- c("orig", "revis")
        spp.dat2 <- spp.dat1 %>% filter(orig > 0 | revis > 0) %>%  # Reducing the data set by removing plots without a species.
          left_join(TC1.2., by = "State_Plot")
        
        # Doing the same for changes in tree density, so long as we performed a density change analysis. 
        if (paste0("X", spp.code) %in% colnames(num_gt)) {
          num.dat1 <- num_gt %>% filter(State_Plot != 0) %>% dplyr::select(State_Plot, paste0("X", spp.code)) %>%
            left_join(num_lt[num_lt$State_Plot != 0, c(1, which(names(num_lt) == paste0("X", spp.code)))], by = "State_Plot") 
          names(num.dat1)[2:3] <- c("gt", "lt")
          num.dat2 <- num.dat1 %>% filter(gt > 0 | lt > 0) %>% 
            left_join(TC1.2., by = "State_Plot")
          
        } else {
          num.dat2 <- NULL  # No analysis for the selected species is available.
        } 
        
        mapdat.out <- spp.dat2
        num.out <- num.dat2
      }
      
      return(list(mapvar = mapvar, mapdat.out = mapdat.out, num.out = num.dat2))
    })
    
    
    ### Map plot ###
    output$plot_map1 <- renderPlot({

      map.plot.dat <- map.data()$mapdat.out
      map.var <- map.data()$mapvar  # The selected variable (e.g., "pre.precip" = visit timing and metric of choice)

      if (input$map_scale == "1") {  # If "All spp" is selected then the full data file (TC1.2.) is used
        maxlat <- max(TC1.2.$LAT); minlat <- min(TC1.2.$LAT)
        maxlong <- max(TC1.2.$LON); minlong <- min(TC1.2.$LON)
      } else {  # Otherwise, working with a species subset of TC1.2.
        maxlat <- max(map.plot.dat$LAT); minlat <- min(map.plot.dat$LAT)
        maxlong <- max(map.plot.dat$LON); minlong <- min(map.plot.dat$LON)
      }
      
      #browser()
      ggplot() +
        coord_fixed(xlim = c(minlong - 1, maxlong + 1),  ylim = c(minlat - 1, maxlat + 1), ratio = 1.3) +
        geom_tile(data = map.plot.dat, mapping = aes(x = LON, y = LAT, z = get(map.var)), binwidth = 0.15, stat = "summary_2d", fun = mean, na.rm = TRUE) + 
        scale_fill_viridis(option = "H") +
        geom_polygon(data = west_df, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "transparent") +
        theme_void() 
      #+
      #geom_polygon(data = west_county, aes(x = long, y = lat, group = group), fill = NA, color = "white") 
    })
    
    
    
    ### Plot of temperature and precipitation change  ###
    output$change_plot <- renderPlotly({
      
      occ.plot.dat <- map.data()$mapdat.out
      num.plot.dat <- map.data()$num.out
      
      # Setting up x and y-axis labels.
      tdc <- "Temperature, C"
      prcp <- "Precipitation, mm"
      delt_tdc <- "Change in temperature, C"
      delt_prcp <- "Change in precipitation, mm"
      
      change.x.lab <- switch(as.numeric(input$metric), tdc , tdc, delt_tdc, tdc , tdc, delt_tdc)
      change.y.lab <- switch(as.numeric(input$metric), prcp, prcp, delt_prcp, prcp, prcp, delt_prcp)
      
      
      if (input$sppname == "1") {  # "all spp" is selected.
        # Grabbing temperature and precipitation columns, but duplicating them so that if pre.temp is selected, pre.precip will be selected as well. Etc.
        change.x <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 1, 2, 6, 1, 2, 6)]) 
        change.y <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 3, 4, 7, 3, 4, 7)])
        
        # There are so many points that I've gone with geom_hex to summarize the density of points by small area. 
        cplot <- ggplot(occ.plot.dat, aes(get(change.x), get(change.y))) + 
          geom_hex(bins = 100) +
          scale_fill_viridis(option = "H") + 
          labs(x = change.x.lab, y = change.y.lab) +
          theme_bw()
        
        
      } else {

        change.x <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 4, 5, 8, 4, 5, 8)]) # Temperature columns
        change.y <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 6, 7, 9, 6, 7, 9)]) # Precipitation columns
        
        if (input$occ_num == "1") {  # If occupancy, setting a column of labels for occupancy status, plus defining the plot data and legend name.
          occ.plot.dat$color.code <- ifelse(occ.plot.dat$orig == 1 & occ.plot.dat$revis == 0, "Extirpated", 
                                            ifelse(occ.plot.dat$orig == 1 & occ.plot.dat$revis == 1, "Occupied", "Colonized"))
          
          cplot.dat <- occ.plot.dat  
          lab.name <- "Occupancy Status"
        } else {  # If change in density, doing the same for that data frame.
          
          num.plot.dat$color.code <- ifelse(num.plot.dat$gt == 1 & num.plot.dat$lt == 0, "More trees", "Fewer Trees")
          
          cplot.dat <- num.plot.dat  
          lab.name <- "Density Change\nDirection"
        }
         # Plot for occupancy/density change for single species.
        cplot <-  ggplot(cplot.dat, aes(get(change.x), get(change.y), color = as.factor(color.code))) + 
          geom_point() + 
          scale_color_viridis(discrete = TRUE,  name = lab.name) + 
          labs(x = change.x.lab, y = change.y.lab) +
          theme_bw()
      }
      
      ggplotly(cplot) 
      
    })
    
    
    ### Change vs. Metric plot ### 
    # This plot is intended to help viewers see how the change in a metric plays out across all values of that metric for the first or second visit.
    output$abs_change_plot <- renderPlotly({
      plot3.occ.dat <- map.data()$mapdat.out
      plot3.num.dat <- map.data()$num.out
      
      metric.num <- as.numeric(input$metric)  # Numbers 1:6.  I simplify it here because I use this value a lot in this section.
      
      p3.type <- switch(metric.num,
                        "precip", "precip", "precip", "temp", "temp", "temp")  # type = either precip or temp.
      p3.timing <- c("pre.", "post.", "pre.", "pre.", "post.", "pre.")  # For the input$metric, people select "first visit", "second visit" or "change".  In this case I set change = first visit.
      #browser()
      p3.x <- colnames(plot3.occ.dat[switch(metric.num, 1, 2, 1, 3, 4, 3)]) 
      labs.x.p3 <- c("Precipitation, mm, First Visit", "Precipitation, mm, Second Visit", "Precipitation, mm, First Visit", # Set x labels
                     "Temperature, C,  First Visit", "Temperature, C,  Second Visit", "Temperature, C,  First Visit")
      
      labs.y.p3 <- if (p3.type == "precip") "Precipitation Change, mm" else "Temperature Change, C"  # Set y labels

      loess.ci <- if (input$ci == "1") TRUE else FALSE  # Do we want the smoother to produce a confidence interval? From radio button.
      
      if (input$sppname == "1") {  # if "all species", produce another hexagon summary of data points.  Not attempting to fit smoothing lines.
        
        plot3.dat2 <- plot3.occ.dat %>% dplyr::select(contains(p3.type)) 
        
        abs.ch <- ggplot(plot3.dat2, aes(get(paste0(p3.timing[metric.num], p3.type)), get(paste0("change.", p3.type)))) + 
          geom_hex(bins = if (p3.type == "precip") 200 else 100) +   # The precip values extend farther, so I want more hexagons.
          geom_hline(yintercept = 0) +
          scale_fill_viridis_c() + 
          labs(x = labs.x.p3[metric.num], y = labs.y.p3) +
          theme_bw()
        
      } else {

        if (input$occ_num == "1") {   # "1"  = occupancy
          plot3.occ.dat$color.code <- ifelse(plot3.occ.dat$orig == 1 & plot3.occ.dat$revis == 0, "Extirpated", 
                                             ifelse(plot3.occ.dat$orig == 1 & plot3.occ.dat$revis == 1, "Occupied", "Colonized"))
          
          # Occupancy plot of metric (x) vs. change in the metric (y)
          abs.ch <- ggplot(plot3.occ.dat, aes(get(paste0(p3.timing[metric.num], p3.type)), get(paste0("change.", p3.type)), 
                                              color = as.factor(color.code))) + 
            geom_point() + 
            geom_hline(yintercept = 0) +
            geom_smooth(se = loess.ci, linetype = "dashed", size = 0.5, span = 1) +  # Will do a loess or gam smoother.
            scale_color_viridis(discrete = TRUE,  name = "Occupancy Status") + 
            labs(x = labs.x.p3[metric.num], y = labs.y.p3) +
            theme_bw()
          
        } else { # density change
          
          plot3.num.dat$color.code <- ifelse(plot3.num.dat$gt == 1 & plot3.num.dat$lt == 0, "More trees", "Fewer Trees")
          
          #browser()
          
          abs.ch <- ggplot(plot3.num.dat, aes(get(paste0(p3.timing[metric.num], p3.type)), get(paste0("change.", p3.type)), 
                                              color = as.factor(color.code))) + 
            geom_point() + 
            geom_hline(yintercept = 0) +
            geom_smooth(se = loess.ci, linetype = "dashed", size = 0.5, span = 1) +
            scale_color_viridis(discrete = TRUE,  name = "Density Change\nDirection") +  
            labs(x = labs.x.p3[metric.num], y = labs.y.p3) +
            theme_bw()
          
        }
        
        
      }
      
      ggplotly(abs.ch)
      
    })      
    
    output$hist_plot <- renderPlot({
      hist.occ.dat <- map.data()$mapdat.out
      hist.num.dat <- map.data()$num.out
      
      
      metric.num <- as.numeric(input$metric)
      
hist.type <- switch(metric.num,
                        "precip", "precip", "precip", "temp", "temp", "temp")
labs.x.hist <- if (hist.type == "precip") "Precipitation Change, mm" else "Temperature Change, C"

            
      if (input$sppname == "1") {
        
        hist.plt <- ggplot(hist.occ.dat, aes(get(paste0("change.", hist.type)))) +
          geom_histogram(bins = 200) +
          labs(x = labs.x.hist) +
          theme_bw()
        
      } else {
        if (input$occ_num == "1") {   # "1"  = occupancy
          hist.occ.dat$color.code <- ifelse(hist.occ.dat$orig == 1 & hist.occ.dat$revis == 0, "Extirpated", 
                                             ifelse(hist.occ.dat$orig == 1 & hist.occ.dat$revis == 1, "Occupied", "Colonized"))
          hist.dat <- hist.occ.dat
          hist.title <- "Occupancy Status"
          } else {

          hist.num.dat$color.code <- ifelse(hist.num.dat$gt == 1 & hist.num.dat$lt == 0, "More trees", "Fewer Trees")
          hist.dat <- hist.num.dat
          hist.title <- "Density Change Direction"
          }
        #browser()
          hist.plt <- ggplot(hist.dat, aes(x = get(paste0("change.", hist.type)), y = color.code, fill = color.code)) +
            geom_density_ridges2( quantile_lines = T, quantile_fun = mean, alpha = 0.75, scale = 1.2) + 
            scale_fill_viridis(discrete = TRUE) +  
            labs(y = NULL, x = labs.x.hist, title = hist.title) +
            theme_ridges() +
            theme(legend.position = "none",
                  text = element_text(size = 20))
          
      }
      
      hist.plt
    
    })
    
    
  })
  
  
}

