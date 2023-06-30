



Map1_UI <- function(id) {
  tagList(
    useShinyjs(),                 # allows toggleState function for Plotly
    fluidRow(column(width = 10, offset = 1, align = "center", h1("Map and plots of temperature, precipitation, and species' ranges"))),
    fluidRow(column(width = 8, offset = 2, align = "center",
                    box(width = 12,
                        p("These figures all display PRISM temperature and precipitation information associated with FIA plots.  Note that under \" Select Metric\" users can choose a change option
                          for precipitation and temperature.  This is the difference in plots' metrics, the second visit - the first visit.  
                          Under \"Select species data to display\" the default option is \"All\".  This provides information on every FIA plot in Oregon,
                          Washington, and California (note that most FIA plots are not visited because they do not contain forest).", style = "font-size:20px"),
                        p("In the box to the right of the map are three other figures.  All three respond to the metric and species selected, plus the \"Range 
                        or density shift results\" selection.  Once you select a species, note that by clicking on the legend you can turn the display of different categories
                          on or off.  The first (top) plot displays a first or second visit metric on the X axis and the corresponding change metric on the Y axis. 
                          It also includes an option to show the best spatially-balanced linear fit of the data for each legend option.
                          The purpose of this figure is to evaluate the stability of a metric's change across the X axis values.
                          The middle plot shows the distribution of precipitation and temperature data.
                          The purpose of the histogram at the bottom is to display the (non-weighted) mean and density of data from plot categories.", style = "font-size:20px")))),
    
    
    fluidRow(column(width = 10, offset = 1, align = "center", 
                    box(width = 12,
                        column(width = 4, selectInput(NS(id, "metric"), label = h3("Select metric"),
                                                      choices = list("Precipitation, First visit" = 1, 
                                                                     "Precipitation, Second visit" = 2,
                                                                     "Change in Precipitation" = 3,
                                                                     "Temperature, First visit" = 4,
                                                                     "Temperature, Second visit" = 5,
                                                                     "Change in Temperature" = 6,
                                                                     "Maximum VPD, First visit" = 7,
                                                                     "Maximum VPD, Second visit" = 8,
                                                                     "Change in Maximum VPD" = 9, 
                                                                     "Minimum VPD, First visit" = 10,
                                                                     "Minimum VPD, Second visit" = 11,
                                                                     "Change in Minimum VPD" = 12 
                                                      ))),
                        column(width = 4, selectInput(NS(id, "sppname"), label = h3("Select species data to display"),
                                                      choices = map.choices)),
                        column(width = 4, selectInput(NS(id, "occ_num"), label = h3("Range or density shift results"),
                                                      choices = list(
                                                        "Range shift" = 1,
                                                        "Density shift" = 2))
                        )))
    ),   # "map.choices" is a long list (see global.r)
    
    
    
    fluidRow(column(width = 4, offset = 1, 
                    box(width = 12, 
                        radioButtons(NS(id, "map_scale"), label = h4("Extent of map"),
                                     choices = list(
                                       "Full map extent" = 1,
                                       "Species range extent" = 2)),   
                        fluidRow(column(width = 11, offset = 1, plotOutput(NS(id, "plot_map1"), height = 800, width = 400))))
    ), 
    
    column(width = 7, 
           box(width = 12, 
               fluidRow(column(width = 10, h3("How consistent was the metric change across the first or second visit values?"))),
               fluidRow(column(width = 8, radioButtons(NS(id, "line"), label = h4("Show spatial linear regression line?"),
                                                       choices = list(
                                                         "Yes" = 1,
                                                         "No" = 2)))), 
               fluidRow(column(width = 10, plotlyOutput(NS(id, "abs_change_plot"), height = 400, width = 600))),
               
               fluidRow(column(width = 10, h3("Data Distribution"))),
               fluidRow(column(width = 8, plotlyOutput(NS(id, "change_plot"), height = 400, width = 600)),
                        column(width = 4, selectInput(NS(id, "xChoice"), label = h3("Select X axis"),
                                                      choices = list("Precipitation, First visit" = 1, 
                                                                     "Precipitation, Second visit" = 2,
                                                                     "Change in Precipitation" = 3,
                                                                     "Temperature, First visit" = 4,
                                                                     "Temperature, Second visit" = 5,
                                                                     "Change in Temperature" = 6,
                                                                     "Maximum VPD, First visit" = 7,
                                                                     "Maximum VPD, Second visit" = 8,
                                                                     "Change in Maximum VPD" = 9, 
                                                                     "Minimum VPD, First visit" = 10,
                                                                     "Minimum VPD, Second visit" = 11,
                                                                     "Change in Minimum VPD" = 12
                                                      ), selected = 4))),
               fluidRow(column(width = 10, h3("Density plots of changes in metrics with means"))),
               fluidRow(column(width = 10, plotOutput(NS(id, "hist_plot"), height = 600, width = 600)))
           )
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
      TC.col.use.index <- switch(as.numeric(input$metric), 1, 2, 11, 3, 4, 10, 5, 6, 12, 7, 8, 13)
      mapvar <- colnames(TC1.2.[TC.col.use.index]) 
      
      if (input$sppname == 1) {  # If the "all species" option is selected, the occupancy/map data are selected and the density (number) data are set to NULL.
        mapdat.out <- TC1.2.     # Toggling between the two options will not change the plots for "all species"
        num.dat2 <- NULL
        
      } else if (input$sppname == 2) {  #or, if all plots forested by our examined species was selected...
        
        all.occ.dat <- as.matrix(occ_orig[, 3:ncol(occ_orig)]) + as.matrix(occ_revis[, 3:ncol(occ_revis)])
        all.occ.dat <- cbind(occ_orig[, 1:2], as.data.frame(all.occ.dat)) 
        all.occ.dat$any <- apply(all.occ.dat[, grep("X", names(all.occ.dat))], 1, sum)
        mapdat.out <- all.occ.dat %>% filter(any > 1) %>% 
          dplyr::select(State_Plot) %>% 
          left_join(TC1.2., by = "State_Plot") 
        mapdat.out <- mapdat.out[, c(2:5, 1, 6:ncol(mapdat.out))]  # Needs to match column order of TC1.2.
        num.dat2 <- NULL
        # browser()
      } else {                   # Else, a single species was selected.
        
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
      map.leg <- switch(as.numeric(input$metric),
             "Precipitation (mm),\nFirst visit", 
             "Precipitation (mm),\nSecond visit",
             "Change in\nPrecipitation (mm)",
             "Temperature (°C),\nFirst visit",
             "Temperature (°C),\nSecond visit",
             "Change in\nTemperature (°C)",
             "Maximum VPD (hPa),\nFirst visit",
             "Maximum VPD (hPa),\nSecond visit",
             "Change in\nMaximum VPD (hPa)",
             "Minimum VPD (hPa),\nFirst visit",
             "Minimum VPD (hPa),\nSecond visit",
             "Change in\nMinimum VPD (hPa)"
      )
      
      ggplot() +
        coord_fixed(xlim = c(minlong - 1, maxlong + 1),  ylim = c(minlat - 1, maxlat + 1), ratio = 1.3) +
        geom_tile(data = map.plot.dat, mapping = aes(x = LON, y = LAT, z = get(map.var)), binwidth = 0.15, stat = "summary_2d", fun = mean, na.rm = TRUE) + 
        scale_fill_viridis(option = "H", name = map.leg) +
        geom_polygon(data = west_df, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "transparent") +
        theme_void() +
        theme(legend.position = c(0.85, 0.45))

      #+
      #geom_polygon(data = west_county, aes(x = long, y = lat, group = group), fill = NA, color = "white") 
    })
    
    observe({
      updateSelectInput(session, NS(id, "xChoice"),
                        selected = (as.numeric(input$metric) + 3) %% 12)  # Selecting a default x axis that is advanced 3 positions from 
                                                                          #  selected y axis variable, and the value wraps after position 12.
    })
    ### Plot of temperature and precipitation change  ###
    output$change_plot <- renderPlotly({
      
      occ.plot.dat <- map.data()$mapdat.out
      num.plot.dat <- map.data()$num.out
      
      if (ncol(occ.plot.dat) == 17) occ.plot.dat <- occ.plot.dat %>% relocate(orig, revis, .after = 17)
      occ.plot.dat <- occ.plot.dat %>% relocate("State_Plot", .before = 1)
      
      # Setting up x and y-axis labels.
      prcp1 <- "Precipitation, 1st visit, mm"
      prcp2 <- "Precipitation, 2nd visit, mm"
      delt_prcp <- "Change in precipitation, mm"
      tdc1 <- "Temperature, 1st visit, C"
      tdc2 <- "Temperature, 2nd visit, C"
      delt_tdc <- "Change in temperature, C"
      vMax1 <- "Maximum VPD, 1st visit, hPa"
      vMax2 <- "Maximum VPD, 2nd visit, hPa"
      delt_vMax <- "Change in Maximum VPD, hPa"
      vMin1 <- "Minimum VPD, 1st visit, hPa"
      vMin2 <- "Minimum VPD, 2nd visit, hPa"
      delt_vMin <- "Change in Minimum VPD, hPa"
      
      lab_order <- c(prcp1, prcp2, delt_prcp, tdc1, tdc2, delt_tdc, vMax1, vMax2, delt_vMax, vMin1, vMin2, delt_vMin)
      
    #if (input$sppname == 3) browser()

      change.x.lab <- lab_order[as.numeric(input$xChoice)]
      change.y.lab <- lab_order[as.numeric(input$metric)]

      if (input$sppname == "1" | input$sppname == "2" ) {  # "all spp" is selected.
        # Grabbing temperature and precipitation columns, but duplicating them so that if pre.temp is selected, pre.precip will be selected as well. Etc.
        
        
        
        change.x <- colnames(occ.plot.dat[switch(as.numeric(input$xChoice), 2, 3, 11, 4, 5, 10, 6, 7, 12, 8, 9, 13)])
        change.y <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 2, 3, 11, 4, 5, 10, 6, 7, 12, 8, 9, 13)])
        
        # There are so many points that I've gone with geom_hex to summarize the density of points by small area. 
        cplot <- ggplot(occ.plot.dat, aes(get(change.x), get(change.y))) + 
          geom_hex(bins = 100) +
          scale_fill_viridis(option = "H") + 
          labs(x = change.x.lab, y = change.y.lab) +
          theme_bw()
        
        
      } else {
        change.x <- colnames(occ.plot.dat[switch(as.numeric(input$xChoice), 2, 3, 11, 4, 5, 10, 6, 7, 12, 8, 9, 13)]) 
        change.y <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 2, 3, 11, 4, 5, 10, 6, 7, 12, 8, 9, 13)])
        
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
      metric.num <- as.numeric(input$metric)  # Numbers 1:12.  I simplify it here because I use this value a lot in this section.
       
      p3.type <- switch(metric.num,
                        "precip", "precip", "precip", "temp", "temp", "temp",
                        "vpdmax", "vpdmax", "vpdmax", "vpdmin", "vpdmin", "vpdmin")  # type = either precip or temp.
      
      
      plot3.occ.dat <- plot3.occ.dat %>% relocate("State_Plot", .before = 1)
      if (ncol(plot3.occ.dat) == 17) plot3.occ.dat <- plot3.occ.dat %>% relocate(orig, revis, .after = 17)
      
      #if(input$sppname == 26) browser()

      p3.timing <- rep(c("pre.", "post.", "pre."), 4 ) # For the input$metric, people select "first visit", "second visit" or "change".  In this case I set change = first visit.
      #browser()
      p3.x <- colnames(plot3.occ.dat[switch(metric.num, 2, 3, 2, 4, 5, 4, 6, 7, 6, 8, 9, 8)]) 
      fvsvfv <- c(", First Visit", ", Second Visit", ", First Visit")
      labs.x.p3 <- c(paste0("Precipitation, mm", fvsvfv), paste0("Temperature, C", fvsvfv),
                     paste0("Maximum VPD, hPa", fvsvfv), paste0("Minimum VPD, hPa", fvsvfv))
      
      labs.y.p3 <- switch(p3.type,                                            # Set y labels
                          "precip" = "Precipitation Change, mm",
                          "temp" = "Temperature Change, C",
                          "vpdmax" = "Maximum VPD Change, hPa",
                          "vpdmin" = "Minimum VPD Change, hPa")
        
      slm.line <- if (input$line == "1") TRUE else FALSE  # Do we want to show the spatial linear regression line?

      if (input$sppname == "1") {  # if "all species", produce another hexagon summary of data points.  Not attempting to fit smoothing lines.
        
        plot3.dat2 <- plot3.occ.dat %>% dplyr::select(contains(p3.type)) 

        abs.ch <- ggplot(plot3.dat2, aes(get(paste0(p3.timing[metric.num], p3.type)), get(paste0("change.", p3.type)))) + 
          geom_hex(bins = if (p3.type == "precip") 200 else 100) +   # The precip values extend farther, so I want more hexagons.
          geom_hline(yintercept = 0) +
          scale_fill_viridis_c() + 
          labs(x = labs.x.p3[metric.num], y = labs.y.p3) +
          theme_bw()
        
        ggp.abs.ch <- ggplotly(abs.ch, tooltip = "text") 
        
                
      } else if (input$sppname == "2") {
        
        # Spatial regression results files (for map page)
        
      ch.title.word <- switch(input$metric,
                              "1" = "precipitation", "2" = "precipitation", "3" = "precipitation",
                              "4" = "temperature", "5" = "temperature", "6" = "temperature", 
                              "7" = "maximum VPD", "8" = "maximum VPD", "9" = "maximum VPD",
                              "10" = "minimum VPD", "11" = "minimum VPD", "12" = "minimum VPD")  
        
      ch.met.symb <- switch(input$metric,
                            "1" = "mm", "2" = "mm", "3" = "mm", "4" = "C", "5" = "C", "6" = "C", 
                            "7" = "hPa", "8" = "hPa", "9" = "hPa", "10" = "hPa", "11" = "hPa", "12" = "hPa")  

        ch.visit <- switch(as.numeric(input$metric),
                           "first", "second", "first", "first", "second", "first",
                           "first", "second", "first", "first", "second", "first")
        
        ch.title <- paste(paste0("Slopes of occupied plot ", ch.title.word, " change across ", ch.visit, " visit"), 
                          "values. Mouse over endpoints to ID species.", sep = "<br>")
        
        ch.xlab <- paste0("Range of plot ", ch.title.word, " values, ", ch.visit, " visit, ", ch.met.symb) 
        ch.ylab <- paste0("Change in plot ", ch.title.word, " values, ", ch.met.symb)
        
        dat.to.pto <- switch(as.numeric(input$metric), 
                             ch.PrecipV1, ch.PrecipV2, ch.PrecipV1, ch.TempV1, ch.TempV2, ch.TempV1,
                             ch.VPDmaxV1, ch.VPDmaxV2, ch.VPDmaxV1, ch.VPDminV1, ch.VPDminV2, ch.VPDminV1) %>%
          filter(spp != "X768")

        pto.dat <- pto.fcn(dat.to.pto) %>% mutate(spp = as.numeric(substr(spp, 2, nchar(spp)))) %>%
          left_join(spp.names2 %>% dplyr::select(spp.codes, sel.names), by = c("spp" = "spp.codes"))

        abs.ch <- ggplot(data = pto.dat, aes(x_value, pred_value, color = factor(spp), text = paste0(sel.names))) + 
          geom_line() + 
          geom_hline(yintercept = 0) + 
          scale_color_viridis(discrete = TRUE, option = "H") + 
          labs(title = ch.title, x = ch.xlab, y = ch.ylab) +
          #theme_bw() + 
          theme(legend.position = "none",
                plot.title = element_text(hjust = 1))
        
        ggp.abs.ch <- ggplotly(abs.ch, tooltip = "text") %>% layout(margin = list(t = 75))
        
        
      } else {
        
        spp.line <- switch(as.numeric(input$metric), 
                           ch.PrecipV1, ch.PrecipV2, ch.PrecipV1, ch.TempV1, ch.TempV2, ch.TempV1,
                           ch.VPDmaxV1, ch.VPDmaxV2, ch.VPDmaxV1, ch.VPDminV1, ch.VPDminV2, ch.VPDminV1) %>% 
          filter(spp != "X768", spp == paste0("X", spp.names2$spp.codes[which(spp.names2$id == as.numeric(input$sppname)) ]))
#browser()
        spp.line <- spp.line %>% mutate(pred1 = if (is.na(slp.coef)) int.coef else {int.coef + slp.coef * (x.min - center.adj)}, # for some odd reason, the code to create pred1 & 2 wasn't working, so recreated the values here.
                                      pred2 = if (is.na(slp.coef)) int.coef else {int.coef + slp.coef * (x.max - center.adj)})
        
        
        txt.coef <- paste0("Int = ", round(spp.line$int.coef, 2), sig.astx.fcn(spp.line, "int.p"), "\n",
                           "Slope = ", round(spp.line$slp.coef, 2), sig.astx.fcn(spp.line, "slp.p"))
        
        
        if (input$occ_num == "1") {   # "1"  = occupancy
          
          # Occupancy data
          plot3.occ.dat$color.code <- ifelse(plot3.occ.dat$orig == 1 & plot3.occ.dat$revis == 0, "Extirpated", 
                                             ifelse(plot3.occ.dat$orig == 1 & plot3.occ.dat$revis == 1, "Occupied", "Colonized"))
          
          # Occupancy plot of metric (x) vs. change in the metric (y)
          abs.ch <- ggplot(plot3.occ.dat, aes(get(paste0(p3.timing[metric.num], p3.type)), get(paste0("change.", p3.type)), 
                                              color = as.factor(color.code))) + 
            geom_point() + 
            geom_hline(yintercept = 0) +
            scale_color_viridis(discrete = TRUE,  name = "Occupancy Status") + 
            labs(x = labs.x.p3[metric.num], y = labs.y.p3) +
            theme_bw()
#          browser()
          if (input$line == "1") abs.ch <- abs.ch + 
            geom_segment(aes(x = spp.line$x.min, xend = spp.line$x.max, y = spp.line$pred1, yend = spp.line$pred2), linewidth = 1, color = "grey") +
            annotate("text", x = 0.9*spp.line$x.max, y = 0.9 * max(get(paste0("change.", p3.type), plot3.occ.dat)), label = txt.coef, size = 3)

          ggp.abs.ch <- ggplotly(abs.ch, tooltip = "text")
          
            
        } else { # density change
          
          plot3.num.dat$color.code <- ifelse(plot3.num.dat$gt == 1 & plot3.num.dat$lt == 0, "More trees", "Fewer Trees")
          

          abs.ch <- ggplot(plot3.num.dat, aes(get(paste0(p3.timing[metric.num], p3.type)), get(paste0("change.", p3.type)), 
                                              color = as.factor(color.code))) + 
            geom_point() + 
            geom_hline(yintercept = 0) +
            scale_color_viridis(discrete = TRUE,  name = "Density Change\nDirection") +  
            labs(x = labs.x.p3[metric.num], y = labs.y.p3) +
            theme_bw()
          
          if (input$line == "1") abs.ch <- abs.ch + 
            geom_segment(aes(x = spp.line$x.min, xend = spp.line$x.max, y = spp.line$pred1, yend = spp.line$pred2), linewidth = 1, color = "grey") +
            annotate("text", x = 0.9*spp.line$x.max, y = 0.9 * max(get(paste0("change.", p3.type), plot3.occ.dat)), label = txt.coef, size = 3)  
          
          ggp.abs.ch <- ggplotly(abs.ch, tooltip = "text")
            
        }
        
        
      }
      
      ggp.abs.ch

    })      
    
    output$hist_plot <- renderPlot({
      hist.occ.dat <- map.data()$mapdat.out
      hist.num.dat <- map.data()$num.out
      
      
      metric.num <- as.numeric(input$metric)
      
      hist.type <- switch(metric.num,
                          "precip", "precip", "precip", "temp",  "temp", "temp",
                          "vpdmax", "vpdmax", "vpdmax", "vpdmin", "vpdmin", "vpdmin")
      labs.x.hist <- switch(hist.type,                                            # Set y labels
                            "precip" = "Precipitation Change, mm", "temp" = "Temperature Change, C",
                            "vpdmax" = "Change in Maximum VPD, hPa", "vpdmin" = "Change in Minimum VPD, hPa")
      
#      if(metric.num == 7) browser()
      if (input$sppname == "1" | input$sppname == "2") {
        
        hist.plt <- ggplot(hist.occ.dat, aes(get(paste0("change.", hist.type)))) +
          geom_histogram(bins = 200) +
          labs(x = labs.x.hist, y = "Count") +
          theme_bw() + 
          theme(text = element_text(size = 15))
        
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

