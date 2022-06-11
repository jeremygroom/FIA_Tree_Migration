



Map1_UI <- function(id) {
  tagList(
    useShinyjs(),
    fluidRow(column(width = 10, offset = 2, h1("Map and plots of temperature, precipitation, and species' ranges"))),
    fluidRow(box(width = 6, offset = 3,
                 column(width = 6, selectInput(NS(id, "metric"), label = h3("Select metric"),
                                               choices = list("Precipitation, First 10 years" = 1, 
                                                              "Precipitation, Second 10 years" = 2,
                                                              "Change in Precipitation" = 3,
                                                              "Temperature, First 10 years" = 4,
                                                              "Temperature, Second 10 years" = 5,
                                                              "Change in Temperature" = 6))),
                 column(width = 6, selectInput(NS(id, "sppname"), label = h3("Select species data to display"),
                                               choices = map.choices)))),
    
    
    
    # Tables UI
    fluidRow(box(width = 4, offset = 1,
                 radioButtons(NS(id, "map_scale"), label = h4("Extent of map"),
                              choices = list(
                                "Full map extent" = 1,
                                "Species range extent" = 2)),   
                 fluidRow(width = 11, plotOutput(NS(id, "plot_map1"), height = 800))),
             box(width = 6,
                 radioButtons(NS(id, "occ_num"), label = h4("Occupancy or density results"),
                              choices = list(
                                "Occupancy" = 1,
                                "Change in density" = 2)),
                 fluidRow(plotlyOutput(NS(id, "change_plot"), height = 600, width = 600))))
  )
}



Map1_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    
    observe({    
      if (as.numeric(input$sppname) %in% spp.absent.gt$id) {
        updateRadioButtons(session, inputId = "occ_num", selected = 1)
      }
    })
    
    observe({  
      toggleState("occ_num", condition = (as.numeric(input$sppname) %in% spp.absent.gt$id == FALSE))
    })     
    
    
    map.data <- reactive({
      
      TC.col.use.index <- switch(as.numeric(input$metric), 3, 4, 7, 1, 2, 6)
      mapvar <- colnames(TC1.2.[TC.col.use.index]) 
      
      if (input$sppname == 1) {
        mapdat.out <- TC1.2.
        num.dat2 <- NULL
      } else {
        
        spp.code <- spp.names2$spp.codes[spp.names2$id == as.numeric(input$sppname)]
        
        spp.dat1 <- occ_orig %>% filter(State_Plot != 0) %>% dplyr::select(State_Plot, paste0("X", spp.code)) %>%
          left_join(occ_revis[occ_revis$State_Plot != 0, c(1, which(names(occ_revis) == paste0("X", spp.code)))], by = "State_Plot") 
        names(spp.dat1)[2:3] <- c("orig", "revis")
        spp.dat2 <- spp.dat1 %>% filter(orig > 0 | revis > 0) %>% 
          left_join(TC1.2., by = "State_Plot")
        
        if (paste0("X", spp.code) %in% colnames(num_gt)) {
          num.dat1 <- num_gt %>% filter(State_Plot != 0) %>% dplyr::select(State_Plot, paste0("X", spp.code)) %>%
            left_join(num_lt[num_lt$State_Plot != 0, c(1, which(names(num_lt) == paste0("X", spp.code)))], by = "State_Plot") 
          names(num.dat1)[2:3] <- c("gt", "lt")
          num.dat2 <- num.dat1 %>% filter(gt > 0 | lt > 0) %>% 
            left_join(TC1.2., by = "State_Plot")
        } else {
          num.dat2 <- NULL
        } 
        
        mapdat.out <- spp.dat2
        num.out <- num.dat2
      }
      
      return(list(mapvar = mapvar, mapdat.out = mapdat.out, num.out = num.dat2))
    })
    
    output$plot_map1 <- renderPlot({
      #browser()
      map.plot.dat <- map.data()$mapdat.out
      map.var <- map.data()$mapvar
      
      if (input$map_scale == "1") {
        maxlat <- max(TC1.2.$LAT); minlat <- min(TC1.2.$LAT)
        maxlong <- max(TC1.2.$LON); minlong <- min(TC1.2.$LON)
      } else {
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
    
    output$change_plot <- renderPlotly({
      
      occ.plot.dat <- map.data()$mapdat.out
      #     if (input$sppname != "1")      browser()
      num.plot.dat <- map.data()$num.out
      
      
      tdc <- "Temperature, C"
      prcp <- "Precipitation, mm"
      delt_tdc <- "Change in temperature, C"
      delt_prcp <- "Change in precipitation, mm"
      
      change.x.lab <- switch(as.numeric(input$metric), tdc , tdc, delt_tdc, tdc , tdc, delt_tdc)
      change.y.lab <- switch(as.numeric(input$metric), prcp, prcp, delt_prcp, prcp, prcp, delt_prcp)
      
      
      if (input$sppname == "1") {
        
        change.x <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 1, 2, 6, 1, 2, 6)]) 
        change.y <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 3, 4, 7, 3, 4, 7)])
        
        
        cplot <- ggplot(occ.plot.dat, aes(get(change.x), get(change.y))) + 
          geom_hex(bins = 100) +
          scale_fill_viridis_c() + 
          labs(x = change.x.lab, y = change.y.lab) +
          theme_bw()
        
        
      } else {
        
        change.x <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 4, 5, 8, 4, 5, 8)]) 
        change.y <- colnames(occ.plot.dat[switch(as.numeric(input$metric), 6, 7, 9, 6, 7, 9)])
        
        if (input$occ_num == "1") {
          occ.plot.dat$color.code <- ifelse(occ.plot.dat$orig == 1 & occ.plot.dat$revis == 0, "Extirpated", 
                                            ifelse(occ.plot.dat$orig == 1 & occ.plot.dat$revis == 1, "Occupied", "Colonized"))
          
          cplot.dat <- occ.plot.dat  
          lab.name <- "Occupancy Status"
        } else {
          
          num.plot.dat$color.code <- ifelse(num.plot.dat$gt == 1 & num.plot.dat$lt == 0, "More treees", "Fewer Trees")
          
          cplot.dat <- num.plot.dat  
          lab.name <- "Second Visit Change\nin Tree Number"
        }
        
        cplot <-  ggplot(cplot.dat, aes(get(change.x), get(change.y), color = as.factor(color.code))) + 
          geom_point() + 
          scale_color_viridis(discrete = TRUE,  name = lab.name) + 
          labs(x = change.x.lab, y = change.y.lab) +
          theme_bw()
      }
      
      ggplotly(cplot) 
    })
    
    
  })
}
