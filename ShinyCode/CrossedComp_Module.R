### Crossed comparison of species change estimates ###
###    --- Second Page ---       ###



CrossedComp_UI <- function(id) {
  tagList(
    fluidRow(box(width = 12,
                 #status = "success",   # makes the top of the box green.  
                 column(10, offset = 1, align = "center", h1(id = "hp-title", "Crossed Comparison of Species Change Estimates"),
                        tags$style(HTML("#hp-title{color: #154360;
                             font-size: 40px;
                                 font-style: italic;}"))))),
    fluidRow(column(width = 8, offset = 2, align = "center",
                    div(htmlOutput("box10"),  style = 'color:black; border-radius: .5em; font-size:22px; background-color:#ccffe6; padding: 20px; margin: 20px;' ,
                        p("This plot displays the means and confidence intervals plotted in the previous page, except along the X and Y axes. 
                          This enables users to compare, say, species' density changes by plot temperature or precipitation values.  The plot
                          can show all species (overwhelming), paired species (those that appear in the occupancy and density analyses), 
                          significant species (at least one confidence interval for a species does not include zero), or paired and significant species.
                          Scroll down to select analysis variables of interest.", style = "font-size:20px")))),
    
    fluidRow(column(width = 8, plotlyOutput(NS(id, "plot_crossedcomp"), height = 700)),
             column(width = 4,
                    box(width = 12, radioButtons(NS(id, "results.show"), label = h2("Results to Show"),
                                            choices = list(
                                              "All Species" = 1,
                                              "Paired Species" = 2,
                                              "Significant Species" = 3,
                                              "Paired Significant Species" = 4),
                                            selected = 3)))),
    fluidRow(box(width = 8, 
                 column(width = 4, offset = 1,
                    fluidRow(width = 12, h2("Y-axis Options")),
                    fluidRow(column(width = 12,  
                                    selectInput(NS(id, "occ.num1"), label = h5("Select analysis type"),
                                                choices = list("Range shift" = 1, 
                                                               "Density shift" = 2)))),
                    fluidRow(width = 12, selectInput(NS(id, "est.bs1"), label = h5("Estimate or bootstrap"),
                                                     choices = list("TSE Estimate" = 1, 
                                                                    "Bootstrap" = 2),
                                                     selected = 1)),
                    fluidRow(width = 12, selectInput(NS(id, "metric1"), label = h5("Select analysis metric"),
                                                     choices = list("Precipitation" = 1, 
                                                                    "Temperature" = 2,
                                                               "Maximum Vapor Pressure Deficit" = 3,
                                                               "Minimum Vapor Pressure Deficit" = 4))),
                    fluidRow(width = 12, selectInput(NS(id, "time1"), label = h5("Select time perspective"),
                                                     choices = list("First visit" = 1,
                                                                    "Second visit" = 2)))),
             
             column(width = 4, offset = 1,
                    fluidRow(width = 12, h2("X-axis Options")),
                    fluidRow(column(width = 12,  
                                    selectInput(NS(id, "occ.num2"), label = h5("Select analysis type"),
                                                choices = list("Range shift" = 1, 
                                                               "Density shift" = 2)))),
                    fluidRow(width = 12, selectInput(NS(id, "est.bs2"), label = h5("Estimate or bootstrap"),
                                                     choices = list("TSE Estimate" = 1, 
                                                                    "Bootstrap" = 2),
                                                     selected = 2)),
                    fluidRow(width = 12, selectInput(NS(id, "metric2"), label = h5("Select analysis metric"),
                                                     choices = list("Precipitation" = 1, 
                                                                    "Temperature" = 2,
                                                               "Maximum Vapor Pressure Deficit" = 3,
                                                               "Minimum Vapor Pressure Deficit" = 4))),
                    fluidRow(width = 12, selectInput(NS(id, "time2"), label = h5("Select time perspective"),
                                                     choices = list("First visit" = 1,
                                                                    "Second  visit" = 2))))
    )
    )
  )
}




CrossedComp_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    
    
    
    
    output$plot_crossedcomp <- renderPlotly({
      
      o.n.sel1 <- o.n.[as.numeric(input$occ.num1)]  # Occupancy/Number, right column
      p.t.v.sel1 <- p.t.v.[as.numeric(input$metric1)]   # Precip/Temp/VPDmax/VPDmin, left column
      e.bs.sel1 <- e.bs[as.numeric(input$est.bs1)]  # Est/ Bootstrap est, left column 
      t.sel1 <- as.numeric(input$time1)             # Time period selected (first 10 yrs, 2nd 10 yrs), left column
      o.n.sel2 <- o.n.[as.numeric(input$occ.num2)]  # Occupancy/Number, right column
      p.t.v.sel2 <- p.t.v.[as.numeric(input$metric2)]   # Precip/Temp/VPDmax/VPDmin, right column
      e.bs.sel2 <- e.bs[as.numeric(input$est.bs2)]  # Est/ Bootstrap est, right column 
      t.sel2 <- as.numeric(input$time2)             # Time period selected (first 10 yrs, 2nd 10 yrs), right column      
      
      resp.name <- c("temactmean", "mean")
      
      r1 <- fia.dataprep.fcn(o.n.sel1, p.t.v.sel1, t.sel1, e.bs.sel1) 
      r1 <- r1 %>%  dplyr::select(SppNames, SciName, response, LCI, UCI, sig)
      
      r2 <- fia.dataprep.fcn(o.n.sel2, p.t.v.sel2, t.sel2, e.bs.sel2) 
      r2 <- r2 %>%  dplyr::select(SppNames, SciName, response, LCI, UCI, sig)
      
      if (nrow(r1) >= nrow(r2)) {
      r3 <- left_join(r1, r2, by = c("SppNames", "SciName")) %>%
        mutate(missing = ifelse(is.na(sig.y) == TRUE | is.na(sig.x) == TRUE, 1, 0))
      order.r <- "r1r2"
      } else {
        r3 <- left_join(r2, r1, by = c("SppNames", "SciName")) %>%
          mutate(missing = ifelse(is.na(sig.y) == TRUE | is.na(sig.x) == TRUE, 1, 0))
        order.r <- "r2r1"        
      }
      
      
      r3[is.na(r3) == TRUE] <- 0
      r3 <- r3 %>% mutate(sig.any = ifelse(sig.x == 1 | sig.y == 1, 1, 0))
      
      if (input$results.show == 2) {
        r3 <- r3 %>% filter(missing == 0)
      } else if (input$results.show == 3) {
        r3 <- r3 %>% filter(sig.any == 1)
      } else if (input$results.show == 4) {
        r3 <- r3 %>% filter(missing == 0 & sig.any == 1)
      }


      # Axes labels need to correspond to order.r 
      # First the labels are built, then their order correctly assigned.
      title.o.n <- c("Range shift, ", "Density shift, ")
      title.p.t <- c("precipitation, ", "temperature, ", "maximum VPD, ", "minimum VPD, ")
      title.time <- c("first visit, ", "second visit, ")
      title.e.bs <- c("TSE estimate", "bootstrap estimate")
      r1.axis <- paste0(title.o.n[as.numeric(input$occ.num1)], title.p.t[as.numeric(input$metric1)], 
                           title.time[as.numeric(input$time1)], title.e.bs[as.numeric(input$est.bs1)])
      r2.axis <- paste0(title.o.n[as.numeric(input$occ.num2)], title.p.t[as.numeric(input$metric2)], 
                           title.time[as.numeric(input$time2)], title.e.bs[as.numeric(input$est.bs2)])
      if (order.r == "r1r2") {
        plt1.axis <- r1.axis 
        plt2.axis <- r2.axis
      } else {
        plt1.axis <- r2.axis 
        plt2.axis <- r1.axis
        
      }
       
#      browser()
        crossed.plt <- ggplot(r3, aes(response.x, response.y, text = paste0(SciName, "\n", SppNames))) +   # fct_reorder allows for reording the factor level (forecats in tidyverse)
          geom_hline(yintercept = 0, size = 0.1) + 
          geom_vline(xintercept = 0, size = 0.1) +
          geom_linerange(aes(y = response.y, ymin = LCI.y, ymax = UCI.y, color = sig.y)) +
          geom_linerange(aes(x = response.x, xmin = LCI.x, xmax = UCI.x, color = sig.x)) +
          geom_point() +
          theme_classic() + 
          labs(y = plt2.axis, x = plt1.axis) +
          theme(legend.position = "none", 
                text = element_text(family = "serif",
                                    size = 15))
          
        ggplotly(crossed.plt, height = 700, tooltip = "text")
    }) 
    
    
    
    
  })
}


