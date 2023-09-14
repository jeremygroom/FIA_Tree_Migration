#### Comparison of Species Change Estiamtes  ####
##     --- First page ---  ###





EstComp_UI <- function(id) {
  tagList(
    fluidRow(box(width = 12,
                 #status = "success",   # makes the top of the box green.  
                 column(10, offset = 1, align = "center", h1(id = "hp-title", "Comparison of Species' Estimates"),
                        tags$style(HTML("#hp-title{color: #154360;
                             font-size: 40px;
                                 font-style: italic;}"))))),
    fluidRow(12, h1("")),
    
    fluidRow(column(width = 8, plotOutput(NS(id, "plot_estcomp"), height = 800)),
             column(width = 4, h2("Select analysis type"),
                    fluidRow(width = 12, selectInput(NS(id, "occ.num"), label = NULL,
                                                     choices = list("Range Shift" = 1, 
                                                                    "Density Shift" = 2))),
                    fluidRow(width = 12, h2("Left Figure Options")),
                    fluidRow(width = 12, selectInput(NS(id, "est.bs1"), label = h5("TSE Estimate or bootstrap"),
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
                                                                    "Second visit" = 2))),
                    
                    fluidRow(column(width = 12, 
                                    fluidRow(width = 12, h2("Right Figure Options")),
                                    fluidRow(width = 12, selectInput(NS(id, "est.bs2"), label = h5("TSE Estimate or bootstrap"),
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
                                                                                    "Second visit" = 2))))
                    )
             )
    ),
    fluidRow(column(width = 8, offset = 2,
                    div(htmlOutput("box8"),  style = 'color:white; border-radius: .5em; font-size:22px; background-color:#272a8f; padding: 20px; margin: 20px;' ,
                        h3("Figure Explanation"),
                        p("This plot allows the side-by-side comparison of different analysis types.  Range shift analyses are those
                   which show the difference in the range-wide temperature or precipitation values for a species between the first and 
                   second visit. Density shift analyses show the difference in temperature or precipitation values between plots that had more trees at the second 
                   visit and those that had fewer.  See the \"Background\" tab for more information."),
                        p("The values for selected climate variables used are estiamted from either the first 20 years up to the first visit (First visit)
                   or from the second ten years up to the second visit (Second visit; see \"Background\" tab for more detail).  Users may also
                   select which variance estimation procedure was used - either variance approximated through a Taylor series expansion approximation (TSE Estimate)
                   or estiamted via a bootstrap procedure (Bootstrap).")))),
    
    fluidRow(column(width = 8, offset = 2,
                    div(htmlOutput("box9"),  style = 'color:black; border-radius: .5em; font-size:20px; background-color:#b8d9b9; padding: 20px; margin: 20px;' ,
                        h2("Figure Interpretations"),
                        h3("Range Shift, Temperature"),
                        p("If the user selected ", strong("Range Shift"), " as an analysis type and ", strong("Temperature"), " as an analysis metric, then a circle to the left of the vertical 
                 zero line indicates that the average temperature of the second-visit occupied plots was cooler than the average temperature of the first-visit occupied plots.
                          A circle to the right of the line indicates that the second-visit occupied plots were on average warmer."),
                        p("If the user also selected ", strong("First visit"), " as a time perspective, then only the plot temperatures from the first visit are considered.  \"Cooler\" means
                that the species' range has shifted towards plots that ", em("were cooler during the first visit.")),
                        p("If the user selected ", strong("Second visit"), " as a time perspective, then only the plot temperatures from the second visit are considered.  \"Cooler\" then means
                that the species is occupying plots that ", em("at the second visit were cooler.")),
                        p("Users are able to select between the first-visit or second-visit climate variables because, without digging deeper, we do not know
                          what happened to plot temperatures over time.  If all plots warmed, and did so evenly, then it should not matter whether we use temperature values 
                          from the first visit or the second visit in this analysis.  This begs some questions: did all plot temperatures warm?  Did they warm evenly?  The user
                           is able to see if it matters which visit's values are used in the analysis.  The user can also see, across a species' range, what happened.  Check out
                           the tab \"Map of temperature, precipitation, and their changes over time.\""),
                        h3("Range Shift, Precipitation"),
                        p("If the user selects ", strong("Range Shift"), " as an analysis type and ", strong("Precipitation"), " as an analysis metric, a circle to the left of the vertical 
                 zero line indicates that the average precipitation value for the second visit is less than the average precipitation value for the first visit.  I.e., the 
                          species' range has shifted towards relatively drier plots.  If the selected time perspective is ", strong("First visit"), " the species' range is shifting towards 
                          plots that were initially drier; a similar shift for ", strong("Second visit"), " plots means the shift is towards plots that were drier as of the second visit."),
                        h3("Range Shift, Vapor Pressure Deficit"),
                        p("If the user selects ", strong("Range Shift"), " as an analysis type and ", strong("Maximum Vapor Pressure Deficit"), " or ", strong("Minimum Vapor Pressure Deficit"), 
                        " as an analysis metric, a circle to the left of the vertical zero line indicates that the average vapor pressure deficit (VPD) value for the second visit is less than 
                        the average precipitation value for the first visit.  I.e., the species' range has shifted towards those plots with relatively lower maximum or minimum VPD values.  
                        If the selected time perspective is ", strong("First visit"), " the species' range is shifting towards plots with lower initial VPD values; a similar shift 
                        for ", strong("Second visit"), " plots means the shift is towards plots with lower maximum or minimum VPD as of the second visit."),
                        h3("Density Shift, Temperature"),
                        p("If the user selects ", strong("Density Shift"), " as an analysis type and ", strong("Temperature"), " as an analysis metric, a circle to the left of the vertical 
                 zero line indicates that, as of the second visit, more trees were found in cooler plots and/or fewer trees were found in warmer plots. The same time perspective interpretations 
                 described above apply."), 
                        h3("Density Shift, Precipitation"),
                        p("If the user selects ", strong("Density Shift"), " as an analysis type and ", strong("Precipitation"), " as an analysis metric, a circle to the left of the vertical 
                 zero line indicates that, on average and as of the second visit, more trees were found in drier plots and/or fewer trees were found in wetter plots. The same time perspective interpretations 
                 described above apply."),
                        h3("Density Shift, Vapor Pressure Deficit"),
                        p("If the user selects ", strong("Density Shift"), " as an analysis type and ", strong("Maximum Vapor Pressure Deficit"), " or ", strong("Minimum Vapor Pressure Deficit"), " as an 
                        analysis metric, a circle to the left of the vertical zero line indicates that, on average and as of the second visit, more trees were found in plots with lower maximum or minimum VPD 
                        and/or fewer trees were found in plots with higher VPD values. The same time perspective interpretations described above apply.")
                        
                    )
    ))
    
  )
  
}




EstComp_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    
    
    
    
    output$plot_estcomp <- renderPlot({
      
      o.n.sel <- o.n.[as.numeric(input$occ.num)]
      p.t.v.sel1 <- p.t.v.[as.numeric(input$metric1)]
      e.bs.sel1 <- e.bs[as.numeric(input$est.bs1)] 
      t.sel1 <- as.numeric(input$time1)
      p.t.v.sel2 <- p.t.v.[as.numeric(input$metric2)]
      e.bs.sel2 <- e.bs[as.numeric(input$est.bs2)] 
      t.sel2 <- as.numeric(input$time2)      
      
      
      #browser()
      resp.name <- c("temactmean", "mean")
      
      # See dataprep function in functions.r
      r1 <- fia.dataprep.fcn(o.n.sel, p.t.v.sel1, t.sel1, e.bs.sel1) %>% arrange(desc(response))
      
      r1.order <- r1 %>% dplyr::select(Spp.symbol) %>%
        mutate(order = seq(1:n()))
      
      r2 <- fia.dataprep.fcn(o.n.sel, p.t.v.sel2, t.sel2, e.bs.sel2) %>%
        left_join(r1.order, by = "Spp.symbol") %>%
        arrange(order)
      
      gls.vals1 <- gls1[[which(names(gls1) == paste0("gls.", o.n.sel, p.t.v.sel1, t.sel1))]]
      gls.vals2 <- gls1[[which(names(gls1) == paste0("gls.", o.n.sel, p.t.v.sel2, t.sel2))]]
      
      # Setting up title for each plot
      title.o.n <- c("Range shift, ", "Density shift, ")
      title.p.t <- c("precipitation, ", "temperature, ", "maximum vapor pressure deficit", "minimum vapor pressure deficit")
      title.time <- c("first visit, ", "second visit, ")
      title.e.bs <- c("TSE estimate", "bootstrap estimate")
      plt1.title <- paste0(title.o.n[as.numeric(input$occ.num)], title.p.t[as.numeric(input$metric1)], "\n",
                           title.time[as.numeric(input$time1)], title.e.bs[as.numeric(input$est.bs1)])
      plt2.title <- paste0(title.o.n[as.numeric(input$occ.num)], title.p.t[as.numeric(input$metric2)], "\n",
                           title.time[as.numeric(input$time2)], title.e.bs[as.numeric(input$est.bs2)])
      
      # Setting up labels for x axes
      plt.x.ax <- c("Difference in precipitation (mm)", "Difference in temperature (C)", "Difference in maximum VPD, hPa", "Difference in minimum VPD, hPa")
      plt1.x.ax <- plt.x.ax[as.numeric(input$metric1)]
      plt2.x.ax <- plt.x.ax[as.numeric(input$metric2)]      
      
      # Calling on plotting functions from functions.r
      q1 <- e.c.plot.fcn(r1, "response", gls.vals1, plt1.title, r1$SciName2, plt1.x.ax, NULL, 1, 15)# r1$SciName2, NULL, 1 )#
      p1 <- e.c.plot.fcn(r2, "response", gls.vals2, plt2.title, r2$Spp.symbol, plt2.x.ax, NULL, 0, 15)#1)
      
      # plotting q1 (estimated mean/var) and p1 (bootstrap mean/var)
      ##  Change y-axis of p1 to null
      q1 <- as.grob(q1)
      p1 <- as.grob(p1)
      
      p_all <- plot_grid(q1, p1, ncol = 2, rel_widths = c(1, 0.75))
      p_all
      
      
    }) 
    
    
    
    
  })
}


