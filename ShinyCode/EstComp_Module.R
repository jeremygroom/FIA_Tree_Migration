#### Comparison of Species Change Estiamtes  ####
##     --- First page ---  ###





EstComp_UI <- function(id) {
  tagList(
    fluidRow(column(width = 6, offset = 3, align = "center", h1("Comparison of Species' Estimates"))),
    fluidRow(column(width = 8, offset = 2, align = "center",
                    box(width = 12,
                        p("This plot allows the side-by-side comparison of different analysis types.  Range shift analyses are those
                   which show the difference in the range-wide temperature or precipitation values for a species between the first and 
                   second visit.  Density shift analyses show the difference in temperature or precipitation values between plots that had more trees at the second 
                   visit and those that had fewer.  The temperature or precipitation values used are estiamted from either the first 10 years up to the first visit (first visit)
                   or from the second ten years up to the second visit (second visit).  Users may also
                   select which variance estimation procedure was used - either variance approximated through a Taylor series expansion approximation (TSE Estimate)
                   or arrived at through a bootstrap procedure (Bootstrap).", style = "font-size:20px")))),
    
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
                                                                    "Temperature" = 2))),
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
                                                                                    "Temperature" = 2))),
                                    fluidRow(width = 12, selectInput(NS(id, "time2"), label = h5("Select time perspective"),
                                                                     choices = list("First visit" = 1,
                                                                                    "Second visit" = 2))))
                    )
             )
    )
  )
  
}




EstComp_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    
    
    
    
    output$plot_estcomp <- renderPlot({
      
      o.n.sel <- o.n.[as.numeric(input$occ.num)]
      p.t.sel1 <- p.t.[as.numeric(input$metric1)]
      e.bs.sel1 <- e.bs[as.numeric(input$est.bs1)] 
      t.sel1 <- as.numeric(input$time1)
      p.t.sel2 <- p.t.[as.numeric(input$metric2)]
      e.bs.sel2 <- e.bs[as.numeric(input$est.bs2)] 
      t.sel2 <- as.numeric(input$time2)      
      
      
      #browser()
      resp.name <- c("temactmean", "mean")
      
      # See dataprep function in functions.r
      r1 <- fia.dataprep.fcn(o.n.sel, p.t.sel1, t.sel1, e.bs.sel1) %>% arrange(desc(response))
      
      r1.order <- r1 %>% dplyr::select(Spp.symbol) %>%
        mutate(order = seq(1:n()))
      
      r2 <- fia.dataprep.fcn(o.n.sel, p.t.sel2, t.sel2, e.bs.sel2) %>%
        left_join(r1.order, by = "Spp.symbol") %>%
        arrange(order)
      
      gls.vals1 <- gls1[[which(names(gls1) == paste0("gls.", o.n.sel, p.t.sel1, t.sel1))]]
      gls.vals2 <- gls1[[which(names(gls1) == paste0("gls.", o.n.sel, p.t.sel2, t.sel2))]]
      
      # Setting up title for each plot
      title.o.n <- c("Range shift, ", "Density shift, ")
      title.p.t <- c("precipitation, ", "temperature, ")
      title.time <- c("first visit, ", "second visit, ")
      title.e.bs <- c("TSE estimate", "bootstrap estimate")
      plt1.title <- paste0(title.o.n[as.numeric(input$occ.num)], title.p.t[as.numeric(input$metric1)], "\n",
                           title.time[as.numeric(input$time1)], title.e.bs[as.numeric(input$est.bs1)])
      plt2.title <- paste0(title.o.n[as.numeric(input$occ.num)], title.p.t[as.numeric(input$metric2)], "\n",
                           title.time[as.numeric(input$time2)], title.e.bs[as.numeric(input$est.bs2)])
      
      # Setting up labels for x axes
      plt.x.ax <- c("Difference in precipitation (mm)", "Difference in temperature (C)")
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


