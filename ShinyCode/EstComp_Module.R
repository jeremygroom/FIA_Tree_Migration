




EstComp_UI <- function(id) {
  tagList(
    fluidRow(column(width = 4, offset = 1, 
                    selectInput(NS(id, "occ.num"), label = h5("Select analysis type"),
                                choices = list("Occupancy" = 1, 
                                               "Number" = 2)))),
    fluidRow(column(width = 8, plotOutput(NS(id, "plot_estcomp"), height = 700))),
    fluidRow(column(width = 4, offset = 1,
                    fluidRow(width = 12, h2("Left Figure Options")),
                    fluidRow(width = 12, selectInput(NS(id, "est.bs1"), label = h5("Estimate or bootstrap"),
                                                     choices = list("Estimate" = 1, 
                                                                    "Bootstrap" = 2),
                                                     selected = 1)),
                    fluidRow(width = 12, selectInput(NS(id, "metric1"), label = h5("Select analysis metric"),
                                                     choices = list("Precipitation" = 1, 
                                                                    "Temperature" = 2))),
                    fluidRow(width = 12, selectInput(NS(id, "time1"), label = h5("Select time perspective"),
                                                     choices = list("First 10 years" = 1,
                                                                    "Second 10 years" = 2)))),

             column(width = 4, offset = 1,
                    fluidRow(width = 12, h2("Right Figure Options")),
                    fluidRow(width = 12, selectInput(NS(id, "est.bs2"), label = h5("Estimate or bootstrap"),
                                                     choices = list("Estimate" = 1, 
                                                                    "Bootstrap" = 2),
                                                     selected = 2)),
                    fluidRow(width = 12, selectInput(NS(id, "metric2"), label = h5("Select analysis metric"),
                                                     choices = list("Precipitation" = 1, 
                                                                    "Temperature" = 2))),
                    fluidRow(width = 12, selectInput(NS(id, "time2"), label = h5("Select time perspective"),
                                                     choices = list("First 10 years" = 1,
                                                                    "Second 10 years" = 2))))
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
      
      r1.order <- r1 %>% select(Spp.symbol) %>%
        mutate(order = seq(1:n()))
      
      r2 <- fia.dataprep.fcn(o.n.sel, p.t.sel2, t.sel2, e.bs.sel2) %>%
        left_join(r1.order, by = "Spp.symbol") %>%
        arrange(order)
      
      gls.vals1 <- gls1[[which(names(gls1) == paste0("gls.", o.n.sel, p.t.sel1, t.sel1))]]
      gls.vals2 <- gls1[[which(names(gls1) == paste0("gls.", o.n.sel, p.t.sel2, t.sel2))]]
      
      title.o.n <- c("Occupancy, ", "Tree number, ")
      title.p.t <- c("precipitation, ", "temperature, ")
      title.time <- c("first 10 yrs, ", "second 10 yrs, ")
      title.e.bs <- c("estimate", "bootstrap estimate")
      plt1.title <- paste0(title.o.n[as.numeric(input$occ.num)], title.p.t[as.numeric(input$metric1)], "\n",
                           title.time[as.numeric(input$time1)], title.e.bs[as.numeric(input$est.bs1)])
      plt2.title <- paste0(title.o.n[as.numeric(input$occ.num)], title.p.t[as.numeric(input$metric2)], "\n",
                           title.time[as.numeric(input$time2)], title.e.bs[as.numeric(input$est.bs2)])

      # Calling on plotting functions from functions.r
      q1 <- e.c.plot.fcn(r1, "response", gls.vals1, plt1.title, r1$SciName2, NULL, 1)# r1$SciName2, NULL, 1 )#
      p1 <- e.c.plot.fcn(r2, "response", gls.vals2, plt2.title, r2$Spp.symbol, NULL, 0)#1)

      # plotting q1 (estimated mean/var) and p1 (bootstrap mean/var)
      ##  Change y-axis of p1 to null
      q1 <- as.grob(q1)
      p1 <- as.grob(p1)
      
      p_all <- plot_grid(q1, p1, ncol = 2, rel_widths = c(1, 0.65))
      p_all
      
      
    }) 
    
    
    
    
  })
}


