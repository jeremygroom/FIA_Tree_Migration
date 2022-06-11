


TabVarTiming_UI <- function(id) {
  tagList(
    fluidRow(column(width = 10, offset = 2, h1("Comparisons of Variance Estimation and Timing Approaches on Species' Results"))),
    fluidRow(column(width = 3, offset = 1, 
                    selectInput(NS(id, "occ.num"), label = h3("Select analysis type"),
                                choices = list("Occupancy" = 1, 
                                               "Density" = 2))),
             column(width = 3, selectInput(NS(id, "metric"), label = h3("Select metric"),
                                           choices = list("Precipitation" = 1, 
                                                          "Temperature" = 2))),
             column(width = 5, selectInput(NS(id, "var.time"), label = h3("Select comparison of variance or timing method"),
                                           choices = list("Variance (Taylor series approximation vs. bootstrap)" = 1, 
                                                          "Timing (First vs. second 10 years)" = 2),
                                           selected = 1))),
    # Tables UI
    fluidRow(column(width = 4, offset = 1, uiOutput(NS(id, "tab1_title")),
             fluidRow(width = 12, tableOutput(NS(id, "table1")))),
             column(width = 4, offset = 1, uiOutput(NS(id, "tab2_title")),
                    fluidRow(width = 12, tableOutput(NS(id, "table2"))))),
    # Histogram UI
    fluidRow(column(width = 4, offset = 1, plotOutput(NS(id, "hist1"))),
             column(width = 4, offset = 1, plotOutput(NS(id, "hist2"))))
    
  )
}



TabVarTiming_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    
    
    output$tab1_title <- renderUI({
      if (input$var.time == "1") {
        tab1.title <- "Variation Difference (Bootstrap - Taylor),<br/>First 10 Years"
      } else {
        tab1.title <- "Difference in Years (Second 10 - First 10),<br/>Taylor Series Estimated Variance"
      }
      h3(HTML(tab1.title))
    })
    
    output$tab2_title <- renderUI({
      if (input$var.time == "1") {
        tab2.title <- "Variation Difference (Bootstrap - Taylor),<br/>Second 10 Years"
        } else {
          tab2.title <- "Difference in Years (Second 10 - First 10),<br/>Bootstrap Variance"
        }
      h3(HTML(tab2.title))
    })
    

        
    tableprep <- reactive({
      
      o.n.sel <- o.n.[as.numeric(input$occ.num)]
      p.t.sel <- p.t.[as.numeric(input$metric)]
      
      #browser()
      
      r1 <- fia.dataprep.fcn(o.n.sel, p.t.sel, 1, e.bs[1]) 
      r2 <- fia.dataprep.fcn(o.n.sel, p.t.sel, 1, e.bs[2])
      r3 <- fia.dataprep.fcn(o.n.sel, p.t.sel, 2, e.bs[1])
      r4 <- fia.dataprep.fcn(o.n.sel, p.t.sel, 2, e.bs[2])
      

      tab.prep.fcn <- function(rx1, rx2) {
        rx.out1 <- rx1 %>% 
          mutate(CIrange = UCI - LCI)
        
        rx.out2 <- rx2 %>% 
          mutate(CIrange = UCI - LCI)
        
        rx.out <- tibble(`Common Names` = rx.out1$SppNames, `Scientific Names` = rx.out1$SciName, 
                         `Mean Response Difference` = rx.out2$response - rx.out1$response, 
                         `CI Range Difference` = rx.out2$CIrange - rx.out1$CIrange,
                         `Percentage CI Range Difference` = (rx.out2$CIrange - rx.out1$CIrange) / rx.out1$CIrange * 100)
        
      }
      
      
      if (input$var.time == "1") {
        tab1 <- tab.prep.fcn(r1, r2)
        tab2 <- tab.prep.fcn(r3, r4)

      } else {
        tab1 <- tab.prep.fcn(r1, r3)
        tab2 <- tab.prep.fcn(r2, r4)
      }
      
      #      browser()    
      return(list(tab1 = tab1, tab2 = tab2))
    })
    
    
    
    output$table1 <- renderTable(tableprep()$tab1, hover = TRUE, spacing = "s")
    
    output$table2 <- renderTable(tableprep()$tab2, hover = TRUE, spacing = "s")
    
    output$hist1 <- renderPlot({
      hist.dat1 <- tableprep()$tab1

      if (input$var.time == "1") {
        hist1.title <- "Histogram of percentage CI difference\n(Bootstrap - Taylor) / Taylor, First 10 Years"
      } else {
        hist1.title <- "Histogram of percentage CI difference in years\n(Second 10 - First 10) / (First 10), Taylor Series Estimated Variance"
      }
      
      ggplot(hist.dat1, aes(`Percentage CI Range Difference`)) + 
        geom_histogram(bins = 50) + 
        geom_vline(xintercept = 0) + 
        geom_hline(yintercept = 0) +
        labs(title = hist1.title, y = "Number of Species") + 
        theme_bw() + 
        theme(text = element_text(size = 20))
      
#browser()
          })
    
    output$hist2 <- renderPlot({
      hist.dat2 <- tableprep()$tab2
      
      if (input$var.time == "1") {
        hist2.title <- "Histogram of percentage CI difference\n(Bootstrap - Taylor) / Taylor, Second 10 Years"
      } else {
        hist2.title <- "Histogram of percentage CI difference in years\n(Second 10 - First 10) / (First 10), bootstrap variance"
      }
      
      ggplot(hist.dat2, aes(`Percentage CI Range Difference`)) + 
        geom_histogram(bins = 50) + 
        geom_vline(xintercept = 0) + 
        geom_hline(yintercept = 0) +
        labs(title = hist2.title, y = "Number of Species") + 
        theme_bw() + 
        theme(text = element_text(size = 20))
      
    })
    
    
    
  })
}

