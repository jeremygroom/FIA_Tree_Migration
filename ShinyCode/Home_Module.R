#### Comparison of Species Change Estimates  ####
##     --- Home page ---  ###


Home_UI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(box(width = 12,
                 #status = "success",   # makes the top of the box green.  
                 column(10, offset = 1, align = "center", h1(id = "hp-title", HTML("West coast U.S. tree species evidence shifts in density but not range:<br/>A data dashboard")),
                        tags$style(HTML("#hp-title{color: #154360;
                             font-size: 40px;
                                 font-style: italic;}"))))),
    fluidRow(column(4, offset = 1, align = "center", 
                    imageOutput(ns("movers"))),
             column(width = 5, offset = 1,
                    box(width = 12, h3("Summary"),
                        p("This dashboard is associated with the manuscript \"West coast U.S. tree species evidence shifts in density but not range\", by Jeremiah D. Groom (", 
                          a("Groom Analytics, LLC", href = "https://www.groomanalytics.com/"), ") and Vicente J. Monleon (USDA Forest Service).", style = "font-size:20px"),
                          p("We analyzed ", a("Forest Inventory and Analysis", href = "https://www.fia.fs.usda.gov/"), "(FIA) plot revisit data in the states of California, 
                          Oregon, and Washington to examine shifts in the ranges and densities of tree species 
                          relative to plot temperature and precipitation values.  Plots were first visited between 2001 and 2010, and again ten years later, between 2011 and 
                          2019.  Temperature and precipitation data were obtained from", a("PRISM", href = "https://prism.oregonstate.edu/" ), ".", style = "font-size:20px"),
                        p("We created this dashboard so that visitors could explore our data in greater depth than our manuscript allows.  Explore how changing estimation 
                          procedures affects outcomes.  Compare, say, temperature and precipitation outcomes simultaneously.  Check out the ranges of individual species 
                          and see how they’ve changed over ten years.", style = "font-size:20px"),
                        p("Our hope is that these broad-scale analysis results are useful for those curious about how tree species are responding to a changing climate, 
                          and how, at the level of a tree species, the climate is changing.", style = "font-size:20px"),
                        br(),
                        p("Sketch by Farren Groom, 2022", style = "font-size:20px"))
             )
    )
    
  ) 
}




Home_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    output$movers <- renderImage({
      return(list(
        src = "www/MigratingTrees.jpg",
        width = "512",
        height = "415px",
        contentType = "image/jpg",
        alt = "Moving truck picture"
      ))
    },
    deleteFile = FALSE)
  })
}
