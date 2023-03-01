## Background module ##


Bkgrnd_UI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(box(width = 12,
                 #status = "success",   # makes the top of the box green.  
                 column(10, offset = 1, align = "center", h1(id = "hp-title", "Background"),
                        tags$style(HTML("#hp-title{color: #154360;
                             font-size: 40px;
                                 font-style: italic;}"))))),
    fluidRow(column(width = 6, offset = 1,
                    div(htmlOutput("box1"),  style = 'color:white; border-radius: .5em; font-size:20px; background-color:#272a8f; padding: 20px; margin: 20px;' ,
                        h3("Situation:"),
                        p("Climate change may be geographically shifting the location of tree species' niches.", style = "font-size:20px"),
                        p("Tree species may adjust their ranges along elevational and/or latitudinal gradients in mountainous regions through mortality and recruitment."
                          , style = "font-size:20px"),
                        p("Changes in temperature and precipitation may drive geographic niche shifts.", style = "font-size:20px"),
                        br(),
                        h3("Questions:"),
                        p(em("Are tree species shifting their ranges according to changes in temperature and precipitation?"), style = "font-size:20px"),
                        p(em("Are their densities shifting within their ranges?"), style = "font-size:20px"),
                        p(em("How are temperature and precipitation changing within species’ ranges?"), style = "font-size:20px")
                    ),
                    fluidRow(column(width = 12, 
                                    div(htmlOutput("box2"),  style = 'color:black; border-radius: .5em; font-size:20px; background-color:#b8d9b9; padding: 20px; margin: 20px;' ,
                                        h3("Opportunity and methods:"),
                                        p("The U.S. Forest Service Forest Inventory and Analysis (FIA) program completed revisiting plots in California, Washington, 
                                          and Oregon. Ten years passed between plot visits."), 
                                        p("This represents a chance to directly measure plot changes between visits."), 
                                        p("A \"range shift\" is a change in the mean precipitation or temperature values for plots occupied by a given species
                                          species between visits. "),
                                        p("A \"density shift\" is the mean precipitation or temperature difference between plots that gained or lost individual
                                          trees of a species. ")
                                        
                                    )))
    ), 
    column(4, align = "center", 
           div(htmlOutput("box3"),  style = 'color:black; border-radius: .5em; font-size:20px; padding: 20px; margin: 20px; background-color:white;' ,
               #box(width = 12, height = "800px", 
               fluidRow(column(12, imageOutput(ns("plotpts"), height = "714px"))),
               fluidRow(column(12, p("Location of FIA plots used in this study.", style = "font-size:20px")))
           )
           
    )
    
    ),
    fluidRow(column(width = 5, offset = 1,
                    div(htmlOutput("box4"),  style = 'color:black; border-radius: .5em; font-size:20px; background-color:#dbe887; padding: 20px; margin: 20px;' ,
                        h3("Analysis Step 1: Impute Climate Values"),
                        p("The first step involves imputing the temperature and precipitation values for each FIA plot at the first and second visit.  We did this 
                          by using the 10 years of data up to and including the first visit year and second visit year.  In the below example, the plot on the 
                          left includes mean yearly temperature data from 1998 to 2007, where 2007 is the year of the first plot visit.  The plot on the right
                          uses year data from 2008 to 2017, where 2017 is the year of the second visit.  Open circles represent the imputed first and second visit values."),
                        br(),
                        fluidRow(column(12, imageOutput(ns("method1")), align = "center")))),
             
             column(width = 5, 
                    div(htmlOutput("box5"),  style = 'color:black; border-radius: .5em; font-size:20px; background-color:#cba3cc; padding: 20px; margin: 20px;' ,
                        h3("Analysis Step 2: Estimate Range Shifts"),
                        p("By species, we find the mean difference of occupied FIA plot temperature (or precipitation) values between the first and second visit.  We anchor
                          the climate values by using either the first", strong("or"), "the second visit value.  Below we used plot temperatures (degrees C) from Visit 1.
                          Note that the mean of Visit 2 plot temperatures are to the left of the Visit 1 mean.  This indicates that the mean of occupied plot temperatures 
                          have shifted towards plots that were cooler in Visit 1."),
                        br(),
                        p("Click on the tabs", em("\"Comparison of species change estimates\""), " and ", em("\"Crossed comparisons of species change estimates\""), "to see the results of this analysis."),
                        br(),
                        fluidRow(column(12, imageOutput(ns("method2")), align = "center"))))
    ),
    fluidRow(column(width = 5, offset = 1,
                    div(htmlOutput("box6"),  style = 'color:black; border-radius: .5em; font-size:20px; background-color:#37d490; padding: 20px; margin: 20px;' ,
                        h3("Analysis Step 3: Estimate Density Shifts"),
                        p("By species, we find the mean difference in temperature (or precipitation) values between plots that gained and lost trees by Visit 2.  We again
                          anchor the the climate values by using either the first", strong("or"), "the second visit values for calculating this mean change.  
                          Below we used plot temperatures (degrees C) from Visit 1.  In this case, the mean temperature value for plots that gained trees are to the left
                          (cooler plots) than those that lost trees, indicating that a range shift towards previously-cooler plots (if we are using Visit 1's plot temperature
                          values) may be underway."),
                        br(),
                        p("Click on the tabs", em("\"Comparison of species change estimates\""), " and ", em("\"Crossed comparisons of species change estimates\""), "to see the results of this analysis."),
                        br(),
                        fluidRow(column(12, imageOutput(ns("method3")), align = "center")))),
             
             column(width = 5, 
                    div(htmlOutput("box7"),  style = 'color:black; border-radius: .5em; font-size:20px; background-color:#d1b424; padding: 20px; margin: 20px;' ,
                        h3("Analysis Step 4: Change in Climate Variables"),
                        p("How did temperature (or precipitation) change across individual species' plots during the 10-year interval? We model the change between visits (
                        &Delta ) by Visit 1 (or Visit 2) climate variable values.  The code selects whether a slope should be used to model the values, or if an intercept-only
                          model is sufficient."),
                        br(),
                        p("Click on the tab", em("\" Map of temperature, precipitation, and their changes over time \""), " to see the results of this analysis and more."),
                        br(),
                        fluidRow(column(12, imageOutput(ns("method4")), align = "center"))))
    )    
  )
}




Bkgrnd_Server <- function(id) {
  
  
  moduleServer(id, function(input, output, session) {
    output$plotpts <- renderImage({
      return(list(
        src = "www/ForestedPtsFig.jpg",
        width = "400px",   #  h/w = 1.787276
        height = "714px",
        contentType = "image/jpg",
        alt = "Locations of FIA plots"
      ))
    },
    deleteFile = FALSE)
    
    output$method1 <- renderImage({
      return(list(
        src = "www/Method1.jpg",
          width = "582px",   #  h/w = 0.3951515
          height = "230px",
        contentType = "image/jpg",
        alt = "First analysis step."
      ))
    },
    deleteFile = FALSE)
    
    output$method2 <- renderImage({
      return(list(
        src = "www/Method2.jpg",
          width = "541px",   #  h/w = 0.4251969
          height = "230px",
        contentType = "image/jpg",
        alt = "Second analysis step."
      ))
    },
    deleteFile = FALSE)
    
    output$method3 <- renderImage({
      return(list(
        src = "www/Method3.jpg",
        width = "500px",   #  h/w = 0.4588068
        height = "229px",
        contentType = "image/jpg",
        alt = "Third analysis step."
      ))
    },
    deleteFile = FALSE)
    
    output$method4 <- renderImage({
      return(list(
        src = "www/Method4.jpg",
        width = "522px",   #  h/w = 0.4402439
        height = "230px",
        contentType = "image/jpg",
        alt = "Third analysis step."
      ))
    },
    deleteFile = FALSE)
    
  })
}

