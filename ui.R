library(shinydashboard)
library(leaflet)
??shiny
header <- dashboardHeader(
  title = "Dublin Buses"
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               tagList(tags$h3("Bus Routes")),
               tryCatch({
                 plotOutput("busmap",height="550")  
               }, error= function(err){
                 flog.info(err)
               })
               
           )
           
           
    ),
    column(width = 3,
           box(width = NULL,
               uiOutput("numVehiclesTable")
           ),
           box(width = NULL, status = "warning",
               uiOutput("routeSelect")
           ),
           box(width=NULL,
               checkboxInput("abvavg", "Show Routes above Average Distance Travelled", FALSE)
           ),
           box(width = NULL, status = "warning",
               sliderInput("zoom","Zoom",min=1,max=12,value=11)
               
           ),
           box(width = NULL, status = "warning",
               selectInput("interval", "Refresh interval",
                           choices = c(
                             "30 seconds" = 30,
                             "1 minute" = 60,
                             "2 minutes" = 120,
                             "5 minutes" = 300,
                             "10 minutes" = 600
                           ),
                           selected = "60"
               ),
               uiOutput("timeSinceLastUpdate"),
               actionButton("refresh", "Refresh now")
               
           )
    )
  ),
  fluidRow(
    column(width = 9,box(width = NULL,
                         tagList(tags$h3("Frequency of Trips")),
                         plotOutput("freq")
    )
    ),
    column( width =3, 
            box( width=NULL,
                 uiOutput("freqSel"),
                 radioButtons("freqSel", "Frequency Of Journey per ",
                              choices = c(
                                "Route",
                                "Stops"
                              ),
                              selected = "Route"
                 ),
                 sliderInput("bin","Bin Sizes",min=1,max=50,value=30)
            )
    )
  ),
  fluidRow(
    
    column(width=12,box(
      width=NULL,
      tagList(tags$h3("Number Of Stops Per Hour")),
      plotOutput("trend")
    ))
  ),
  fluidRow(
    column(width=9,
           box(
             width=NULL,
             tagList(tags$h3("Stops on Map")),
             plotOutput("stopmap",height=550)
           )
    ),
    column(width=3,
           box(
             width=NULL,
             sliderInput("zoomS","Zoom map",min=1,max=18,value=11)
           )
    ),
    column(width=3,
           box(
             width=NULL,
             #                 uiOutput("rangeS")
             sliderInput("rangeS","Stops with trips in range :  ",min=1,max=1951,value=c(1,1951))
           )
    ),
    column(width=3,
           box(
             width=NULL,
             #                 uiOutput("rangeS")
             sliderInput("stopSize","Stops Indicator Size",min=0.1,max=5.0,value=1)
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)