library(googleVis)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(devtools)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggmap)
library(leafletR)
library(maps)
library(maptools)
library(sp)
library(RJSONIO)
library(futile.logger)

# 1=South, 2=East
dirColors <-c("0"="#595490", "1"="#527525")
setwd("~/Data Viz/ass3")



# Load  stop, trip and route data and shape data
trips  <- readRDS("data/trips.Rds")
flog.info('Loaded trips')
shapes <- readRDS("data/shapes.Rds")
flog.info('Loaded shape')
routes <- readRDS("data/routes.Rds")
flog.info('Loaded routes')
stop_times <- readRDS("data/stop_times.Rds")
flog.info('Loaded stop_times')
stops <-readRDS('data/stops.Rds')
flog.info('Loaded stops')



# Get the shape for a particular route. This isn't perfect. Each route has a
# large number of different trips, and each trip can have a different shape.
# This function simply returns the most commonly-used shape across all trips for
# a particular route.
get_route_shape <- function(route) {
  routeid <- route
  flog.info("routeid to nrow get %s",routeid)
  flog.info("routeid to nrow get %s",nrow(routeid))
  flog.info("routeid is null %s",is.null(routeid))
  if (is.null(routeid) | nrow(routeid) <1) {
    routeid <-routes
  }else{
    routeid <-filter(routes,route_short_name == routeid$route_short_name)  
  }
  
  
  # For this route, get all the shape_ids listed in trips, and a count of how
  # many times each shape is used. We'll just pick the most commonly-used shape.
  
  shape_counts <- trips %>%
    filter(route_id == routeid$route_id) %>%
    group_by(shape_id) %>%
    summarise(n = n()) %>%
    arrange(-n)
  
  shapeid <- unique(shape_counts$shape_id)
  
  # Get the coordinates for the shape_id
  res <- shapes %>% filter(shape_id == shapeid)
  res
}
#make sure the zoom level is correct
get_zoom <- function(zoomIn){
  zoom <-NULL
  if(is.null(zoomIn)){
    zoom<-11
  }else{
    zoom<-zoomIn
  }
  
  return(zoom)
}




function(input, output, session) {
  
  # Route select input box
  output$routeSelect <- renderUI({
    
    routeNums <- sort(unique(as.character(routes$route_short_name)))
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(All = 0, routeNums)
    selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[1])
  })
  #select the variable the histogram counts for 
  output$freqSelect <- renderUI({
    freqSel <- c("Trips", "Stops")
    selectInput("freqSel", "Freq Select", choices = freqSel, selected = freqSel[2])
  })
  # bin slider for histogram
  output$binSize <- renderUI({
    sliderValues()
  })
  #zoom slider for map
  output$zoomSlider <- renderUI({
    sliderValues()
  })
  #stop sizes on map
  output$stopSize <- renderUI({
    sliderValues()
  })
  
  # range of stops slider
  output$rangeS <- renderUI({
    groupS <- group_by(stop_times,stop_id) %>% summarise(trips=n())
    minT <- min(groupS$trips)
    maxT <- max(groupS$trips)
    sliderValues("rangeS","Stops",min = minT , max = maxT , value=10)
  })
  # Locations of all active vehicles
  vehicleLocations <- reactive({
    
    input$refresh # Refresh if button clicked
    
    # Get interval (minimum 30)
    interval <- max(as.numeric(input$interval), 30)
    # Invalidate this reactive after the interval has passed, so that data is
    # fetched again.
    invalidateLater(interval * 1000, session)
    
    routes
    
  })
  
  # Locations of a particular route or all
  routeVehicleLocations <- reactive({
    
    locations <- vehicleLocations()
    
    if (is.null(input$routeNum))
      return(locations)
    
    flog.info(input$routeNum)
    res <- NULL
    if(input$routeNum ==0){
      res <- locations
    }else{
      res <- locations[locations$route_short_name == input$routeNum, ]
    }
    
    flog.info("route vehicle location %s",nrow(res))
    res
  })
  
  # Get time that vehicles locations were updated
  lastUpdateTime <- reactive({
    vehicleLocations() # Trigger this reactive when vehicles locations are updated
    Sys.time()
  })
  
  # Number of seconds since last update
  output$timeSinceLastUpdate <- renderUI({
    # Trigger this every 5 seconds
    invalidateLater(500000, session)
    p(
      class = "text-muted",
      "Data refreshed ",
      round(difftime(Sys.time(), lastUpdateTime(), units="secs")),
      " seconds ago."
    )
  })
  #count of trips per direction per rendered routes
  output$numVehiclesTable <- renderUI({
    locats <- trips
    rts <-routeVehicleLocations()
    
    locats <- locats %>% filter(route_id %in% (rts$route_id))
    if (length(locats) == 0 || nrow(locats) == 0)
      return(NULL)
    
    # Create a Bootstrap-styled table
    tags$table(class = "table",
               tags$thead(tags$tr(
                 tags$th("Color"),
                 tags$th("Direction"),
                 tags$th("Number of Stops")
               )),
               tags$tbody(
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:blue; display:inline-block;",
                     dirColors[4]
                   ))),
                   tags$td("Northbound"),
                   tags$td(nrow(locats[locats$direction_id == "0",]))
                 ),
                 tags$tr(
                   tags$td(span(style = sprintf(
                     "width:1.1em; height:1.1em; background-color:green; display:inline-block;",
                     dirColors[1]
                   ))),
                   tags$td("Southbound"),
                   tags$td(nrow(locats[locats$direction_id == "1",]))
                 ),
                 tags$tr(class = "active",
                         tags$td(),
                         tags$td("Total"),
                         tags$td(nrow(locats))
                 )
               )
    )
  })
  #trend plot of stops per hout
  output$trend <- renderPlot({
    
    res <- group_by(stop_times,hr=substr(departure_time,0,2)) %>% summarise(n=n())
    
    res[] <- lapply(res, as.numeric)
    
    p<-ggplot(res, aes(hr, n)) +
      geom_point(color="light grey",size=7) +
      geom_line(color="blue") +
      labs( x="Hour", 
            y = "Number Of Stops")+theme(axis.ticks = element_blank(), axis.text.y = element_blank())
    p
  })
  #plot frequency of map of trips per route or stop
  output$freq <- renderPlot({
    freq <- input$freqSel
    bin <- input$bin
    freqD <- NULL
    flog.info("Freq %s",freq)
    m<-NULL
    if( freq =="Route"){
      freqD <- group_by(trips,route_id) %>% summarise(trips_count=n())
      
    }else if (freq =="Stops"){
      freqD <- group_by(stop_times,stop_id) %>% summarise(trips_count=n())
      
    }else {
      return(NULL)
    }
    m <- ggplot(freqD, aes(x=trips_count)) + geom_histogram(binwidth = bin,color="030300",fill="030300",alpha=0.3)+labs(x=freq)
    m
  })
  
  # Store last zoom button value so we can detect when it's clicked
  lastZoomButtonValue <- NULL
  #plot bus routes 
  # check if the bus routes need to be over average route num be route
  #check zoom level of map
  output$busmap <- renderPlot({
    locations <- routeVehicleLocations()
    
    flog.info("locations %s",nrow(locations))
    
    
    # Show only selected directions
    rt <- filter(routes,route_short_name %in% input$routeNum)
    trip <- get_route_shape(rt)
    flog.info("is checked %s",input$abvavg)
    
    flog.info("trip %s",nrow(trip))
    
    # Four possible directions for bus routes
    dirPal <- colorFactor(dirColors, names(dirColors))
    
    if(input$abvavg){
      res<-group_by(stop_times,trip_id) %>% summarise(totdist=sum(shape_dist_traveled))
      avgDist<-mean(res$totdist)
      
      res <- filter(res,totdist>avgDist)
      res <- unique(res$trip_id)
      
      trip <-filter(trips,trip_id %in% res)
      flog.info("trip Lavbels %s",nrow(trip))
    }
    flog.info("trip Lavbels %s",head(trip))
    flog.info("locations Lavbels %s",head(locations))
    
    
    shapes_metro <- shapes %>% filter(shape_id %in% trip$shape_id)  %>%
      arrange(shape_id, shape_pt_sequence)
    flog.info("shapes_met %s",nrow(shapes_metro))
    box <- make_bbox(shape_pt_lon, shape_pt_lat, data = shapes_metro )
    
    
    theme_set(theme_bw(base_size = 12))
    flog.info("Zoom %s",get_zoom(input$zoom))
    al1 <- get_map(location = box, zoom =get_zoom(input$zoom), maptype = 'terrain')
    map <- ggmap(al1) +
      geom_path(data=shapes_metro, aes(shape_pt_lon, shape_pt_lat, group=shape_id), color="blue", size=0.5, alpha=0.8) 
    flog.info(class(map))
    map
  })
  
  #plot all stops with in the slider range
  output$stopmap <- renderPlot({
    
    
    flog.info("rage stops 1 %s",input$rangeS[1])
    flog.info("rage stops 2 %s",input$rangeS[2])
    
    grp_stp <- stop_times %>% group_by(stop_id) %>% summarise(trips=n())
    
    flog.info("grp_stp size %s",nrow(grp_stp))
    
    grp_stp <- filter(grp_stp,trips >input$rangeS[1])
    
    grp_stp <- filter(grp_stp,trips < input$rangeS[2])
    
    
    sps <- filter(stops,stop_id %in% grp_stp$stop_id)
    
    box <- make_bbox(stop_lon, stop_lat, data = sps)
    al1 <- get_map(location = box, zoom =get_zoom(input$zoomS), maptype = 'terrain')
    map <- ggmap(al1) + geom_point(data=sps , aes(stop_lon,stop_lat),color="red",size=input$stopSize,alpha=0.6)
    
    map
  })
}