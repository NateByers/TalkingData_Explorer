library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(sunburstR)
library(leaflet)
load("data/prepped_data.rda")

city_names <- paste0("<b><a href='https://en.wikipedia.org/wiki/",
                    gsub(" ", "_", map_summary$region),
                    "'>",
                    map_summary$region,
                    "</a></b>")
number_events <- paste0(map_summary$events, " events on") 
number_devices <- paste0(map_summary$devices, " devices")
overview_content <- paste(sep = "<br/>", city_names, number_events,
                          number_devices)

######################################################################3

ui <- dashboardPage(
  dashboardHeader(title = "TalkingData Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 7, 
                    leafletOutput("map", height = 550)
                    ),
                box(width = 4, offset = 1,
                    title = "Demographics (in view)",
                    p("Below are plots of age and gender based on the data for events
                      that are within the boundary of the map."),
                    plotOutput("age_histogram", height = 250),
                    plotOutput("gender_bar", height = 200)
                    )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    
    leaflet(map_summary) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 112.80, lat = 31.85, zoom = 4) %>%
      addCircleMarkers() %>%
      addPopups(map_summary$longitude, map_summary$latitude, overview_content)
  })
  
  # A reactive expression that returns the set of events that are
  # in bounds right now
  eventsInBounds <- reactive({
    print(input$map_bounds)
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(full_data,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })


  output$age_histogram <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(eventsInBounds()) == 0)
      return(NULL)

    hist(eventsInBounds()$age,
         main = "",
         xlab = "Age",
         col = '#00DD00',
         border = 'white')
  })
  
  output$gender_bar <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(eventsInBounds()) == 0)
      return(NULL)
    
    counts <- table(eventsInBounds()$gender)
    
    barplot(counts, xlab = "Gender", col = 'dodgerblue', border = 'white')
  })

  
  # 
  # # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #                              selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  # 
  # # When map is clicked, show a popup with city info
  # observe({
  #   leafletProxy("map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   if (is.null(event))
  #     return()
  #   
  #   isolate({
  #     showZipcodePopup(event$id, event$lat, event$lng)
  #   })
  # })
  # 
    
}

shinyApp(ui, server)