library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(sunburstR)
library(leaflet)
load("data/prepped_data.rda")

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
                    leafletOutput("map")
                    ),
                box(width = 4, offset = 1,
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
      addCircleMarkers()
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
         main = "Age Histogram (visible cities)",
         xlab = "Age",
         col = '#00DD00',
         border = 'white')
  })
  
  output$gender_bar <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(eventsInBounds()) == 0)
      return(NULL)
    
    counts <- table(eventsInBounds()$gender)
    barplot(counts, main="Car Distribution", 
            xlab="Number of Gears")
    
    barplot(counts,
         main = "Gender (visible cities)",
         col = 'blue',
         border = 'white')
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