library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(sunburstR)
library(leaflet)
load("data/prepped_data.rda")
hist_color = "springgreen2"


###### User Interface #####################################################3

ui <- dashboardPage(
  dashboardHeader(title = "TalkingData Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe"))
      ,
      menuItem("Phone Brand", tabName = "phone", icon = icon("mobile"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12,
                    selectInput("size", "Size of Circle by", 
                                choices = c("Events", "Devices"))       
                )
              ),
              fluidRow(
                box(width = 7, 
                    leafletOutput("map", height = 550)
                    )
                ,
                box(width = 4, offset = 1,
                    title = "Demographics (in view)",
                    p("Below are plots of age and gender based on the data for events
                      that are within the boundary of the map."),
                    textOutput("n"),
                    plotlyOutput("age_histogram", height = 250),
                    plotlyOutput("gender_bar", height = 200)
                    )
              )
      )
      ,

      tabItem(tabName = "phone",
                     fluidRow(
                       box(width = 6, sunburstOutput("sunburst", height = 325)),
                       box(plotlyOutput("brands_bar", height =325), width = 6,
                           title = "Top 25 brands")
                     ),
              fluidRow(

                box(width = 6, title = "Select a city",
                    p("Subset the data by selecting one or more cities."),
                    textOutput("n_phone"),
                    checkboxGroupInput("region", "City",
                                       choices = c("Beijing", "Chengdu",
                                                   "Hong Kong", "Shanghai"),
                                       selected = c("Beijing", "Chengdu",
                                                    "Hong Kong", "Shanghai"),
                                       inline = TRUE)
                    ),
                box(width = 6, title = "Demographics",
                    column(width = 6,
                           plotlyOutput("phone_age_histogram", height  = 200)
                    ),
                    column(width = 6,
                           plotlyOutput("phone_gender_bar", height = 200)
                           )
                           )

              )


      )
    )
  )
)


############ Server Logic ######################################

server <- function(input, output) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 112.80, lat = 31.85, zoom = 3) 
  })
  
  observe({
    sizeBy <- input$size
    print(sizeBy)
    
    if (sizeBy == "Events") {
      radius <- map_data$radius_events
      
    } else {
      radius <- map_data$radius_devices
    }
    print(max(radius))
    
    leafletProxy("map", data = map_data) %>%
      clearShapes() %>%
      addCircleMarkers(~longitude, ~latitude, radius=radius, layerId=~cells,
                       stroke=FALSE, fillOpacity=0.4) 
  })
  
  # Show a popup at the given location
  showCellPopup <- function(cell, lat, lng) {
    selected_cell <- map_data[map_data$cells == cell,]
    content <- paste0(selected_cell$events, " events on<br/>",
                      selected_cell$devices, " devices")
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = cell)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    print(event)
    if (is.null(event))
      return()
    
    isolate({
      showCellPopup(event$id, event$lat, event$lng)
    })
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
             longitude >= lngRng[1] & longitude <= lngRng[2]) %>%
      select(device_id, gender, age) %>%
      distinct()
  })

  output$n <- renderText({
    paste("Total number of people:", dim(eventsInBounds())[1])
  })

  
  output$age_histogram <- renderPlotly({
    
    if (nrow(eventsInBounds()) == 0)
      return(NULL)
    
    plot_ly(x = eventsInBounds()$age, autobinx = F, type = "histogram",
            xbins = list(start = 0, end = 100, size = 5) ,
            marker = list(color = toRGB(hist_color))) %>%
      layout(xaxis = list(title = "Age"))
  })

  
  output$gender_bar <- renderPlotly({
    if (nrow(eventsInBounds()) == 0)
      return(NULL)
    
    counts <- table(eventsInBounds()$gender)
    
    p <- plot_ly(
      x = names(counts),
      y = counts,
      name = "Gender",
      type = "bar",
      marker = list(color = toRGB("dodgerblue"))) %>%
      layout(xaxis = list(title = "Gender"))
    p
  })

  ##### Phone Brands ###############################################
  getBrandData <- reactive({
    d <- full_data %>%
      filter(region %in% input$region) %>%
      select(device_id, phone_brand, device_model, gender, age) %>%
      distinct()

    d
  })
  
  output$n_phone <- renderText({
    paste("Total number of devices:", dim(getBrandData())[1])
  })


  output$brands_bar <- renderPlotly({
    d <- getBrandData() %>%
      group_by(phone_brand) %>%
      summarize(total = n()) %>%
      arrange(desc(total)) %>%
      as.data.frame()

    gg <- ggplot(d[1:25, ], aes(phone_brand, total, fill = phone_brand)) +
      geom_bar(stat = "identity") +
      theme(legend.position = "none", axis.text.x=element_text(angle=90),
            axis.title.x=element_blank())
    # Convert the ggplot to a plotly
    p <- ggplotly(gg)
    p
  })

  getPhoneDataSunburst <- reactive({
    d <- getBrandData() %>%
      mutate(brand = gsub(" ", "_", phone_brand),
             model = gsub(" ", "_", device_model),
             sunburst = paste(brand, model, "end", sep = "-")) %>%
      group_by(sunburst) %>%
      summarize(total = n()) %>%
      arrange(desc(total)) %>%
      select(sunburst, total) %>%
      rename(V1 = sunburst, V2 = total) %>%
      as.data.frame()

    d
  })

  output$sunburst <- renderSunburst({
    d <- getPhoneDataSunburst()
    sunburst(d)
  })

  output$phone_age_histogram <- renderPlotly({
    
    plot_ly(x = getBrandData()$age, autobinx = F, type = "histogram",
            xbins = list(start = 0, end = 100, size = 5) ,
            marker = list(color = toRGB(hist_color))) %>%
      layout(xaxis = list(title = "Age"))
  })

  output$phone_gender_bar <- renderPlotly({
  
    counts <- table(getBrandData()$gender)
    
    p <- plot_ly(
      x = names(counts),
      y = counts,
      name = "Gender",
      type = "bar",
      marker = list(color = toRGB("dodgerblue"))) %>%
      layout(xaxis = list(title = "Gender"))
    p
  })

}

shinyApp(ui, server)