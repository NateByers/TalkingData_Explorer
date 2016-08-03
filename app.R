library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(sunburstR)
library(leaflet)
load("data/prepped_data.rda")



###### User Interface #####################################################3

ui <- dashboardPage(
  dashboardHeader(title = "TalkingData Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Phone Brand", tabName = "phone", icon = icon("mobile"))
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
                    textOutput("n"),
                    plotOutput("age_histogram", height = 250),
                    plotOutput("gender_bar", height = 200)
                    )
              )
      ),
      
      tabItem(tabName = "phone",
                     fluidRow(
                       box(width = 6, sunburstOutput("sunburst", height = 300)),
                       box(plotlyOutput("brands_bar"), width = 6, height =300,
                           title = "Top 25 brands")
                     ),
              fluidRow(
                
                box(width = 6, title = "Control Panel",
                    column(width = 6,
                           checkboxGroupInput("region", "Select city", 
                                       choices = c("Beijing", "Chengdu",
                                                   "Hong Kong", "Shanghai"),
                                       selected = c("Beijing", "Chengdu",
                                                    "Hong Kong", "Shanghai"))
                      
                    ),
                    column(width = 6,
                           checkboxGroupInput("time_of_day", "Time of the day",
                                              choices = c("morning", "midday", 
                                                          "evening", "night"),
                                              selected = c("morning", "midday", 
                                                           "evening", "night"))
                           )
                    ),
                box(width = 6, title = "Demographics",
                    column(width = 6,
                           plotOutput("phone_age_histogram", height  = 200)
                    ),
                    column(width = 6,
                           plotOutput("phone_gender_bar", height = 200)
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
             longitude >= lngRng[1] & longitude <= lngRng[2]) %>%
      select(device_id, gender, age) %>%
      distinct()
  })
  
  output$n <- renderText({
    paste("Total number of people:", dim(eventsInBounds())[1])
  })


  output$age_histogram <- renderPlot({
    
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

  ##### Phone Brands ###############################################
  getBrandData <- reactive({
    d <- full_data %>%
      filter(region %in% input$region, time %in% input$time_of_day) %>%
      select(device_id, phone_brand, device_model, gender, age) %>%
      distinct()
    
    d
  })
  
  
  output$brands_bar <- renderPlotly({
    d <- getBrandData() %>%
      group_by(phone_brand) %>%
      summarize(total = n()) %>%
      arrange(desc(total)) %>%
      as.data.frame()
    
    gg <- ggplot(d[1:25, ], aes(reorder(phone_brand, -total), total,
                                fill = phone_brand)) + 
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
  
  output$phone_age_histogram <- renderPlot({
    
    hist(getBrandData()$age,
         main = "",
         xlab = "Age",
         col = '#00DD00',
         border = 'white')
  })
  
  output$phone_gender_bar <- renderPlot({
    counts <- table(getBrandData()$gender)
    
    barplot(counts, xlab = "Gender", col = 'dodgerblue', border = 'white')
  })
    
}

shinyApp(ui, server)