library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(sunburstR)
library(leaflet)
load("data/shiny_data.rda")

ui <- dashboardPage(
  dashboardHeader(title = "TalkingData Explorer"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Geography", tabName = "geography", icon = icon("globe")),
    menuItem("Phone Brand", tabName = "phone", icon = icon("mobile"))
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                column(width = 7,
                       imageOutput("map_overview")
                       ),
                column(width = 4, offset = 1,
                       box(width = NULL, plotOutput("plot1", height = 250)),
                       
                       box(width = NULL, plotOutput("plot2", height = 250))
                       )
              )
      ),
      
      tabItem(tabName = "geography",
              column(width = 8,
                     fluidRow(
                       box(width = NULL, leafletOutput("leaflet"))
                       
                     ),
                     fluidRow(
                       box(width = 6, plotOutput("geography_gender", height = 250)),
                       
                       box(width = 6, plotOutput("geography_age", height = 250))
                     )
                     ),
              column(width = 3, offset = 1,
                     box(width = NULL, 
                         dateInput("date", label = h3("Date input"), 
                                   value = "2016-05-01", min = "2016-05-01",
                                   max = "2016-05-08"),
                         radioButtons("time", label = h3("Part of the day"),
                                      choices = c("morning", "midday", 
                                                  "evening", "night"), 
                                      selected = "morning")
                     )
              )
              ),
      
      tabItem(tabName = "phone",
              fluidRow(
                box(width = 12, plotlyOutput("plot3"))
              ),
              fluidRow(
                box(width = 12, sunburstOutput("sunburst"))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    ggplot(gender_age_train, aes(gender, fill = gender)) + geom_bar()
  })
  
  output$plot2 <- renderPlot({
    ggplot(gender_age_train, aes(age)) + geom_histogram()
  })
  
  output$map_overview <- renderImage({
    filename <- normalizePath("data/map_overview3.png")
    
    # Return a list containing the filename and alt text
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  getPhoneDataBar <- reactive({
    d <- phone_brand_device_model %>%
      group_by(phone_brand) %>%
      summarize(total = n()) %>%
      arrange(desc(total)) %>%
      as.data.frame()
    
    d
  })
  
  getPhoneDataSunburst <- reactive({
    d <- phone_brand_device_model %>%
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
  
  getGeographyData <- reactive({
    d <- events %>%
      filter(date == input$date, time == input$time,
             longitude != 0, latitude != 0,
             longitude > 73.227924, latitude > 18.192231)
    d
  })
  
  
  
  output$plot3 <- renderPlotly({
    d <- getPhoneDataBar()
    
    gg <- ggplot(d[1:50, ], aes(
      
      reorder(phone_brand, -total)
      
      , total, fill = phone_brand)) + 
      geom_bar(stat = "identity") + 
      theme(legend.position = "none", axis.text.x=element_text(angle=90))
    # Convert the ggplot to a plotly
    p <- ggplotly(gg)
    p
  })
  
  output$sunburst <- renderSunburst({
    d <- getPhoneDataSunburst()
    sunburst(d)
  })
  
  output$leaflet <- renderLeaflet({
    d <- getGeographyData()
    leaflet(d) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions()
    )
  })
  
  geographyGenderAge <- reactive({
    semi_join(gender_age_train, getGeographyData(), by = "device_id") 
  })
  
  output$geography_gender <- renderPlot({
    d <- geographyGenderAge()
    ggplot(d, aes(gender, fill = gender)) + geom_bar()
  })
  
  output$geography_age <- renderPlot({
    d <- geographyGenderAge()
    ggplot(d, aes(age)) + geom_histogram()
  })
    
    
}

shinyApp(ui, server)