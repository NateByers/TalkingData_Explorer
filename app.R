library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(sunburstR)
load("data/shiny_data.rda")

ui <- dashboardPage(
  dashboardHeader(title = "TalkingData Explorer"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Phone Brand", tabName = "phone", icon = icon("mobile"))
  )
  ),
  dashboardBody(
    tabItems(
      # First tab content
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
      
      # Second tab content
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
    
    
}

shinyApp(ui, server)