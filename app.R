library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
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
                       box(width = NULL, imageOutput("map_overview"))
                       ),
                column(width = 4, offset = 1,
                       box(width = NULL, plotOutput("plot1", height = 250)),
                       
                       box(width = NULL, plotOutput("plot2", height = 250))
                       )
              ),
              fluidRow(
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "phone",
              fluidRow(
                box(width = 12, plotlyOutput("plot3"))
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
    filename <- normalizePath("data/map_overview.png")
    
    # Return a list containing the filename and alt text
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  getPhoneData <- reactive({
    d <- phone_brand_device_model %>%
      group_by(device_model) %>%
      summarize(total = n()) %>%
      arrange(desc(total)) %>%
      as.data.frame()
    
    d
  })
  
  output$plot3 <- renderPlotly({
    d <- getPhoneData()[1:50, ]
    orderd_labels <- d$device_model
    gg <- ggplot(d, aes(
      
      reorder(device_model, -total)
      
      , total, fill = device_model)) + 
      geom_bar(stat = "identity") + 
      theme(legend.position = "none", axis.text.x=element_text(angle=90))
    # Convert the ggplot to a plotly
    p <- ggplotly(gg)
    p
  })
    
    
}

shinyApp(ui, server)