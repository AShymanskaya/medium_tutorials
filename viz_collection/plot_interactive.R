library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Interactive Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectCountry", "Select Country:", choices = unique(data$Country), selected = "Germany")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Filter data based on selection
    selected_data <- data[data$Country == input$selectCountry,]
    relative_costs <- data$`Yearly student living costs (£)` - selected_data$`Yearly student living costs (£)`
    
    # Generate plot
    ggplot(data, aes(x = reorder(Country, relative_costs), y = relative_costs, fill = relative_costs > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("green", "red")) +
      labs(title = paste("Costs Relative to", input$selectCountry), x = "Country", y = "Relative Costs (£)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)