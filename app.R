# Load packages
library(shiny)

# Load data
data <- readRDS("m1_sub.Rds")

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("SCRVV"),
  mainPanel(
    tabsetPanel(type = "tabs", id="tabs",
                tabPanel("All Columns", value=1,
                         verbatimTextOutput("all_columns")),
                tabPanel("Summary", value=2,
                  sidebarPanel(uiOutput("sidebar_summary")),
                  verbatimTextOutput("summary")),
                tabPanel("Unique", value=3,
                  sidebarPanel(uiOutput("sidebar_unique")),
                  verbatimTextOutput("unique")))
  )
)

# Define server logic
server <- function(input, output) {
  output$all_columns <- renderPrint({
    if (input$tabs == 1){
      for (i in 1:11) {
        cat(colnames(data)[i], "\n")
      }
    }
  })
  output$sidebar_summary <- renderUI({
    if (input$tabs == 2){
      radioButtons(inputId = "column",
                   label = "Choose a column:",
                   choices = colnames(data)[1:9])
    }
  })
  output$summary <- renderPrint({
    summary(data[input$column])
  })
  output$sidebar_unique <- renderUI({
    if(input$tabs == 3){
      radioButtons(inputId = "column2",
                   label = "Choose a column:",
                   choices = colnames(data))
    }
  })
  output$unique <- renderPrint({
    unique(data[input$column2])
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)