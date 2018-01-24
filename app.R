# Load packages
library(shiny)
library(DT)

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
                  verbatimTextOutput("unique")),
                tabPanel("Raw", value=4,
                  verbatimTextOutput("Raw"),
                  DT::dataTableOutput('ex1')
                ))
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
  output$Raw <- renderPrint({
    if(input$tabs == 4){
      for (i in 1:length(data$Heterozygous.SNP)){
        if(length(data$Heterozygous.SNP[[i]][[1]]) == 0){
           data$Heterozygous.SNP[[i]][[1]] = 'NA'
        } else if(length(data$Heterozygous.SNP[[i]][[1]]) != 0) {
      for (j in 1:length(data$Heterozygous.SNP[[i]][[1]])){
        data$Heterozygous.SNP[[i]][[1]][j] = paste0('<a href=\"https://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=',data$Heterozygous.SNP[[i]][[1]][j],'\">',data$Heterozygous.SNP[[i]][[1]][j],'</a>')
      }
        }
          if(length(data$Homozygous.SNP[[i]][[1]]) == 0){
            data$Homozygous.SNP[[i]][[1]] = 'NA'
          } else if(length(data$Homozygous.SNP[[i]][[1]]) != 0) {
            for (j in 1:length(data$Homozygous.SNP[[i]][[1]])){
              data$Homozygous.SNP[[i]][[1]][j] = paste0('<a href=\"https://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=',data$Homozygous.SNP[[i]][[1]][j],'\">',data$Homozygous.SNP[[i]][[1]][j],'</a>')
            }
          }

          data$Heterozygous.SNP[i] = paste(unlist(data$Heterozygous.SNP[i][1]), collapse=',')
          data$Homozygous.SNP[i] = paste(unlist(data$Homozygous.SNP[i][1]), collapse=',')
          
}
      data$Heterozygous.SNP = unlist(data$Heterozygous.SNP)
      data$Homozygous.SNP = unlist(data$Homozygous.SNP)

      output$ex1 <- DT::renderDataTable(DT::datatable(data, escape = FALSE, options = list(pageLength = 10)))
    }
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)