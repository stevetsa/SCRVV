# Load packages
library(shiny)
library(DT)

# Load data
data <- readRDS("m1_sub.Rds")


# Define UI
ui <- fluidPage(
  # App title
  # titlePanel("SCRVV"),
  tags$h1("SCRVV", align="center"),
  # Banner links to our github repo
  tags$div(
    HTML(paste('<a href="https://github.com/NCBI-Hackathons/SCRVV/">',
                 '<img style="position: absolute; top: 0; right: 0; border: 0;"',
                   'src="https://camo.githubusercontent.com/365986a132ccd6a44c23a9169022c0b5c890c387/',
                      '68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e73',
                      '2f666f726b6d655f72696768745f7265645f6161303030302e706e67" ',
                   'alt="Fork me on GitHub" data-canonical-src="https://s3.amazonaws.com/github/ribbons/forkme_right_red_aa0000.png">',
               '</a>',sep=""))
  ),
  mainPanel(
    tabsetPanel(type = "tabs", id="tabs",
                tabPanel("Summary", value=1,
                         verbatimTextOutput("all_columns")),
                tabPanel("Basic Statistics", value=2,
                         sidebarPanel(uiOutput("sidebar_stat")),
                         verbatimTextOutput("stat")),
                tabPanel("Raw Data", value=3,
                         htmlOutput("raw")),  
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
  output$sidebar_stat <- renderUI({
    if (input$tabs == 2){
      radioButtons(inputId = "column",
                   label = "Choose a column:",
                   choices = colnames(data))
    }
  })
  output$stat <- renderPrint({
    if (input$column == "Homozygous.SNP") {
      
    } else if (input$column == "Heterozygous.SNP") {
      
    } else {
      print(summary(data[input$column]))
      print(unique(data[input$column]))
    }
  })
  output$raw <- renderUI({
    HTML("<h3>Nothing yet</h3>")
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

      output$ex1 <- DT::renderDataTable(DT::datatable(data, filter = 'top',escape = FALSE, options = list(pageLength = 10, scrollX='400px',autoWidth = TRUE)))
    }
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)