#
# This is the user-interface definition of a Shiny web application.
#

library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("qPCR Plots"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose Excel File',
                accept=c('.xls','.xlsx')),
      tags$hr(),
      fileInput('file2', 'If needed, choose another File',
                accept=c('.xls','.xlsx')),
      tags$hr(),
      fileInput('file_metadata', 'Choose a file for experiment design',
                accept=c('.xls','.xlsx')),
      tags$hr(),
      selectInput(inputId = "gene1", label = "Select target gene:",
                  choices =  c(" "), selected = " "),
      sliderInput("range1", "Tm range to accept for target gene",min = 50, max = 90, value = c(50,90),
                  step = 0.5, width = "90%"),
      selectInput(inputId = "gene2", label = "Select reference gene:",
                  choices =  c(" "), selected = " "),
      sliderInput("range2", "Tm range to accept for reference gene",min = 50, max = 90, value = c(50,90),
                  step = 0.5, width = "90%"),
      textInput(inputId = "threshold", label = "deltaCT detection threshold", 
                value = 1e-7 ),
      submitButton("Apply Changes")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('CT',column(5, h3("Target Gene"),tableOutput('CT1')),
                 column(5, h3("Reference Gene"),tableOutput('CT2'))),
        tabPanel('deltaCT',tableOutput('deltaCT'),
                 downloadButton("download_deltaCT_table","Download Excel File")),
        tabPanel('Plots',  checkboxInput("plot_1_notch", "Apply notch", FALSE),plotOutput('plot_1'),
                 downloadButton('download_plot_1',"Download Figure"))
      )
    )
  )
)

ui