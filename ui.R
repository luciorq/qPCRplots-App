#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
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
      # radioButtons('design', 'Design',
      #              c(Duplicate='Duplicate',
      #                Triplicate='Triplicate'),
      #              'Duplicate'),
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
      selectInput("groups", "Number of groups",
                  list("1", "2", "3", "4", "5","6","7","8","9","10"), selected = "1"),
      conditionalPanel(
        condition = "input.groups != '1'",
        uiOutput("group_sliders")
      ),
      downloadButton('downloadPlot')
    ),
    mainPanel(
      tabsetPanel(
        #column(6,plotOutput(outputId="plotgraph1", width="300px",height="300px")),  
        #column(6,plotOutput(outputId="plotgraph2", width="300px",height="300px"))
        tabPanel('CT',column(5, h3("Target Gene"),tableOutput('CT1')),
                 column(5, h3("Reference Gene"),tableOutput('CT2'))),
        tabPanel('deltaCT',tableOutput('deltaCT')),
        tabPanel('Plots', plotOutput('plot1'))
      )
    )
  )
)

ui