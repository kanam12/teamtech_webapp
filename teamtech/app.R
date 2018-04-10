#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(UsingR)
library(shiny)
library(Hmisc)
library(corrplot)

wd <- getwd()
setwd(wd)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 h3('Output Information'),
                 h4('File entered'),
                 p("testing"),
                 url <- a("Google Homepage", href="https://www.google.com/"),
                 # p(url),
                 verbatimTextOutput("helloworld"),
                 verbatimTextOutput("ofile"),
                 h4('You selected plot type'),
                 verbatimTextOutput("oplotType"),
                 h4('You entered'),
                 verbatimTextOutput("odate"),
                 plotOutput('newHist')
                 # tableOutput('contents')
               )
             )
    ),
    tabPanel("First Type",
             pageWithSidebar(
               headerPanel('My First Plot'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    )
    
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   #print(input);
   #print(output);
  output$ofile        <- renderPrint({input$infile})
  output$oplotType    <- renderPrint({input$plotType})
  output$odate        <- renderPrint({input$date})
  output$helloworld   <- renderPrint("hello world")
  
  plotdata <- reactive({
    filestr <- input$infile
    read.csv(filestr$name)
  })
  
  output$newHist <- renderPlot({
    hist(plotdata())
  })
  #   Conditional plot selection is test in progress
      # corrdf <- cor(plotdata)
      # output$newHist <- renderPlot({
      #     corrplot(corrdf, method = "circle")
      # })
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
renderPrint("!!hello world")
shinyApp(ui = ui, server = server)

