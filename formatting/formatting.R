library(shiny)
library(ggplot2)


# Define UI for data upload app ----
ui <- fluidPage(
  fluidRow(
    column(12,
           tags$hr(),
           h1("UC Berkeley Team Tech"),
           tags$hr(),
           fluidRow(
             column(12,
                    #Input: Select a file ---- 
                    fileInput("file1", "Choose CSV File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                    fluidRow(
                      column(8,
                             h4(align = "center", "Pressure"),
                             plotOutput("histogram")
                             ),
                      column(4, 
                             h4("summary"),
                             verbatimTextOutput("summary"))
                      )
                    )
             )
           )
    ),
  
  tags$hr(),
  
  fluidRow(
    fluidRow(
      column(8,
            h4(align = "center", "Strain"),
            plotOutput("histogram2")
             ),
      column(4, 
             h4("summary2"),
             verbatimTextOutput("summary2")
             )
      )
    )
  )
  

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$histogram <- renderPlot({
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    ggplot(df, aes(x=x, y=y)) + geom_point()
  })  
  output$histogram2 <- renderPlot({
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    ggplot(df, aes(x=x, y=y)) + geom_point()
  })
  
  output$summary <- renderPrint({
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    summary(df)
  })  
  output$summary2 <- renderPrint({
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    summary(df)
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    
    
    return(head(df))
    
    
  })  
  output$contents2 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    
    
    return(head(df))
    
    
  })
  
}
shinyApp(ui = ui, server = server)