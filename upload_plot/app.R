library(shiny)
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("graph"),
      # Output: Data file ----
      tableOutput("contents"),
      verbatimTextOutput("summary"),
      plotOutput("histogram")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$graph <- renderPlot({
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    # ggplot(df, aes(x=x, y=y)) + geom_point()
    ggplot(df, aes(x=df[1], y=df[2])) + 
      geom_point(shape=18, color="blue")+
      geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue")
  })
  
  output$histogram <- renderPlot({
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    qplot(df[2],
          geom="histogram",
          binwidth = 2,  
          main = "Histogram for Pressure", 
          xlab = "Pressure",  
          fill=I("blue"), 
          col=I("red"), 
          alpha=I(.2),
          xlim=c(0,30))
  })
  
  output$summary <- renderPrint({
    df <- read.csv(input$file1$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
    summary(df)
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)