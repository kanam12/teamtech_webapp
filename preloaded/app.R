library(shiny)
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny Text"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # plotOutput("print"),
      
      plotOutput("histogram"),
      
      plotOutput("mpgPlot"),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = read_csv(file = "../test.csv"),
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$print <- renderPlot({
    dataset <- datasetInput()
    x <- strtoi(dataset[1])
    y <- strtoi(dataset[2])
    qplot(y,
          geom="histogram",
          binwidth = 2,  
          main = "Histogram for y Values", 
          xlab = "y value",  
          fill=I("blue"), 
          col=I("red"), 
          alpha=I(.2),
          xlim=c(0,150))
    # hist(dataset$x, 
         # main = 'Pls Work' )
    # typeof(dataset)
    # dataset[2]
    # renderPrint("hello")
    # renderPrint(dataset)
    # renderPrint("world")
    # renderPrint(y)
    # hist(x)
    # plot(x = x, y = y, xlim = c(0, 20), ylim = c(0, 20))
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
    # casted <- strtoi(datasetInput())
    # renderPrint("temp")
    # renderPrint(casted)
  })
  
  output$mpgPlot <- renderPlot({
    # ggplot(datasetInput(), aes(x=x, y=y)) + geom_point()
    ggplot(datasetInput(), aes(x=datasetInput()[1], y=datasetInput()[2])) + 
      geom_point(shape=18, color="blue")+
      geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue")
  })
  
  output$histogram <- renderPlot({
    qplot(datasetInput()[2],
          geom="histogram",
          binwidth = 2,  
          main = "Histogram for Pressure", 
          xlab = "Pressure",  
          fill=I("blue"), 
          col=I("red"), 
          alpha=I(.2),
          xlim=c(0,140))
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)