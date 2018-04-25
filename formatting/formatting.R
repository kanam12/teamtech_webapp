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
             column(6,
                    #Input: Select a file ---- 
                    fileInput("file1", "Choose CSV File",
                              multiple = TRUE,
                              accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"))
                    ),
                    column(6,
                           selectInput("time", h3("Time range"),
                                       choices = list("Past 30 minutes" = 1,
                                                      "Past hour" = 2,
                                                      "Past day" = 3,
                                                      "Past week" = 4,
                                                      "Past month" = 5,
                                                      "Past year" = 6,
                                                      "All time" = 7),
                                       selected = 7)
                    ),
                    fluidRow(
                      column(8,
                             h4(align = "center", "Pressure"),
                             plotOutput("histogram", 
                                        hover = "plot_hover", 
                                        click = "plot_click", 
                                        dblclick = "plot_dblclick", 
                                        brush = "plot_brush"), 
                             verbatimTextOutput("info")
                             ),
                      column(4, 
                             h4("Summary"),
                             verbatimTextOutput("summary"))
                      )
                    ),
           tags$hr()
           )
    ),
  
  tags$hr(),
  
  fluidRow(
    fluidRow(
      column(8,
            h4(align = "center", "Strain"),
            plotOutput("histogram2", 
                       hover = "plot_hover2", 
                       click = "plot_click2", 
                       dblclick = "plot_dblclick2", 
                       brush = "plot_brush2"), 
            verbatimTextOutput("info2")
             ),
      column(4, 
             h4("Summary"),
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
    m <- ggplot(df, aes(x=df[1], y=df[2])) +
      geom_point(shape=18, color="blue")+
      geom_smooth(method=lm,  
                  linetype="dashed",
                  color="darkred", 
                  fill="blue")
    
    m +  labs(title = "Pressure over time", x
              = "Time in Seconds", 
              y = "Pressure in Newtons")
  })  
  
  output$histogram2 <- renderPlot({
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    p <- ggplot(df, aes(x=df[1], y=df[2])) +
      geom_point(shape=18, color="blue")+
      geom_smooth(method=lm,  
                  linetype="dashed",
                  color="darkred", 
                  fill="blue")
    
    p + labs(title = "Strain in newtons over time", 
             x = "Time in Seconds", 
             y = "Pressure in Newtons")
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
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  
  output$info2 <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click2: ", xy_str(input$plot_click),
      "dblclick2: ", xy_str(input$plot_dblclick),
      "hover2: ", xy_str(input$plot_hover),
      "brush2: ", xy_range_str(input$plot_brush)
    )
  })
  
}
shinyApp(ui = ui, server = server)