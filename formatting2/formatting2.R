library(shiny)
library(ggplot2)
# Define UI for data upload app ----

ui <- navbarPage("How's my dog doing?",
                  tabPanel("Select files",
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
                             )#,
                            ## column(6,
                                    # Added drop-down box for time range selection -- NOT YET IMPLEMENTED. Source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
                                    ##selectInput("time", h3("Time range"),
                                               ## choices = list("Past 30 minutes" = 1,
                                                 ##              "Past hour" = 2,
                                                   ##            "Past day" = 3,
                                                     ##          "Past week" = 4,
                                                       ##        "Past month" = 5,
                                                         ##      "Past year" = 6,
                                                           ##    "All time" = 7),
                                            ##    selected = 7)
                             ##)
                           )
                           ),
                 
                  tabPanel("Pressure",
                           fluidRow(
                             column(8,
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
                  tabPanel("Strain",
                           fluidRow(
                             column(8,
                                    # Prepare plotOutput to display hover/click/etc information: source:  https://shiny.rstudio.com/articles/plot-interaction.html
                                    plotOutput("histogram2", 
                                               hover = "plot_hover2", 
                                               click = "plot_click2", 
                                               dblclick = "plot_dblclick2", 
                                               brush = "plot_brush2"), 
                                    verbatimTextOutput("info2")
                             ),
                             column(4, 
                                    h4("Summary"),
                                    #Change the color of the background and text displayed in the summary box, use font-family: 'font name'; to change font
                                    tags$style(type='text/css', "#summary2 {background-color: white; color: black;}"), 
                                    verbatimTextOutput("summary2")
                             )
                           ))
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
                  fill="blue") +
      xlab("Time in Seconds") + 
      ylab("Pressure") +
      ggtitle("Pressure in newtons over time")
    
    m +  theme(
      plot.title = element_text(color="black", size=24),
      axis.title.x = element_text(color="black", size=18),
      axis.title.y = element_text(color="black", size=18)
    )
  })  
  
  output$histogram2 <- renderPlot({
    df <- read.csv(input$file1$datapath,
                   header = TRUE,
                   sep = ",",
                   quote = '"')
    
    #added xlab, tlab, and ggtitle so I could implement theme lower down and change font size/color/face type 
    p <- ggplot(df, aes(x=df[1], y=df[2])) +
      geom_point(shape=18, color="blue")+
      geom_smooth(method=lm,  
                  linetype="dashed",
                  color="darkred", 
                  fill="blue") +
      xlab("Time in Seconds") + 
      ylab("Strain") +
      ggtitle("Strain over time")

    # Change title/axis color/formatting and display 
    p + theme(
      plot.title = element_text(color="black", size=24),
      axis.title.x = element_text(color="black", size=18),
      axis.title.y = element_text(color="black", size=18)
    )
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
  
  #Set display parameters for click information. Source: https://shiny.rstudio.com/articles/plot-interaction.html
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("X: ", round(e$x, 1), " Y: ", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Min X: ", round(e$xmin, 1), ", Max X: ", round(e$xmax, 1), 
             ", Min Y: ", round(e$ymin, 1), ", Max Y: ", round(e$ymax, 1))
    }
    
    paste0(
      "Last click: ", xy_str(input$plot_click),
      "Last double click: ", xy_str(input$plot_dblclick),
      "Hover coordinates: ", xy_str(input$plot_hover),
      "Selected area: ", xy_range_str(input$plot_brush)
    )
  })
  
  #Set display parameters for click information. Source: https://shiny.rstudio.com/articles/plot-interaction.html
  output$info2 <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("X: ", round(e$x, 1), " Y: ", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      #Labels for displaying data. Added commas and spaces for ease of reading. 
      paste0("Min X: ", round(e$xmin, 1), ", Max X: ", round(e$xmax, 1), 
             ", Min Y: ", round(e$ymin, 1), ", Max Y: ", round(e$ymax, 1))
    }
    
    paste0(
      #input$plot_click2 refers to plot_click2 that I defined up in plotOutput for histogram2, which allows the clicks on that histogram to be separate from histogram 1
      "Last click: ", xy_str(input$plot_click2),
      "Last double click: ", xy_str(input$plot_dblclick2),
      "Hover coordinates: ", xy_str(input$plot_hover2),
      "Selected area: ", xy_range_str(input$plot_brush2)
    )
  })
  
}
shinyApp(ui = ui, server = server)