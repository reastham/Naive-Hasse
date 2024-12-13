library(shiny)
library(tidyverse)
library(markdown)
library(DT)
  
  # Define the Shiny app
  shinyApp(
    ui = fluidPage(
      titlePanel("Number of Points on an Elliptic Curve Over a Finite Field"),
      sidebarLayout(
        sidebarPanel(
          numericInput("a", "Parameter a:", value = 1),
          numericInput("b", "Parameter b:", value = 1),
          numericInput("p", "Finite Field p:", value = 1, min = 0),
          actionButton("update", "Update")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Introduction", includeMarkdown("Text.Rmd")),
            tabPanel("Results",
                     DT::dataTableOutput("resultsTable"),
                     plotOutput("scatterPlot")
            )
          )
        )
      )
    ),
    
    server = function(input, output, session) {
      # Reactive function to source the external script
      calculateData <- eventReactive(input$update, {
        # Pass inputs to the external script
        a <<- input$a
        b <<- input$b
        p <<- input$p
        
        # Source the script to execute it
        source("Naive-Hasse.R", local = TRUE)
        
        # Return the table and plot created in Naive.R
        list(table = table, plot = plot)
      })
      
      # Render the table output
      output$resultsTable <- DT::renderDataTable({
        data <- calculateData()$table  # Retrieve the table
        t(data) %>% as.data.frame()  # Transpose and convert to data frame
      }, options = list(scrollX = TRUE))
      
      # Render the plot output
      output$scatterPlot <- renderPlot({
        calculateData()$plot
      })
    }
  )
)
