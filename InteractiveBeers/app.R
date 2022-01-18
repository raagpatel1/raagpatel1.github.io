#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Interactive Graphs"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          # Input files
          fileInput("file1", "Choose CSV File",
                    multiple = F,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
          # Choose IBU graph type
          radioButtons("ibu", label = h3("IBU Graph Type"),
                            choices = list("Histogram" = 1, "Boxplot" = 2),
                            selected = 2),

          # Choose ABV graph type
          radioButtons("abv", label = h3("ABV Graph Type"),
                       choices = list("Histogram" = 1, "Boxplot" = 2),
                       selected = 2),
          
          # Show SLR
          actionButton("action", label = "Show Simple Linear Regression Line?")
          
        ),

        # Show Plots
        mainPanel(
          plotOutput("IBU"),
          plotOutput("ABV"),
          plotOutput("Scatter")
        )
    )
)


# Define server logic 
server <- (function(input, output) {

  # SCATTER
  output$Scatter <- renderPlot({
    
    x <- read.csv(input$file1$datapath,
                  header = TRUE)
    
    x %>% drop_na()
    
    if((input$action %% 2) == 0){
      
      x %>% ggplot(aes(x = IBU, y = ABV, color = 'red')) + geom_point() + 
        labs(x = "IBU", y = "ABV", title = "IBU vs ABV")
      
    } else {
      
      x %>% ggplot(aes(x = IBU, y = ABV, color = 'red')) + geom_point() + 
        geom_smooth(method=lm) + 
        labs(x = "IBU", y = "ABV", title = "IBU vs ABV")
      
    }
    
  })
  
  # IBU
  output$IBU <- renderPlot({
    
    x <- read.csv(input$file1$datapath,
                  header = TRUE)
    
    x %>% select(IBU) %>% drop_na()
    
    if(input$ibu == 1){
      
      x %>% ggplot(aes(x = IBU)) + geom_col() + 
        labs(x = "IBU", y = "Count", title = "IBU Histogram")
      
    }
    
    if(input$ibu == 2){
      
      x %>% ggplot(aes(x = IBU)) + geom_boxplot() + 
        labs(x = "IBU", y = "Count", title = "IBU BoxPlot")
      
    }
    
  })
  
  # ABV
  output$ABV <- renderPlot({
    
    x <- read.csv(input$file1$datapath,
                  header = TRUE)
    
    x %>% select(ABV) %>% drop_na()
    
    if(input$abv == 1){
      
      x %>% ggplot(aes(x = ABV)) + geom_bar() + 
        labs(x = "ABV", y = "Count", title = "ABV Histogram")
      
    }
    
    if(input$abv == 2){
      
      x %>% ggplot(aes(x = ABV)) + geom_boxplot() + 
        labs(x = "ABV", y = "Count", title = "ABV BoxPlot")
      
    }
    
  })

  
})

# Run the application 
shinyApp(ui = ui, server = server)
