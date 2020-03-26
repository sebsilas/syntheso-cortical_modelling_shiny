# implemented by Seb Silas, March 2020. Goldsmiths University. ssila010@gold.ac.uk

require(matlab)
require(ramify)
library(shiny)

# plots
library(ggplot2)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Perceptron on Letter Recognition problem"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("lr",
                  "Learning Rate:",
                  min = 0,
                  max = 1,
                  step = 0.02,
                  value = 0.7),
      
      sliderInput("no_training_steps",
                  "Training Steps",
                  min = 0,
                  max = 50,
                  step = 1,
                  value = 20)
    ),
    
    # Show a plot of the generated distribution
    mainPanel( 
      plotOutput("plot"),
      textOutput("final_error") 
    )
  )
) # End Fluid Page


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # training vectors 
  pattern1 <- read.table("pattern1", header = FALSE, sep = "")
  
  nIn<-12*13
  nOut<-26
  rIn<-reshape(t(pattern1), nIn, 26) 
  rDes <- diag(x = 1, 26, 26)
  
  output$plot <- renderPlot({
    
    
    
    wOut<-rand(nOut,nIn)-0.5 
    no_training_steps <- input$no_training_steps
    learning_rate <- input$lr
    error <- matrix()
    
    # Updating and training network 
    for (training_step in 1:no_training_steps) {
      # test all pattern
      rOut <- as.numeric(matrix((wOut%*%rIn)>0.5))
      distH <- matlab::sum(matlab::sum((rDes-rOut)^2))/26
      print(distH)
      error[training_step] <- distH
      # training with learning_rate rule
      wOut<-wOut+learning_rate*(rDes-rOut)%*%t(rIn) 
    }
    
    # base R
    # plot(seq(0, no_training_steps-1),error)
    
    data <- data.frame("training_steps"=seq(0, no_training_steps-1),"error"=error)
    
    data %>%
      ggplot(aes(x=training_steps, y=error)) +
      geom_line() +
      geom_point() +
      ggtitle("Perceptron Learning") +
      annotate(geom="text", size=6, x=no_training_steps, y=(as.numeric(error[no_training_steps])+0.02), 
               label=sprintf("Error: %.2f", error[no_training_steps]))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
