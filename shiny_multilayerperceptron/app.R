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
    titlePanel("Multilayer Perceptron on XOR problem"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("N_h",
                        "Number of hidden neurons:",
                        min = 1,
                        max = 100,
                        value = 2),
        
            sliderInput("lr",
                        "Learning Rate:",
                        min = 0,
                        max = 1,
                        step = 0.02,
                        value = 0.7),
        
            sliderInput("training_steps",
                        "Training Steps",
                        min = 0,
                        max = 50000,
                        step = 10,
                        value = 10000)
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

    output$plot <- renderPlot({
      
       # Numerical Integration with the Euler method
      
      N_i <- 2 # no neurons in
      N_o <- 1 # no neurons out
      N_h <- input$N_h # n hidden
      
      d <- c()
      
      lr <- input$lr # learning rate
      training_steps <- input$training_steps
      
      w_h <- rand(N_h,N_i)-0.5 # randomly initialise hidden weights
      w_o <- rand(N_o,N_h)-0.5 # randomly initialise output weights
      
      # training vectors (XOR)
      r_i <- matrix(data=c(0,1,0,1,0,0,1,1),ncol=4, nrow=2, byrow = TRUE)
      r_d <- matrix(data=c(0,1,1,0),ncol=4,nrow=1)
      
      # Updating and training network with sigmoid activation function
      for (sweep in 1:training_steps) {
        # training randomly on one pattern
        i <- sample(1:4,1)
        
        r_h <- 1./(1+exp(-w_h%*%r_i[,i]))
        r_o <- 1./(1+exp(-w_o%*%r_h))
        d_o <- (r_o*(1-r_o))*(r_d[,i]-r_o)
        d_h <- (r_h*(1-r_h))*(t(w_o)%*%d_o)
        w_o <- w_o + lr %*% t( r_h %*% t(d_o) )
        w_h <- w_h + lr * t( r_i[,i] %*% t(d_h) )
        
        # test all pattern
        r_o_test <- 1./(1+exp(-w_o %*% (1./(1+exp(-w_h %*% r_i)))))
        d[sweep] <- 0.5*base::sum((r_o_test-r_d)^2)
      }
      
      
      # plot
      data <- data.frame("x" = c(1:length(d)), "y" = as.vector(d))
      
      
      ggplot(data, aes(x=x, y=y)) +
        geom_line() +
        labs(x = "Training Steps", y = "Training Error")  +
        annotate(geom="text", size=6, x=training_steps, y=(as.numeric(d[training_steps])+0.02), 
                 label=sprintf("Error: %.2f", d[training_steps]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
