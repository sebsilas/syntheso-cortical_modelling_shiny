# implemented by Seb Silas, March 2020. Goldsmiths University. ssila010@gold.ac.uk

library(shiny)
library(matlab)
library(ramify)
library(ggplot2)
library(dplyr)
library(patchwork) 
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("3IFs"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("tau",
                  "time constant [ms]:",
                  min = 0,
                  max = 20,
                  value = 10,
                  step = 1,
      ),
      
      sliderInput("E_L",
                  "resting potential [mV]",
                  min = -80,
                  max = 0,
                  value = -65,
                  step = 1,
      ),
      
      sliderInput("theta",
                  "theta (firing threshold [mV])",
                  min = -80,
                  max = 0,
                  value = -55,
                  step = 1,
      ),
      
      sliderInput("RI_ext",
                  "constant external input [mA/Ohm]",
                  min = 0,
                  max = 80,
                  value = 12,
                  step = 1,
      ),
      
      sliderInput("w21",
                  "weight from 2 to 1",
                  min = -100,
                  max = 100,
                  value = -90,
                  step = 2,
      ),
      
      sliderInput("w31",
                  "weight from 3 to 1",
                  min = -100,
                  max = 100,
                  value = 30,
                  step = 2,
      ),
      
      sliderInput("w12",
                  "weight from 1 to 2",
                  min = -100,
                  max = 100,
                  value = -90,
                  step = 2,
      ),
      
      sliderInput("w32",
                  "weight from 3 to 2",
                  min = -100,
                  max = 100,
                  value = 20,
                  step = 2,
      ),
      
      sliderInput("w13",
                  "weight from 3 to 2",
                  min = -100,
                  max = 100,
                  value = 80,
                  step = 2,
      ),
      
      sliderInput("w23",
                  "weight from 3 to 2",
                  min = -100,
                  max = 100,
                  value = 80,
                  step = 2,
      ),
      
      sliderInput("t",
                  "t:",
                  min = 0,
                  max = 1000,
                  value = 1000,
                  step = 1,
      )
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot", height = "1000px"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Numerical Integration with the Euler method
  
  
  calcNeur <- function() {
    

    tau <- as.numeric(input$tau)
    E_L <- as.numeric(input$E_L)
    theta <- as.numeric(input$theta)
    RI_ext <- as.numeric(input$RI_ext)
    
    v1 <- E_L # voltage of neuron 1
    v2 <- E_L # voltage of neuron 2
    v3 <- E_L # voltage of neuron 3
    
    w21  <- as.numeric(input$w21)
    w31  <- as.numeric(input$w31)
    w12  <- as.numeric(input$w12)
    w32  <- as.numeric(input$w32)
    w13  <- as.numeric(input$w13)
    w23  <- as.numeric(input$w23)
      
    s1  <- 0
    s2  <- 0
    s3  <- 0
    
    
    #t <- c(2:input$t) # control evolution across time
    
    t_step <- 0
    dt <- 0.1
    
    v1_rec <- NULL
    t1_rec <- NULL
    s1_rec <- NULL
    v2_rec <- NULL
    t2_rec <- NULL
    s2_rec <- NULL
    v3_rec <- NULL
    t3_rec <- NULL
    s3_rec <- NULL
    
    for (t in seq(from = 0, to = 100, by = dt)) {
      
      t_step <- t_step+1
      
      t1  <-  t
      s1 <- v1>theta
      v1 <- s1*E_L+ (1-s1)*(v1-dt/tau*((v1-E_L)-RI_ext-w21*s2-w31*s3))
      v1_rec[t_step] <- v1
      t1_rec[t_step] <- t1
      s1_rec[t_step] <- s1
      
      t2  <-  t
      s2 <- v2>theta
      v2 <- s2*E_L+ (1-s2)*(v2-dt/tau*((v2-E_L)-RI_ext-w12*s1-w32*s3))
      v2_rec[t_step] <- v2
      t2_rec[t_step] <- t2
      s2_rec[t_step] <- s2
      
      t3 <- t
      s3 <- v3>theta
      v3 <- s3*E_L+ (1-s3)*(v3-dt/tau*((v3-E_L)-RI_ext-w13*s1-w23*s2))
      v3_rec[t_step] <- v3
      t3_rec[t_step] <- t3
      s3_rec[t_step] <- s3     
      
    }
    
    
    # to numeric
    s1_rec <- as.numeric(s1_rec)
    s2_rec <- as.numeric(s2_rec)
    s3_rec <- as.numeric(s3_rec)
    
    
    # https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
    
    # 1
    
    create.IF.plots <- function(t_values, v_values, s_values) {
      
      data <-  data.frame("x" = t_values, "y" = v_values)
      data.spikes <- data.frame("x" = t_values, "y" = s_values)
      
      # single graphs
      
      # graph1.1 <- ggplot(data, aes(x=x, y=y)) +
      # geom_line() +
      # labs(x = "Time [ms]", y = "v [mV]")
      # 
      # graph1.2 <- ggplot(data.spikes, aes(x=x, y=y)) +
      # geom_point() +
      # labs(x = "Time [ms]", y = "Spikes")
      
      # side-by-side graphs
      
      # graph1.1 + graph1.2
      
      
      # combined graphs
      
      # create a scaled version of the y variable such that the y value of the spike is the top of the spike
      data.spikes.y.tran <- data.spikes$y
      data.spikes.y.tran[data.spikes.y.tran == 1] <- max(data$y)
      data.spikes.y.tran[data.spikes.y.tran == 0] <- NA
      
      
      data_c <- cbind(data, as.data.frame(data.spikes.y.tran))
      
      graph <- ggplot(data_c) +
        geom_hline(xintercept = 0, yintercept = theta,linetype="dotted", color = "orange", size=0.5) +
        geom_line(aes(x=x, y=y)) +
        geom_point(aes(x=x, y=data.spikes.y.tran, colour = "spike")) +
        labs(x = "Time [ms]", y="v [mV]")
      
      return(graph)
      
    }
    
    neuron.1 <- create.IF.plots(t1_rec, v1_rec, s1_rec)
    neuron.2 <- create.IF.plots(t2_rec, v2_rec, s2_rec)
    neuron.3 <- create.IF.plots(t3_rec, v3_rec, s3_rec)
    
    # panel
    res <- gridExtra:::grid.arrange(neuron.1, neuron.2, neuron.3, nrow = 3)
    
    
    return(res)
  }
  
  
  # plot
  
  output$plot <- renderPlot({ calcNeur() })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
