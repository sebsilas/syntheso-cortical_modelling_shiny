# implemented by Seb Silas, March 2020. Goldsmiths University. ssila010@gold.ac.uk


require(matlab)
require(ramify)
require(ggplot2)
require(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Izhikevich Network"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Ne",
                  "Number excitatory:",
                  min = 0,
                  max = 1000,
                  value = 800,
                  step = 1,
      ),
      
      sliderInput("Ni",
                  "Number inhibitory:",
                  min = 0,
                  max = 1000,
                  value = 200,
                  step = 1,
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
  Ne <- as.numeric(input$Ne)              
  Ni <- as.numeric(input$Ni)
  
  re <- rand(Ne,1)
  ri <- rand(Ni,1)
  
  # Excitatory neurons    # Inhibitory neurons (i.e different column vectors)
  a <- matrix(c(0.02*ones(Ne,1), 0.02+0.08*ri))
  
  b <- matrix(c(0.2*ones(Ne,1) , 0.25-0.05*ri))
  c <- matrix(c(-65+15*re^2, 65*ones(Ni,1)))
  d <- matrix(c(8-6*re^2, 2*ones(Ni,1)))
  
  
  S <- cbind(0.5*rand(Ne+Ni,Ne),-rand(Ne+Ni,Ni))
  
  v <- -65*ones(Ne+Ni,1)  # Initial values of v
  u <- b*v               # Initial values of u
  firings <- c()           # spike timings
  
  for (t in 1:1000) {         # simulation of 1000 ms 
    
    I <- matrix(c(5*randn(Ne,1), 2*randn(Ni,1))) # thalamic input 
    
    fired <- matlab::find(v >= 30) # indices of spikes
    
    if (!isempty(fired))    { 
      
      firings <- rbind(firings,cbind(t+0*fired, fired))
      v[fired] <- c[fired]  
      u[fired] <- u[fired]+d[fired]
      
      I <- I + sum(S[,fired]) 
    }
    
    v <- v + 0.5 * (0.04*v^2+5*v+140-u+I)
    v <- v + 0.5 * (0.04*v^2+5*v+140-u+I)
    u <- u + a * (b*v-u)   
    
  }
  

  data <- data.frame("x" = firings[,1], "y" = firings[,2])

  ggplot(data, aes(x=x, y=y)) + 
    geom_point(shape = ".", size = 0.5, alpha = 0.1) +
    geom_rug(alpha = 0.01) +
    labs(x="Time (ms)", y = "Neurons")  
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
