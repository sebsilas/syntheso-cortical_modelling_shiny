# implemented by Seb Silas, March 2020. Goldsmiths University. ssila010@gold.ac.uk

library(shiny)
library(matlab)
library(ramify)

# plots
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Izhikevich Neuron"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("a_parameter",
                        "a:",
                        min = 0,
                        max = 2,
                        value = 0.02,
                        step = 0.01,
                        ),
            
            sliderInput("b_parameter",
                        "b:",
                        min = 0,
                        max = 4,
                        value = 0.2,
                        step = 0.01,
                        ),
        
        sliderInput("c_parameter",
                    "c:",
                    min = -70,
                    max = 2,
                    value = -65+15*re^2,
                    step = 0.2,
                    ),
        
        sliderInput("d_parameter",
                    "d:",
                    min = 0,
                    max = 10,
                    value = 8-6*re^2,
                    step = 0.01,
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
           plotOutput("plot1"),
           plotOutput("plot2"),
           plotOutput("plot3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# Numerical Integration with the Euler method

    
calcNeur <- function() {

# Setting parameters

# Excitatory neuron
Ne <- 1
re <- matrix(runif(1*Ne), ncol=1)

# Find the correct set of parameters in the literature to reproduce different neurodynamics
    
a <- as.numeric(input$a_parameter)
b <- as.numeric(input$b_parameter)
c <- as.numeric(input$c_parameter)
d <- as.numeric(input$d_parameter)


t <- c(2:input$t) # control evolution across time

#  Initial values of v

v <- -65*ones(1,1000)


#  Initial values of u
u <- b*v

# spike timings
firings <- c()

I <- 5*randn(Ne,1000)


#  simulation of 1000 ms 
for (t in t) {

    #  thalamic input 
    
    fired=find(v[1,t-1]>=30) #  indices of spikes
    
    if (!isempty(fired))    { 
        
        firings <- rbind(firings,cbind(t+0*fired, fired))
        
        v[1,t-1] <- c[fired]  
        u[1,t-1] <- u[fired]+d[fired]
    }
    
    v[1,t] <- v[1,t-1]+1.0*(0.04*v[1,t-1]^2+5*v[1,t-1]+140-u[1,t-1]+I[1,t-1])
    
    u[1,t] <- u[1,t-1]+a*(b*v[1,t-1]-u[1,t-1])   
    
}

data <- data.frame("x"=firings[,1],"y"=firings[,2])
data2 <- data.frame("x" = c(1:length(v)), "y" = as.vector(v))
data3 <- data.frame("x" = c(1:length(I)), "y" = as.vector(I))

res <- list(data, data2, data3)
return(res)
}
    

# plot 1

output$plot1 <- renderPlot({
   data <- calcNeur()[[1]]
ggplot(data, aes(x=x, y=y)) + 
    geom_point() +
    ggtitle("Spikes")
})



# plot 2
output$plot2 <- renderPlot({
    data2 <- calcNeur()[[2]]
ggplot(data2, aes(x=x, y=y)) +
    geom_line() +
    ggtitle("Membrane Potential")
})



# plot 3
output$plot3 <- renderPlot({
    data3<- calcNeur()[[3]]
data3 %>%
    ggplot(aes(x=x, y=y)) +
    geom_line() +
    geom_point() +
    ggtitle("Input Current")
})


}

# Run the application 
shinyApp(ui = ui, server = server)
