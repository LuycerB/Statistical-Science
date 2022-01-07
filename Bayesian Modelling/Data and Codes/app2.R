library(shiny)
library(tidyverse)

# ui codes ----------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
    sliderInput("n", "Play to ncrease sample size", min = 100,
                max = 1000, step = 10,value = 100, animate = T),
    tags$hr(),
    numericInput("beta", "Beta value", value = 5),
    tags$hr(),
    numericInput("alpha", "Alpha Value", value = 5), width = 2
    ),
    
    mainPanel(
      h3("The likelihood and posterior distributions converge as the sample size n increases"),
      br(),
plotOutput("my_plot", height = "500px")
    )
    
  )
)

# server code -------------------------------------------------------------

server <- function(input, output, session){
  


# define posterior function -----------------------------------------------

post <- function(n = 100, t = 30, alpha = 1, beta = 1){

theta <- seq(0, 1, length.out = 101)  # 1data initialization

LL <- dbinom(t, n, theta)  # quartiles of a binomial

# prior
P0 <- dbeta(theta, alpha, beta)  #beta prior distribution
  
# posterior
alpha1 <- t + alpha
beta1 <- n - t + beta

Postr <- dbeta(theta, alpha1, beta1)

ggplot(data = NULL, aes(theta, LL/max(LL), col = "Likelihood")) + geom_line(size = 0.5)+
  geom_line(aes(theta, P0/max(P0), col = "Prior"), size = 0.5) + 
  geom_line(aes(theta, Postr/max(Postr), col = "Posterior"), size = 0.5) +
  labs(y = "Density", x = expression(theta)) + theme_minimal() +
  scale_colour_manual("", values = c("Likelihood"="green", "Prior"="red", "Posterior"="blue")) +
  theme(legend.position = "top")

}

output$my_plot <- renderPlot({
  post(n=input$n, t = 30/100*input$n, alpha = input$alpha, beta = input$beta)
})
  
  
}

shinyApp(ui, server)