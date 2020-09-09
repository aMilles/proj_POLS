library(here)
library(shiny)

# HARVEST AND GROWTH FUNCTIONS AS IMPLEMENTED IN THE MODEL

harvest_function <- 
  function(RD_, harvest_alpha, harvest_beta){
    HR = RD_ ^ harvest_alpha / harvest_beta^harvest_alpha
    return(HR)
  }

growth_function <- 
  function(RD_, growth_beta, growth_type, growth_linear, linear_growth_limit){
    if(growth_type == "logistic"){
      RD2 = RD_ - growth_beta
      SIGMA = 1 / (1 + 2.72 ^ (- 1*RD2))
      GR = SIGMA * (1- SIGMA)
    }else{
      GR = ifelse(RD_ < linear_growth_limit, growth_linear, 0)
    }
    return(GR)
  }


# PLOT FOR DEFAULT PARAMETERIZATION

RD <- 0
for(i in (seq(50))) RD <- append(RD,growth_function(sum(RD), 3, "logistic", 0, 0))

png(here("Figs", "2020-08-20", "supplemental", "Fig_SX08.png"), width = 650, height = 300)
par(mfrow = c(1,3), oma = c(0,0,0,0), mar = c(4,4,2,1), bty = "l", cex = 1.5)
plot(cumsum(RD)[-1], RD[-1], type= "l", xlab = "Resource density", ylab = "Growth rate")
plot(cumsum(RD)[-1], harvest_function(cumsum(RD)[-1], 1.5, 5), type = "l", xlab = "Resource density", ylab = "Harvest rate")
abline(v = cumsum(RD)[-1][which.min(abs(RD[-1] - harvest_function(cumsum(RD)[-1], 1.5, 5)))], lty = "dashed")
plot(cumsum(RD)[-1], RD[-1] - harvest_function(cumsum(RD)[-1], 1.5, 5), type = "l", xlab = "Resource density", ylab = "Growth rate - harvest rate")
abline(h = 0, lty = "dashed")
dev.off()

# PLOT FOR ALTERNATIVE PARAMETERIZATION WITH LOGISTIC-GROWTH-FACTOR EQUAL 1.75

RD <- 0
for(i in (seq(100))) RD <- append(RD,growth_function(sum(RD), 1.75, "logistic", 0, 0))

png(here("Figs", "2020-08-20", "supplemental", "Fig_SX09.png"), width = 650, height = 300)
par(mfrow = c(1,3), oma = c(0,0,0,0), mar = c(4,4,2,1), bty = "l", cex = 1.5)
plot(cumsum(RD)[-1], RD[-1], type= "l", xlab = "Resource density", ylab = "Growth rate")
plot(cumsum(RD)[-1], harvest_function(cumsum(RD)[-1], 1.5, 5), type = "l", xlab = "Resource density", ylab = "Harvest rate")
abline(v = cumsum(RD)[-1][which.min(abs(RD[-1] - harvest_function(cumsum(RD)[-1], 1.5, 5)))], lty = "dashed")
plot(cumsum(RD)[-1], RD[-1] - harvest_function(cumsum(RD)[-1], 1.5, 5), type = "l", xlab = "Resource density", ylab = "Growth rate - harvest rate")
abline(h = 0, lty = "dashed")
dev.off()


if(F){
  shinyApp(ui = shinyUI(fluidPage(
    fluidRow(
      column(4,
             sliderInput("growth_beta", "growth_beta", 1, 20, 3, 0.1)
      ),
      column(4,
             sliderInput("linear_growth_rate", "growth_linear", 0.01, 1, .1, 0.01),
             sliderInput("linear_growth_limit", "growth_linear_limit", 10, 30, 15, 1),
             shiny::selectInput(inputId = "growth_type", label = "growth type", choices = c("linear", "logistic"), selected = "logistic", multiple = F)
             ),
        column(4,
               sliderInput("harvest_alpha", "harvest_alpha", 0.1, 5, 1.5, 0.1),
               sliderInput("harvest_beta", "harvest_beta", 1, 20, 10, 0.1)
        )
        ),
      fluidRow(
        column(10,
               plotOutput("Dynamics", height = 600)
        )
      )
    )
  ),
  server = shinyServer(function(input, output){
    output$Dynamics <- renderPlot({
      
      RD <- 0
      for(i in (seq(100))) RD <- append(RD,growth_function(sum(RD), input$growth_beta, input$growth_type, input$linear_growth_rate, input$linear_growth_limit))
      
      par(mfrow = c(2,2))
      plot(seq(length(RD))[-1], cumsum(RD)[-1], type = "l", xlab = "Time steps", ylab = "Resource density", main = "Change in resource density (initial value = 0)")
      plot(cumsum(RD)[-1], RD[-1], type= "l", xlab = "Resource density", ylab = "Growth rate", main = "dRD / dt")
      plot(cumsum(RD)[-1], harvest_function(cumsum(RD)[-1], input$harvest_alpha, input$harvest_beta), type = "l", xlab = "Resource density", ylab = "Harvest rate", main = "Harvest rates")
      plot(cumsum(RD)[-1], RD[-1] - harvest_function(cumsum(RD)[-1], input$harvest_alpha, input$harvest_beta), type = "l", xlab = "Resource density", ylab = "Growth rate - harvest rate", main = "Growth rate - harvest rate")
    })
  })
  )
}
