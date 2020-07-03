library(shiny)

x <- fluidPage(
  titlePanel("Hello"),
  sidebarLayout(
    sidebarPanel(
      actionButton("normal", "Normal"),
      actionButton("poisson", "Poisson"),
      actionButton("chi", "Chi-square"),
      actionButton("binomial", "Binomial"),
      actionButton("exponential", "Exponential"),
      actionButton("t", "T"),
      
      numericInput("sample", "Sample Size", min = 0, max = 1000, value = 0),
      numericInput("success", "Number of Success/ Mean", min= 0, max = 1000, value = 0),
      numericInput("probability", "Probability/ SD", min = 0, max = 1, value = "Enter value"),
      numericInput("lambda", "Lambda/Degrees of Freedom", min = 0, max = 100, value = 0)
    ),
    mainPanel(plotOutput("hist"))
  )
)


y <- function(input, output, session) {
  rv= reactiveValues(data= rnorm(0, 0, 0))
  observeEvent(input$normal, {rv$data= rnorm(input$sample, input$success, input$probability)})
  observeEvent(input$poisson, {rv$data= rpois(input$sample, input$lambda)})
  observeEvent(input$binomial, {rv$data= rbinom(input$sample, input$success, input$probability)})
  observeEvent(input$exponential, {rv$data= rexp(input$sample, input$lambda)})
  observeEvent(input$t, {rv$data= rt(input$sample, input$lambda)})
  observeEvent(input$chi, {rv$data= rchisq(input$sample, input$lambda)})
  
  output$hist= renderPlot({hist(rv$data)})
  
}

shinyApp(x, y)



