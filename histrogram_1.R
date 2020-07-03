shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
    selectInput("variable", "Variable:",
                c("Normal" = rnorm(1, 1, 1),
                  "Binomial" = rbinom(1, 1, 1)  ))
      ),
    
    mainPanel("hist")
    )
  ),
  server = function(input, output) {
    rv= reactiveValues(data= rnorm(10, 0, 1))
    observeEvent(input$normal, {rv$data= input$variable(100, 1, .5)})
   
    output$hist= renderPlot({hist({rv$data})})
  }
)
