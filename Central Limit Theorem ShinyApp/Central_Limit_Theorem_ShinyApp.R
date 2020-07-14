library(shiny)

ui = fluidPage(
  headerPanel("Central Limit Theorem"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Select a Distribution",
                  c("Normal", "Binomial", "Exponential", "Uniform", "Poisson", "Log normal",
                    "Gamma", "Beta", "T", "Chi-square")),
      
      sliderInput("mean_in", "Sample size", min = 0, max = 1000, value = 500, animate = T),
      
      conditionalPanel(condition = "input.dist == 'Normal'", 
                       numericInput("mean.n", "Mean", min = -1000, max = 1000, value = 0),
                       numericInput("sd.n", "SD", min = 0, max = 100, value = 1)
      ),
      conditionalPanel(condition = "input.dist == 'Binomial'", 
                       numericInput("success", "Success", min = 0, max = 1000, value = 0),
                       numericInput("prob", "probability", min = 0, max = 1, value = 0)
      ),
      conditionalPanel(condition = "input.dist == 'Exponential'", 
                       numericInput("lambda.exp", "Lambda", min = 0, max = 1000, value = 0)
      ),
      conditionalPanel(condition = "input.dist == 'Uniform'", 
                       numericInput("min", "Minimum value", min = -1000, max = 1000, value = 0),
                       numericInput("max", "Maximum value", min = -1000, max = 1000, value = 0)
      ),
      conditionalPanel(condition = "input.dist == 'Poisson'",
                       numericInput("lambda.p", "Lambda", min = 0, max = 100, value = 0)
        
      ),
      conditionalPanel(condition = "input.dist == 'Log normal'",
                       numericInput("mean.l", "Mean", min = 0, max = 1000, value = 0),
                       numericInput("sd.l", "SD", min = 0, max = 100, value = 0)
      ),
      conditionalPanel(condition = "input.dist == 'Gamma'",
                       numericInput("alpha.g", "Alpha", min = -1000, max = 1000, value = 0),
                       numericInput("beta.g", "Beta", min = -1000, max = 1000, value = 0)
        
      ),
      conditionalPanel(condition = "input.dist == 'Beta'",
                       numericInput("alpha.b", "Alpha", min = -1000, max = 1000, value = 0),
                       numericInput("beta.b", "Beta", min = -1000, max = 1000, value = 0)
      ),
      conditionalPanel(condition = "input.dist == 'T'",
                       numericInput("df.t", "Degrees of Freedom", min = 0, max = 100, value = 0)
      ),
      conditionalPanel(condition = "input.dist == 'Chi-square'",
                       numericInput("df.chi", "Degrees of Freedom", min = 0, max = 100, value = 0)
        
      ),
      actionButton("new", "New plot"),
      p("Click on 'New plot' to get new another plot for the same information.")
    ),
    
    mainPanel(plotOutput("Hist"), plotOutput("hist"))
  )
)


y= function(input, output, session){
  output$Hist= renderPlot({req(input$new)
    dist_type= input$dist
    size= 100
    
    if(dist_type == "Normal"){
  
        random= rnorm(size, input$mean.n, input$sd.n)
    }
    
    else if(dist_type == "Binomial"){
      
        random= rbinom(size, input$success, input$prob)
      
    }
    
    else if(dist_type == "Exponential"){
      
        random= rexp(size, 1/input$lambda.exp)
      
    }
    
    else if(dist_type == "Uniform"){
      
        random= runif(size, input$min, input$max)
      
    }
    
    else if(dist_type == "Poisson"){
     
        random= rpois(size, input$lambda.p)
      
    }
    
    else if(dist_type == "Log normal"){
     
        random= rlnorm(size, input$mean.l, input$sd.l)
      
    }
    
    else if(dist_type == "Gamma"){
      
        random= rgamma(size, input$alpha.g, input$beta.g)
      
    }
    
    else if(dist_type == "Beta"){
      
        random= rbeta(size, input$alpha.b, input$beta.b)
      
    }
    
    else if(dist_type == "T"){
      
        random= rt(size, input$df.t)
      
    }
    
    else if(dist_type == "Chi-square"){
      
        random= rchisq(size, input$df.chi)
      
    }
    
    # Histogram------------------
    hist(random, col = "#C0C0C0", breaks = 15, main = paste("Histogram of", dist_type, "Distribution"), 
         xlab = "Sample")
  })
  
    output$hist= renderPlot({req(input$new)
    dist_type= input$dist
    size= 100
    mean_in= input$mean_in
    random_vec= c()
    
    if(dist_type == "Normal"){
      
      for(i in 1:mean_in){
      random_vec[i]= mean(rnorm(size, input$mean.n, input$sd.n))
      }
    }
    
    else if(dist_type == "Binomial"){
      
      for(i in 1:mean_in){
        random_vec[i]= mean(rbinom(size, input$success, input$prob))
      }
    }
    
    else if(dist_type == "Exponential"){
      
      for(i in 1:mean_in){
      random_vec[i]= mean(rexp(size, 1/input$lambda.exp))
      }
    }
    
    else if(dist_type == "Uniform"){
      for(i in 1:mean_in){
        random_vec[i]= mean(runif(size, input$min, input$max))
      }
    }
    
    else if(dist_type == "Poisson"){
      for (i in 1:mean_in) {
        random_vec[i]= mean(rpois(size, input$lambda.p))
      }
    }
    
    else if(dist_type == "Log normal"){
      for(i in 1:mean_in){
        random_vec[i]= mean(rlnorm(size, input$mean.l, input$sd.l))
      }
    }
    
    else if(dist_type == "Gamma"){
      for(i in 1:mean_in){
        random_vec[i]= mean(rgamma(size, input$alpha.g, input$beta.g))
      }
    }
    
    else if(dist_type == "Beta"){
      for(i in 1:mean_in){
        random_vec[i]= mean(rbeta(size, input$alpha.b, input$beta.b))
      }
    }
    
    else if(dist_type == "T"){
      for(i in 1:mean_in){
        random_vec[i]= mean(rt(size, input$df.t))
      }
    }
    
    else if(dist_type == "Chi-square"){
      for(i in 1:mean_in){
        random_vec[i]= mean(rchisq(size, input$df.chi))
      }
    }
    
    # Histogram------------------
    hist(random_vec, col = "#488AC7", breaks = 15, main = paste("Distribution of mean for", dist_type, "Distribution"), 
         xlab = "sample mean")
  })
}

shinyApp(ui, y)
