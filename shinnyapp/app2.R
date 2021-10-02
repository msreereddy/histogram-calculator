library(shiny)

ui <- fluidPage(pageWithSidebar(
  headerPanel("GRAPHS"),
  
  sidebarPanel(
    selectInput("Distribution", "please select distribution type",
                choices = c("Normal", "Exponential")),
    sliderInput("sampleSize","please select samplesize: ",
                min = 100, max = 5000, value = 1000, step = 100),
    conditionalPanel(condition = "input.Distribution == 'Normal'",
                     textInput("mean", "please select the mean", 10),
                     textInput("sd", "please select standard deviation", 3)),
    conditionalPanel(condition = "input.Distribution == 'Exponential'",
                     textInput("lambda", "please select exponential lambda: ", 1))
  ),
  mainPanel(
    plotOutput("myPlot")
  )
)
)
server <-  function(input, output, session){
  output$myPlot <- renderPlot({
    
    distType <- input$Distribution
    size <- input$sampleSize
    
    if(distType == "Normal"){
      
      randomVec <- rnorm(size, mean = as.numeric(input$mean), sd = as.numeric(input$sd))
    }
    else {
      randomVec <- rexp(size, rate = 1/ as.numeric(input$lambda))
    }
    
    hist(randomVec, col = "yellow")
  })
}
shinyApp(ui = ui, server = server)