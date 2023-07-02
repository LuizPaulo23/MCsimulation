library(shiny)

# Definição da interface Shiny
ui <- fluidPage(
  titlePanel("Monte Carlo Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("intercept", "Intercept:", value = 0),
      numericInput("beta", "Beta:", value = 1),
      numericInput("n", "Sample Size (n):", value = 100),
      numericInput("repetitions", "Number of Repetitions:", value = 1000),
      actionButton("simulate", "Simulate"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Definição do servidor Shiny
server <- function(input, output) {
  
  # Função Monte Carlo
  MonteCarlo <- function(intercept, beta, n, repetitions) {
    x <- rnorm(n)
    intercept_mc <- numeric(repetitions)
    beta_mc <- numeric(repetitions)
    
    for (i in 1:repetitions) {
      y <- intercept + beta * x + rnorm(length(x), mean = 0, sd = 1)
      model_lm <- lm(y ~ x)
      intercept_mc[i] <- as.vector(model_lm$coefficients[1])
      beta_mc[i] <- as.vector(model_lm$coefficients[2])
    }
    
    list(intercept_mc, beta_mc)
  }
  
  # Função para gerar o gráfico
  output$plot <- renderPlot({
    if (input$simulate > 0) {
      mc_data <- MonteCarlo(input$intercept, input$beta, input$n, input$repetitions)
      plot(mc_data[[1]], mc_data[[2]], xlab = "Intercept", ylab = "Beta",
           main = "Monte Carlo Simulation", pch = 16, col = "blue")
    }
  })
  
  # Função para fazer o download dos dados
  output$downloadData <- downloadHandler(
    filename = function() {
      "monte_carlo_data.csv"
    },
    content = function(file) {
      mc_data <- MonteCarlo(input$intercept, input$beta, input$n, input$repetitions)
      write.csv(data.frame(Intercept = mc_data[[1]], Beta = mc_data[[2]]), file, row.names = FALSE)
    }
  )
}

# Executar a interface Shiny
shinyApp(ui = ui, server = server)



