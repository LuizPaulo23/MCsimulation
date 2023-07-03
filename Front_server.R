#' @title Experimento de Monte Carlo 
#' @author Luiz Paulo Tavares Gonçalves 
#' @details Assumindo o seguinte modelo: Y_{i} = \beta_{1} + \beta_{2}X_{i} + \mu_{i} 

rm(list = ls())
graphics.off()
set.seed(123)

# Packages 

library(shiny)
library(ggplot2)
library(stats)
library(bslib)

# Custom - Front

custom_theme = custom_theme = bslib::bs_theme(bootswatch = "darkly")

# Informação da descrição 

info <- "The Monte Carlo methodology is a statistical technique that involves the use of random sampling to perform simulations and estimate numerical results. The method is named after the famous Monte Carlo casino, known for its games of chance based on random events. In the context of the linear model, Monte Carlo simulation can be applied to obtain estimates of the model parameters (β1 and β2) and evaluate the uncertainty associated with these estimates. Monte Carlo simulation allows for examining multiple realizations of the model based on different input values and obtaining a distribution of results. The application follows the linear model defined as Yᵢ = β₁ + β₂Xᵢ + μᵢ, where Yᵢ represents the dependent variable, Xᵢ is the independent variable, β₁ and β₂ are the model parameters, and μᵢ is the error term."

# \\\\Front-end

ui <- shiny::fluidPage(
  theme = custom_theme,
  titlePanel("Monte Carlo Simulation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("intercept", "Intercept (β1):", value = NULL),
      numericInput("beta", "Parameter (β2):", value = NULL),
      selectInput("distribution",
                  label = "Distribution of X and Residues:",
                  choices = c("Gaussian", "Uniform", "Exponential", "Poisson: λ = 2"), 
                  selected = NULL),
      numericInput("n", "Sample Size:", min = 1,
                   value = 2),
      sliderInput("repetitions", "Number of Repetitions (simulation):", 
                  min = 0, 
                  max = 5000, 
                  step = 100, 
                  value = 100),
      actionButton("simulate", "Simulate"),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Information",
          fluidRow(
            column(width = 12,
                   textOutput("descricao")
            ), 
            column(width = 12,
                   HTML("Developed by Luiz Paulo Tavares, GitHub: <a href='https://github.com/LuizPaulo23'>https://github.com/LuizPaulo23</a>")
          )
        )
     ),
        tabPanel(
          "Plots",
          fluidRow(
            column(width = 6,
                   plotOutput("intercept")
            ), 
            
            column(width = 6, 
                   plotOutput("beta"))
          )
        )
      )
    )
  )
)


# \\\\\Back-end 

server <- function(input, output) {
  
# Função Monte Carlo -----------------------------------------------------------  

MonteCarlo <- function(intercept, beta,distribution,n,repetitions){
    
    
intercept_mc = numeric(repetitions)
beta_mc = numeric(repetitions)
    
    for (i in 1:repetitions) {
      # Definindo a equação 
      
      if(distribution == "Gaussian"){
        
        x = stats::rnorm(n) 
        y <- intercept + beta * x + stats::rnorm(length(x)) 
        model_lm <- stats::lm(y ~ x)
        intercept_mc[[i]] <- base::as.vector(model_lm$coefficients[1])
        beta_mc[[i]] <- base::as.vector(model_lm$coefficients[2])
        
        MonteCarlo = list(intercept_mc, beta_mc, x, y)
        
      } else if(distribution == "Uniform"){
        
        x = stats::runif(n) 
        y <- intercept + beta * x + stats::runif(length(x)) 
        model_lm <- stats::lm(y ~ x)
        intercept_mc[[i]] <- base::as.vector(model_lm$coefficients[1])
        beta_mc[[i]] <- base::as.vector(model_lm$coefficients[2])
        
        MonteCarlo = list(intercept_mc, beta_mc, x, y)
        
         } else if(distribution == "Exponential"){
        
        x = stats::rexp(n) 
        y <- intercept + beta * x + stats::rexp(length(x)) 
        model_lm <- stats::lm(y ~ x)
        intercept_mc[[i]] <- base::as.vector(model_lm$coefficients[1])
        beta_mc[[i]] <- base::as.vector(model_lm$coefficients[2])
        
        MonteCarlo = list(intercept_mc, beta_mc, x, y)
        
        
      }else if(distribution == "Poisson: λ = 2"){
        
        x = stats::rpois(n, lambda = 2)
        y <- intercept + beta * x + stats::rpois(length(x), lambda = 2) 
        model_lm <- stats::lm(y ~ x)
        intercept_mc[[i]] <- base::as.vector(model_lm$coefficients[1])
        beta_mc[[i]] <- base::as.vector(model_lm$coefficients[2])
        
        MonteCarlo = list(intercept_mc, beta_mc, x, y)
        
      }
      
      
    }
    
    # MonteCarlo = data.frame(target = y,
    #                         beta_1 = MonteCarlo[[1]], 
    #                         beta_2 = MonteCarlo[[2]], 
    #                         observations = x)
    
    return(MonteCarlo)
    
}
  
  
# Informação Descritiva --------------------------------------------------------

output$descricao <- renderText({ info })

# Gráfico de dispersão ---------------------------------------------------------
  
output$intercept <- renderPlot({
    if (input$simulate > 0) {
      
      mc_data <- MonteCarlo(input$intercept, 
                            input$beta,
                            input$distribution,
                            input$n,
                            input$repetitions)
      
      media <- mean(mc_data[[1]])
      desvio_padrao <- sd(mc_data[[1]])
      
      ggplot() +
        geom_histogram(data = data.frame(x = mc_data[[1]]), aes(x = x),
                       fill = "gray", 
                       color = "black",
                       bins = 30) +
        geom_vline(xintercept = media,
                   color = "red",
                   linetype = "dashed",
                   size = 1) +
        labs(x = "Intercept (β1)", 
             y = "Frequency",
             title = "Monte Carlo Simulation - Intercept (β1)",
             subtitle = paste("Mean:", round(media, 4), "| Standard deviation:", round(desvio_padrao, 4))) +
        theme_bw()
      
     
    }
})
  

# Parâmetro - beta2 ------------------------------------------------------------

output$beta <- renderPlot({
  if (input$simulate > 0) {
    
    mc_data <- MonteCarlo(input$intercept, 
                          input$beta,
                          input$distribution,
                          input$n,
                          input$repetitions)
    
    media <- mean(mc_data[[2]])
    desvio_padrao <- sd(mc_data[[2]])
    
    # Criar o histograma com ggplot
    ggplot() +
      geom_histogram(data = data.frame(x = mc_data[[2]]), aes(x = x),
                     fill = "gray", 
                     color = "black",
                     bins = 30) +
      geom_vline(xintercept = media,
                 color = "red",
                 linetype = "dashed",
                 size = 1) +
      labs(x = "Parameter (β2)", 
           y = "Frequency",
           title = "Monte Carlo Simulation - Parameter (β2)",
           subtitle = paste("Mean:", round(media, 4), "| Standard deviation:", round(desvio_padrao, 4))) +
      theme_bw()
    
    
  }
})

# Função para fazer o download dos dados ---------------------------------------

  output$downloadData <- downloadHandler(
    filename = function() {
      "monte_carlo_data.csv"
    },
    content = function(file) {
      
      mc_data <- MonteCarlo(input$intercept, 
                            input$beta, 
                            input$distribution,
                            input$n, 
                            input$repetitions)
      
      write.csv(data.frame(Intercept = mc_data[[1]],
                           Beta = mc_data[[2]]), 
                           file, 
                           row.names = FALSE)
    }
  )
}

# Executar a interface Shiny ---------------------------------------------------

shinyApp(ui = ui, server = server)

