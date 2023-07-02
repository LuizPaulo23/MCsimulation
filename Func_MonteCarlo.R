#' @title Experimento de Monte Carlo 
#' @author Luiz Paulo Tavares Gonçalves 
#' @details Assumindo o seguinte modelo: Y_{i} = \beta_{1} + \beta_{2}X_{i} + \mu_{i} 

rm(list = ls())
graphics.off()
set.seed(123)

# \Packages
library(tidyverse)
library(DataExplorer)
library(stargazer)
library(stats)

# Desenvolvendo a função: 

MonteCarlo <- function(intercept, beta,n,repetitions){
  
  x = stats::rnorm(n) 
  intercept_mc = numeric(repetitions)
  beta_mc = numeric(repetitions)
  
  for (i in 1:repetitions) {
    # Definindo a equação 
    y <- intercept + beta * x + stats::rnorm(length(x), mean = 0, sd = 1)  # resíduos como normal-padrão
    model_lm <- stats::lm(y ~ x)
    intercept_mc[[i]] <- base::as.vector(model_lm$coefficients[1])
    beta_mc[[i]] <- base::as.vector(model_lm$coefficients[2])
    
    MonteCarlo = list(intercept_mc, beta_mc)
    
  }
 
  return(MonteCarlo)
   
}

simulation_mc = MonteCarlo(intercept = 20, 
                           beta = 0.6, 
                           n = 25,
                           repetitions = 10000)

plot_histogram(simulation_mc[[1]])
plot_histogram(simulation_mc[[2]])

