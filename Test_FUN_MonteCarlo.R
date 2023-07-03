#' @title Testando a função Monte Carlo Simulation 
#' @author Luiz Paulo Tavares Gonçalves 

# Configurando ambiente de trabalho --------------------------------------------

rm(list = ls())
graphics.off()
set.seed(123)

# \Packages
library(tidyverse)
library(DataExplorer)
library(stargazer)
library(stats)

# Chamando a função 

simulation_mc = MonteCarlo(intercept = 20, 
                           distribution = "Exponential",
                           beta = 0.6, 
                           n = 1000,
                           repetitions = 1000)


ggplot2::ggplot(data = simulation_mc)+
         aes(y = target,
             x = observations)+
         geom_point(pch = 19)+
         geom_smooth(method = "lm", 
                     col = "red", 
                     fill = "purple")+
        labs(title = "Relationship between X and Y", 
             #caption = paste0("Mean Y:", mean(target), "|", "Mean X:", mean(observations)),
             y = "Y", 
             x = "x")

# Distribuições de probabilidade 
gaussiana <- rnorm(100) %>% print()
normal_padrao <- rnorm(100, mean = 0, sd = 1) %>% print()
uniforme <- runif(100) %>% print()
exponencial <- rexp(100) %>% print()
poisson <- rpois(100, lambda = 2) %>% print()


        

