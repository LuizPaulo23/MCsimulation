#' @title Experimento de Monte Carlo 
#' @author Luiz Paulo Tavares Gonçalves 
#' @details Assumindo o seguinte modelo: Y_{i} = \beta_{1} + \beta_{2}X_{i} + \mu_{i} 

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
      
      
      
    }else if(distribution == "Poisson"){
      
      x = stats::rpois(n, lambda = 2)
      y <- intercept + beta * x + stats::rpois(length(x), lambda = 2) 
      model_lm <- stats::lm(y ~ x)
      intercept_mc[[i]] <- base::as.vector(model_lm$coefficients[1])
      beta_mc[[i]] <- base::as.vector(model_lm$coefficients[2])
      
      MonteCarlo = list(intercept_mc, beta_mc, x, y)
      
    }
  
    
  }
 
  MonteCarlo = data.frame(target = y,
                          beta_1 = MonteCarlo[[1]], 
                          beta_2 = MonteCarlo[[2]], 
                          observations = x)
  
  return(MonteCarlo)
   
}






