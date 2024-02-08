predict <- function(fit, new.y = NULL, new.X = NULL)UseMethod("predict")

predict.VLMCX <- function(fit, new.y = NULL, new.X = NULL)
{
  # check if new.y is NULL
  if (is.null(new.y) && is.null(new.X))
  {
    new.y = fit$y
    new.X = fit$X
  }
  else if ((is.null(new.y) && !is.null(new.X)) && (!is.null(new.y) && is.null(new.X)))
  {
    cat("\n\n both new.y and new.X must be provided. Reverting to original data in fit. \n\n")  
  }
  
  
  # check if new.y is the same type as y in the VLMCX object
  if (sum(new.y %in% fit$y) != length(new.y))
    stop("\n\n elements of new.y are not of the same type as y in the fit object. ","\n\n") 
  
  ## Check if dimension of new.X is the same as dimension of fit$X
  d = ifelse(is.null(dim(fit$X)), 1, dim(fit$X)[2]) ## number of covariates
  d.new = ifelse(is.null(dim(new.X)), 1, dim(new.X)[2]) ## number of covariates
  if (d != d.new)
    stop("\n\n elements of new.y are not of the same type as y in the fit object. ","\n\n") 
  

  alphabet = sort(unique(fit$y))
  
  

    node = find.underlying.context.in.tree(new.y, fit$tree)  
    
    if (is.null(node)) ## context does not exist in tree: set equal probabilities
      prob = rep(1/length(alphabet), length(alphabet))
    else
      if (is.null(node$alpha)) ## there are no estimated parameters: it is not a leaf, not final node (nor partially final node)
        prob = rep(1/length(alphabet), length(alphabet))
      else ## there is alpha and maybe beta
      {
        if (is.null(node$beta)) ## check if beta was dropped
        {
          alpha_u = 0
          prob = rep(0,(length(alphabet)-1))
          for (ind_alphabet in 1:(length(alphabet)-1))
          {
            alpha_u = node$alpha[ind_alphabet]
            prob[ind_alphabet] = exp(as.numeric(alpha_u))  ## probabilities are computed only with alpha
          }
          
        }
        else ## beta exists
        {
          alpha_u = 0
          beta_v = 0
          prob = rep(0,(length(alphabet)-1))
          for (ind_alphabet in 1:(length(alphabet)-1))
          {
            alpha_u = node$alpha[ind_alphabet]
            if (d == 1) ## there is only one covariate through time: X is a vector
            {
              beta_v = as.numeric(node$beta[,1,ind_alphabet])
              prob[ind_alphabet] = exp(as.numeric(alpha_u + beta_v%*%new.X[length(new.y):(length(new.y)+1-length(beta_v))]))					
            }
            else ## there is a vector with several covariates observed through time: X is a matrix
            {
              dimension = length(node$beta[,1,ind_alphabet]) ## number of steps into the past
              beta_v = 0
              for (ind in 1:dimension)
                beta_v = beta_v + as.numeric(node$beta[ind,,ind_alphabet]%*%new.X[(length(new.y)+1-ind),])
              prob[ind_alphabet] = exp(as.numeric(alpha_u + beta_v))					## multinomial regression
            }
          }
        }
        prob = c(1,prob)/(1 + sum(prob))
      }

      too.large.index = which(is.nan(prob))
      if (length(too.large.index) > 0) ## this happens when exp(alpha+betaX) is too large then we have Inf/(1+Inf): should be 1
        prob[too.large.index] = 1

  
  return(prob)
}