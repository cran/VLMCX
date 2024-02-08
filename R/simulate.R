 

simulate <- function(VLMCXtree, nsim = 500, X = NULL, seed = NULL, n.start = 100)UseMethod("simulate")

########################################################################################################################
## simulates a categorical time series of size nsim corresponding to the Markov chain with exogenous covariates whose
## structure is in VLMCXtree
########################################################################################################################
simulate.default <- function(VLMCXtree, nsim = 500, X = NULL, seed = NULL, n.start = 100)
{  
  alphabet = get.alphabet.from.tree(VLMCXtree)
  
  set.seed(seed)
  
  if (is.null(X)) ### if X is not provided, a vector of zeroes is used
    X = rep(0, n.start+nsim)
  d = ifelse(is.null(dim(X)), 1, dim(X)[2])
  
  y = sample(alphabet,1) ## initial 2 obs
  
  for (i in 2:(n.start+nsim))
  {
    node = find.underlying.context.in.tree(y, VLMCXtree) 
    
    

    if (is.null(node)) ## context does not exist in tree
      prob = rep(1/length(alphabet), length(alphabet))
    else if (is.null(node$alpha)) ## there are no estimated parameters = it is not a leaf not final node (nor partially final node)
        prob = rep(1/length(alphabet), length(alphabet))
    else ## there is alpha and maybe beta
    {
      if (is.null(node$beta))
      {
        alpha_u = 0
        prob = rep(0,(length(alphabet)-1))
        for (ind_alphabet in 1:(length(alphabet)-1))
        {
          alpha_u = node$alpha[ind_alphabet]
          prob[ind_alphabet] = exp(as.numeric(alpha_u)) ## compute probabilities based on the multinomial regression model
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
          if (d ==1)
          {
            beta_v = as.numeric(node$beta[,1,ind_alphabet])
            prob[ind_alphabet] = exp(as.numeric(alpha_u + beta_v%*%X[(i-1):(i-length(beta_v))]))	  ## compute probabilities based on the multinomial regression model				
          }
          else
          {
            dimension = length(node$beta[,1,ind_alphabet]) ## number of steps into the past
            beta_v = 0
            for (ind in 1:dimension)
            {
              beta_v = beta_v + as.numeric(node$beta[ind,,ind_alphabet]%*%X[(i-ind),])
            }
            prob[ind_alphabet] = exp(as.numeric(alpha_u + beta_v))   ## compute probabilities based on the multinomial regression model
          }
        }
      }
      prob = c(1,prob)/(1 + sum(prob))
    }    
    for (ind in 1:length(prob)) 
      if (is.nan(prob[ind]))  ## this happens when exp(alpha+betaX) is too large then we have Inf/(1+Inf): should be 1
        prob[ind] = 1
    y[i] = sample(alphabet,1,prob=prob)
    
  }
  
  result = NULL
  result$y = y[(n.start+1):(n.start+nsim)]
  if (d == 1)
    result$X = X[(n.start+1):(n.start+nsim)]
  else
    result$X = X[(n.start+1):(n.start+nsim),]
  return(result)
}
  