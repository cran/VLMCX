LogLik <- function(fit)UseMethod("LogLik")

#######################################################################################################################
# computes the log-likelihood of the data based on the tree structure and estimated parameters in the fitted VLMCX model
#######################################################################################################################
LogLik.VLMCX <- function(fit)
{
  ## estimate the parameters based on the tree structure
  fit$tree = estimate(fit$tree, fit$y, fit$X)
  alphabet = sort(unique(fit$y))
  

  y = fit$y
  X = fit$X
  d = ifelse(is.null(dim(X)), 1, dim(X)[2]) ## number of covariates
  
  logLikel = log(1/length(alphabet)) ## for the first data point - since there is no past
  for (i in 2:length(fit$y))
  {
    node = find.underlying.context.in.tree(fit$y[1:(i-1)], fit$tree)  
    
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
          if (d ==1) ## there is only one covariate through time: X is a vector
          {
            beta_v = as.numeric(node$beta[,1,ind_alphabet])
            prob[ind_alphabet] = exp(as.numeric(alpha_u + beta_v%*%X[(i-1):(i-length(beta_v))]))					
          }
          else ## there is a vector with several covariates observed through time: X is a matrix
          {
            dimension = length(node$beta[,1,ind_alphabet]) ## number of steps into the past
            beta_v = 0
            for (ind in 1:dimension)
              beta_v = beta_v + as.numeric(node$beta[ind,,ind_alphabet]%*%X[(i-ind),])
            prob[ind_alphabet] = exp(as.numeric(alpha_u + beta_v))					## multinomial regression
          }
        }
      }
      prob = c(1,prob)/(1 + sum(prob))
    }
    prob_this_observation = prob[which(alphabet == y[i])] ## get the proba corresponding to the observed data point
    
    if (is.nan(prob_this_observation)) ## this happens when exp(alpha+betaX) is too large then we have Inf/(1+Inf): should be 1
      prob_this_observation = 1
    if (prob_this_observation != 0) 
      logLikel = logLikel + log(prob_this_observation)
    
  }
  return(logLikel)
}









