

BIC <- function(fit)UseMethod("BIC")

## computes the BIC of the model for the data in a VLMCX object using the count.param() and LogLik.VLMCX() functions

BIC.VLMCX <- function(fit)
{       
    k = count.param(fit$tree)
    
    result = -2*LogLik.VLMCX(fit) + log(length(fit$y))*k
  
  return(result)
}
