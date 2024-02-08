 

AIC <- function(fit)UseMethod("AIC")

## computes the AIC of the model for the data in a VLMCX object using the count.param() and LogLik.VLMCX() functions
AIC.VLMCX <- function(fit)
{
    k = count.param(fit$tree)
    
    result = -2*LogLik.VLMCX(fit) + 2*k

  return(result)
}
  