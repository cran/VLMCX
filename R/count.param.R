

count.param <- function(node)UseMethod("count.param")

## recursive function
## counts the number of parameters in the entire beta-context tree: both alpha and beta parameters are counted
count.param.default <- function(node)
{
      n.param = 0
      if (!is.null(node$alpha))
        n.param = n.param + length(node$alpha)
      if (!is.null(node$beta))
        n.param = n.param + length(node$beta)
      
      if (length(node$child) > 0)
        for (ind in 1:length(node$child))
          n.param = n.param + count.param(node$child[[ind]]) ## recursive call to cound the children
      return(n.param)  
}





