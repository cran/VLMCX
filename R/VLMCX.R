VLMCX <- function(y, X, alpha.level = 0.05, max.depth = 5, n.min = 5, trace = FALSE)UseMethod("VLMCX")

###############################################################################################################
###############################################################################################################
## Main function
## Starts obtaining the maximum context tree and calls the context algorithm to sequentially prune
## as in Zambom et al 2022
###############################################################################################################
###############################################################################################################
VLMCX.default <- function(y, X, alpha.level = 0.05, max.depth = 5, n.min = 5, trace = FALSE)
{
   max_context = maximum.context(y, X, max.depth = max.depth, n.min = n.min)
   r = max_context$tree.depth
   
   ## main call to the pruning algorithm
   result = context.algorithm(max_context, max_context$tree, alpha.level = alpha.level, max.depth = max.depth, n.min = n.min, trace = trace)

   if (trace == TRUE)
     draw(result, title = "Estimated Context Tree Structure", print.coef = trace)
   
   result$tree = estimate(result$tree, y, X)
   
   return(result)
}
   
   