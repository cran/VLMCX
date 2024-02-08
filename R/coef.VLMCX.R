coef <- function(fit, context)UseMethod("coef")

### obtain the alpha and beta coefficients corresponding to the node "context"
coef.VLMCX <- function(fit, context)
{
    node = find.underlying.context.in.tree(rev(context), fit$tree) ## returns the node with all its info where the context was found
    
    estimate = NULL
    estimate$alpha = node$alpha
    estimate$beta = node$beta
    
    if (is.null(node$alpha) && is.null(node$beta))
      stop("\n\n", context," is not a context: either it is not a final node or it is not in the estimated tree. Check VLMCX.draw() to see which contexts are in the estimated tree.","\n\n") 
    
    ## inform the user of the baseline state and which coefficients correspond to which state transitions
    cat("\n\n coefficients correspond to the transition into states (", sort(unique(fit$y))[2:length(unique(fit$y))], "), with baseline state (",sort(unique(fit$y))[1],")\n\n")

    return(estimate)
}