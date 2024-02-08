

remove.last.beta <- function(tree, context)UseMethod("remove.last.beta")

  ##########################################################################################################
  ##########################################################################################################
remove.last.beta.default <- function(tree, context)
{
    result = tree
    
    if (length(context) == length(tree$context))
      if (sum(context == tree$context) == length(context)) ## arrived at the bottom of the tree and it is the context wanted
      {
        if (dim(tree$beta)[1] == 1)
          tree$beta = NULL
        else if (!is.null(tree$beta))
          tree$beta = array(0,c(dim(tree$beta)[1]-1, dim(tree$beta)[2], dim(tree$beta)[3]))
        result = tree
      }

    if (length(context) > length(tree$context)) ## the context is further down: recursevely try to find it in the children
      if (length(tree$child) > 0)
      for (ind in 1:length(tree$child))
        result$child[[ind]] = remove.last.beta(tree$child[[ind]],context)

    return(result)
}









