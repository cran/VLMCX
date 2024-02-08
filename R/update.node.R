

update.node <- function(tree, node)UseMethod("update.node")

  ##########################################################################################################
  ##########################################################################################################
  ##########################################################################################################
  ## updates the tree with the new characteristics of the node
update.node.default <- function(tree, node)
{
    result = tree
    
    if (length(node$context) == length(tree$context))
    {
      if (sum(node$context == tree$context) == length(node$context)) ## arrived at node
      {
        result = node
      } else ## it is the same length but it is not the same
        if (length(tree$child) > 0)
          for (ind in 1:length(tree$child))
            result$child[[ind]] = update.node(tree$child[[ind]],node)
    } 
    
    if (length(node$context) > length(tree$context)) ## the context is further down: recursevely try to find it in the children
      if (length(tree$child) > 0)
        for (ind in 1:length(tree$child))
          result$child[[ind]] = update.node(tree$child[[ind]],node)
        
        return(result)

}







