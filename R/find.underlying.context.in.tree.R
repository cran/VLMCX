find.underlying.context.in.tree <- function(y, tree)UseMethod("find.underlying.context.in.tree")


## navigates the tree recursively to find the node that has the context desired (which is in the already reversed string y)
find.underlying.context.in.tree.default <- function(y, tree)
{
  result = NULL
  
  is_root = FALSE
  if (length(tree$context) == 1)
    if (tree$context == "x") ## when we are at the root
      is_root = TRUE
  
  if (is_root == TRUE)
  {
    if (length(tree$child) == 0) ## the root is empity = no children = no tree
      result = NULL
    else
      for (ind in 1:length(tree$child))
      {
        possible.tree = find.underlying.context.in.tree(y,tree$child[[ind]])  ## if we are at the root, there is no context here and we need to look at the children nodes
        
        if (!is.null(possible.tree)) ## if the node of the context was found in the recursion, return it
          result = possible.tree
      }
  } else ## this is not a root node
  {
    len_this_context = length(tree$context)
    if (length(y) >= len_this_context)
    {
      if (sum(y[length(y):(length(y)-len_this_context+1)] == tree$context) == length(tree$context)) # this gets reverse time from y
        result = tree
      ## this could be a smaller context and the tree actually goes deeper, so check the children
      if (length(tree$child) > 0)
        for (ind in 1:length(tree$child))
        {
          possible.tree = find.underlying.context.in.tree(y,tree$child[[ind]])
          
          if (!is.null(possible.tree))
            result = possible.tree
        }
      
    }
  }
  return(result)  
}