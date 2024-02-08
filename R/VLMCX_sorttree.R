VLMCX_sorttree <- function(VLMCXtree)UseMethod("VLMCX_sorttree")

##### recursive function that
##### sorts the children of each node to be in alphabetical order (corresponding to the order() function)
VLMCX_sorttree.default <- function(VLMCXtree)
{
  node = VLMCXtree

  n.children = length(node$child)
  if (n.children <= 1)
    ordered_tree = node
  else
  {
    ordered_tree = node

    children.names = NULL
    for (ind in 1:n.children)
      children.names[ind] = node$child[[ind]]$context[length(node$child[[ind]]$context)]
    
    children.names.order = order(children.names) ## get the children names in alphabetical order
    
    for (ind in 1:n.children)
    {
      ordered_tree$child[[ind]] = node$child[[children.names.order[ind]]] ## assign the ordered children names
      ordered_tree$child[[ind]] = VLMCX_sorttree(ordered_tree$child[[ind]]) ## recursive call for the child nodes
    }
  }
  
  return(ordered_tree)
}