VLMCX_depth <- function(VLMCXtree)UseMethod("VLMCX_depth")


## recursive function that returns the maximum depth of the tree
VLMCX_depth.default <- function(VLMCXtree)
{
  
  if (length(VLMCXtree$child) == 0)
      result = 0
  else
  {
    depth.children = 0
    for (ind in 1:length(VLMCXtree$child))
      depth.children[ind] = VLMCX_depth(VLMCXtree$child[[ind]])
    result = 1 + max(depth.children)
      
  }
  return(result)
}