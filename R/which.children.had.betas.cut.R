

which.children.had.betas.cut <- function(node)UseMethod("which.children.had.betas.cut")

  ##########################################################################################################
  ##########################################################################################################
  ##########################################################################################################
  ## find the children whose betas were cut - only return those without grandchildren (nodes with grandchild cant be prunned)
 
which.children.had.betas.cut.default <- function(node)
{
    betas_cut = NULL
    no_grandchildren = NULL
    #cat("# children: ", length(node$child))
    if (length(node$child) > 0)
    {
      for (ind in 1:length(node$child)) ## check which chinden had their betas cut
      {
        if (is.null(node$child[[ind]]$beta))
          betas_cut = c(betas_cut, ind)
        else if (dim(node$child[[ind]]$beta)[1] < length(node$child[[ind]]$context))
          betas_cut = c(betas_cut, ind)
      }
      no_grandchildren = NULL ## which child has children (where there are grandchildren)
      for (ind in 1:length(node$child)) ## find children without grandchildren
      {
        if (length(node$child[[ind]]$child) == 0)
          no_grandchildren = c(no_grandchildren, ind)
      }
    }
    return(intersect(betas_cut, no_grandchildren))
}







