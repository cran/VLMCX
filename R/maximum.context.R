maximum.context <- function(y, X, max.depth = 5, n.min = 5)UseMethod("maximum.context")

#####################################################################################################################
## builds the largest (deepest) rooted tree possible (up to max.depth) by including nodes that correspond to 
## contexts that appear in the data at least n.min number of times (n.min) per coefficient to be estimated (see details
## in Zambom et al. 2022)
#####################################################################################################################
maximum.context.default <- function(y, X, max.depth = 5, n.min = 5)
{
  
  if (n.min < 2)
    stop("\n\n n.min must be an integer larger or equal to 2 ","\n\n") 
  if (max.depth < 1)
    stop("\n\n n.min must be an integer larger or equal to 1 ","\n\n") 
  

  fit = NULL
  fit$y = y
  fit$X = X

  
  
  
  node = NULL
  node$context = "x"  ## this is the root
  node$alpha = NULL
  node$beta = NULL
  node$child = list()
  ## call to add children to the root, but the add.children recursive function will add all children
  node = add.children(node, y, d = ifelse(is.null(dim(X)), 1, dim(X)[2]), max.depth = max.depth, n.min = n.min)
  
  # return some information about the tree
  fit$tree = node
  fit$tree = VLMCX_sorttree(fit$tree)
  fit$tree = estimate(fit$tree, y, X)
  fit$tree.depth = VLMCX_depth(fit$tree)
  fit$LogLik = LogLik.VLMCX(fit)
  fit$baseline.state = sort(unique(fit$y))[1]
  
  fit$call <- match.call()
  class(fit) <- "VLMCX"

  return(fit)
}