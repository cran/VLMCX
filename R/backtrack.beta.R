

backtrack.beta <- function(result_fit, node, alpha.level = 0.05, trace = TRUE)UseMethod("backtrack.beta")

  ##########################################################################################################
  ##########################################################################################################
  ##########################################################################################################
  ## find the children whose betas were cut - only return those without grandchildren (nodes with grandchild cant be prunned)
 
backtrack.beta.default <- function(result_fit, node, alpha.level = 0.05, trace = FALSE)
{
  X = result_fit$X
  y = result_fit$y
  alphabet = sort(unique(y))
  d = ifelse(is.null(dim(X)), 1, dim(X)[2])
  
  need.to.backtrack.beta = TRUE
    while ((need.to.backtrack.beta == TRUE) && (!is.null(node$beta)))
    {
      result_fit_H0 = result_fit
      
      if (dim(node$beta)[1] == 1)
        node$beta = NULL
      else
        node$beta = array(0,c(dim(node$beta)[1]-1, d, length(alphabet)-1)) ## steps, d, alphabet
      result_fit_H0$tree = update.node(result_fit_H0$tree, node)
      result_fit_H0$tree = estimate(result_fit_H0$tree, y, X)
      p.value = 1-pchisq(-2*(LogLik(result_fit_H0) - LogLik(result_fit)), d*(length(alphabet)-1))
      if (p.value > alpha.level)
      {
        if (trace == TRUE)
          if (is.null(node$beta))
            cat("\n pruning beta parameters sequentially for node ", node$context, ": now beta depth 0")
        else
          cat("\n pruning beta parameters sequentially for node ", node$context, ": now beta depth ", dim(node$beta)[1])
        result_fit = result_fit_H0
      }
      else
        need.to.backtrack.beta = FALSE
    }
    return(result_fit)
}







