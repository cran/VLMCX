  ######################################################################
  ##### draw in text form #############################################
  ## siblings is a vector of TRUE or FALSE indicating if there are other siblings down the list
  ## if there are siblings, we need to print "|" otherwise we do not print "|"
  write.node <- function(node, alphabet, print.coef, siblings)
  {
    if (print.coef == TRUE)
    {
      if (!is.null(node$alpha))
      {
        cat("\ncontext: ", node$context)
        for (i in 1:(length(alphabet)-1))
        {
          if (length(alphabet) > 2)
            cat("\nestimated param. for next symbol ", alphabet[i+1])
          cat("\n alpha: ", node$alpha[i], "\n")
          if (!is.null(node$beta))
          {
            cat(" beta\n")
            print(node$beta[,,i])
          }
        }
      }
      

      n.children = length(node$child)
      if (n.children > 0)
        for (ind in 1:n.children)
          write.node(node = node$child[[ind]], alphabet = alphabet, print.coef = print.coef, siblings = TRUE) ## siblings not used here
    }
    else ## print.coef = FALSE
    {
      if (length(node$context) > 1)
      for (j in 1:(length(node$context)-1))
      {
        if (siblings[j] == TRUE)
          cat("|   ")
        else
          cat("    ")
      }
      cat("+",rep("-",2), sep = "")
      cat("[",node$context[length(node$context)],"]\n", sep = "")

      n.children = length(node$child)
      if (n.children > 0)
        for (ind in 1:n.children)
          write.node(node = node$child[[ind]], alphabet = alphabet, print.coef = print.coef, siblings = c(siblings,ifelse(n.children-ind > 0,TRUE, FALSE)))
    }
  }
  