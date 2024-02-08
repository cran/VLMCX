print.VLMCX <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
  ## Purpose: "print" Method for "VLMCX" objects
  ## ----------------------------------------------------------------------
  ## Author: Adriano Zanin Zambom, Date: 22 Mar 2022
  if(!inherits(x, "VLMCX")) stop("first argument must be a \"VLMCX\" object; see ?VLMCX")
  
  
  cat("\n", sQuote('VLMCX')," a Variable Length Markov Chain with Exogenous Covariates;\n",
      "\t alphabet '", sort(unique(x$y)), "', |alphabet| = ",length(unique(x$y)),
      ", n = ",length(x$y),".\nCall: ",deparse(x$call),
      "\n")
  
  df <- data.frame(AIC = format(AIC(x), digits = digits),
                   BIC = format(BIC(x), digits = digits),
                   LogLik = format(LogLik(x), digits = digits),
                   tree.depth = x$tree.depth
                   )

  print(df, row.names = FALSE)
}

summary.VLMCX <- function(object, ...)
{
  print.VLMCX(object)
}
