\name{simulate}
\alias{simulate}


\title{
Simulate a Variable Length Markov Chain with Exogenous covariates
}
\description{
Simulate the states of a Markov Chain based on VLMCX model.
}
\usage{
simulate(VLMCXtree, nsim = 500, X = NULL, seed = NULL, n.start = 100)
}
\arguments{
  \item{VLMCXtree}{
	a VLMCX tree (a VLMCX object can also be used, in which case its tree is used).
}
  \item{nsim}{
	non-negative integer, giving the length of the result.
}
  \item{X}{
	A vector or matrix of exogenous variables. If vector, its length must be equal to nsim+n.start, if matrix, its first dimension must be of length nsim+n.start, if NULL a univariate independent and identically distributed Normal vector is used.
}
  \item{seed}{
	random seed initializer.
}
  \item{n.start}{
	the number of initial values to be discarded (because of initial effects).
}

}

\value{ a vector with nsim simulated states.

}

\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}

\examples{

\donttest{
tree = NULL
  tree$context = "x"  ## this is the root
  tree$alpha = NULL
  tree$beta = NULL
  tree$child = list()
  this_child = NULL
  this_child$context = "left"
  this_child$alpha = 0.5
  this_child$child = list()
  tree$child[[1]] = this_child

  this_grandchild = NULL
  this_grandchild$context = c("left", "left")
  this_grandchild$alpha = 0.6
  this_grandchild$beta = array(c(1.9, 1.6, 2.6, -1.6),c(2, 2, 1)) ## steps, d, alphabet
  this_grandchild$child = list()

  tree$child[[1]]$child[[1]] = this_grandchild

  this_other_grandchild = NULL
  this_other_grandchild$context = c("left", "right")
  this_other_grandchild$alpha = -0.6
  this_other_grandchild$beta = array(c(-1.3, -1.5, 2.3, -1.2),c(2, 2, 1)) 
  this_other_grandchild$child = list()

  tree$child[[1]]$child[[2]] = this_other_grandchild

  other_child = NULL
  other_child$context = "right"
  other_child$alpha = -0.7
  other_child$beta = array(c(1,-.3),c(1, 2, 1)) ## steps, d, alphabet
  other_child$child = list()
  tree$child[[2]] = other_child

  set.seed(1)
  X = cbind(rnorm(1100), rnorm(1100))
  simulated.data = simulate(tree, nsim = 1000, X, seed = 1, n.start = 100)
  fit = VLMCX(simulated.data$y, simulated.data$X, alpha.level = 0.001, 
                 max.depth = 4, n.min = 20, trace = TRUE)
  draw(fit)
  fit
}
}
