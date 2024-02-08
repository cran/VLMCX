\name{estimate}
\alias{estimate}


\title{
Estimation of Variable Length Markov Chain with Exogenous Covariates
}
\description{
Estimates the parameters of the multinomial logistic model in the VLMCX tree for each context in the tree.
}
\usage{
estimate(VLMCXtree, y, X)
}
\arguments{
  \item{VLMCXtree}{
	a VLMCX tree
}
  \item{y}{
	a "time series" vector (numeric, charachter, or factor)
}
  \item{X}{
	Numeric matrix of predictors with rows corresponding to the y observations (over time) and columns corresponding to covariates.
}
}

\value{ A tree from an object of type VLMCX. The tree contains the items
	\item{context}{the context, or sequence of symbols.}
	\item{alpha}{a vector with coefficients corresponding to the intercept for the transition into the states in the state space of \code{y}.}
	\item{beta}{a 3 dimensional-array of estimated coefficients corresponding to [steps in the past, number of covariate, symbol (in the state space) to transition into].}
	\item{child}{list whose entries are \code{nodes} with the same structure.}

}

\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}

\examples{

\donttest{
set.seed(1)
n = 4000
d = 2

X = cbind(rnorm(n), rnorm(n))
alphabet = 0:2 ### state space
y = sample(alphabet,2, replace = TRUE)
for (i in 3:n)
{
	if (identical(as.numeric(y[(i-1):(i-2)]), c(0,0)))
		value =  c(exp(-0.5 + -1*X[i-1,1] + 2.5*X[i-1,2]),      
		           exp(0.5 + -2*X[i-1,1] - 3.5*X[i-1,2]))
	else if (identical(as.numeric(y[(i-1):(i-2)]), c(0,1)))
		value = c(exp(-0.5),     exp(0.5))
	else
		value = c(runif(1,0,3), runif(1,0,3))


    prob = c(1,value)/(1 + sum(value)) ## compute probs with baseline state probability
    y[i] = sample(alphabet,1,prob=prob)
    
}
  

tree = NULL
tree$context = "x"  ## this is the root
tree$alpha = NULL
tree$beta = NULL
tree$child = list()
this_child = NULL
this_child$context = "0"
this_child$alpha = 0
this_child$child = list()
tree$child[[1]] = this_child

this_grandchild = NULL
this_grandchild$context = c(0, 0)
this_grandchild$alpha = 0
this_grandchild$beta = array(c(0,0,0,0),c(1, 2, 2)) ## steps, d, alphabet (state space)
this_grandchild$child = list()

tree$child[[1]]$child[[1]] = this_grandchild

this_other_grandchild = NULL
this_other_grandchild$context = c(0, 1)
this_other_grandchild$alpha = 0
this_other_grandchild$beta = NULL
this_other_grandchild$child = list()

tree$child[[1]]$child[[2]] = this_other_grandchild

estimate(tree, y, X)

fit = VLMCX(y, X, alpha.level = 0.0001, max.depth = 2, n.min = 15, trace = TRUE)
estimate(fit$tree, y, X)

}

}

