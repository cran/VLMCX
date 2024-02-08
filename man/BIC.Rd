\name{BIC}
\alias{BIC}


\title{
Bayesian Information Criteria for for \code{VLMCX} objects that compose Variable Length Markov Chains with Exogenous Covariates
}
\description{
Computes the Bayesian Information Criteria for the data using the estimated parameters of the multinomial logistic regression in the VLMCX fit.
}
\usage{
BIC(fit)
}
\arguments{
  \item{fit}{
	a betaVLMC object.
}
}

\value{ a numeric value with the corresponding BIC.

}

\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}



\examples{



\donttest{
set.seed(1)
n = 1000
d = 2

X = cbind(rnorm(n), rnorm(n))
p = 1/(1 + exp(0.5 + -2*X[,1] - 3.5*X[,2]))

y = c(sample(1:0,1), rbinom(n,1, p)) 

fit = maximum.context(y[1:n], X, max.depth = 3, n.min = 25)
draw(fit)
BIC(fit)
##[1] 696.0343

fit = VLMCX(y[1:n], X, alpha.level = 0.001, max.depth = 3, n.min = 25)
draw(fit)
BIC(fit)
##[1] 588.9432
}
}
