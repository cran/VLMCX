\name{coef}
\alias{coef}


\title{
Coefficients from a Variable Length Markov Chain with Exogenous Covariates
}
\description{
Extracts the estimated coefficients from a VLMCX object for a specific context (sequence of states in the past used to predict the next state/symbol of the chain).
}
\usage{
coef(fit, context)
}
\arguments{
  \item{fit}{
	a VLMCX object.
}
  \item{context}{
	the context whose coefficients are desired.
}
}

\value{ an object with two items: 
	\item{\code{alpha}}{a vector with coefficients corresponding to the intercept for the transition into the states in the state space of \code{y}.} 
	
	\item{\code{beta}}{a 3 dimensional-array of estimated coefficients corresponding to [steps in the past, number of covariate, symbol (in the state space) to transition into].}

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

y = rbinom(n,1,.5)
fit = maximum.context(y, X)

coef(fit, c(0,0,1,0)) 
## context in the order: y_{t-1} = 0, y_{t-2} = 0, y_{t-3} = 1, y_{t-4} = 0
}
}

