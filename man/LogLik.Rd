\name{LogLik}
\alias{LogLik}


\title{
Log Likelihood for Variable Length Markov Chains with Exopgenous Covariates
}
\description{
Computes the log-likelihood of the data using the estimated parameters of the multinomial logistic regression based on contexts of variable length, that is, a finite suffix of the past, called "context", is used to predict the next symbol, which can have different lengths depending on the past observations themselves.
}
\usage{
LogLik(fit)
}
\arguments{
  \item{fit}{
	a VLMCX object.
}
}

\value{ a numeric value with the corresponding log-likelihood

}

\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}

\examples{


\donttest{
n = 1000
d = 2

X = cbind(rnorm(n), rnorm(n))

y = rbinom(n,1,.5)
fit = maximum.context(y, X)

LogLik(fit)
}
}
