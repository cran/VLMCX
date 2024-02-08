\name{predict}
\alias{predict}


\title{
Prediction of the next state of the Markov Chain/Categorical Time series
}
\description{
Uses the estimated coefficients from a VLMCX object to estimate the next state of the Markov Chain either using new data or the original data with which the model was fit.
}
\usage{
predict(fit, new.y = NULL, new.X = NULL)
}
\arguments{
  \item{fit}{
	a VLMCX object.
  }
  \item{new.y}{
	the new sequency of observations of the "time series" as a vector (numeric, charachter, or factor). The values of y.new must be of the same type as the ones used to fit the VLMCX object. If new.y is NULL (or if new.X is NULL) the algorithm uses the original data used to fit the VLMCX object.
  }
  \item{new.X}{
	Numeric matrix of predictors with rows corresponding to the new.y observations (over time) and columns corresponding to covariates.
  }
}

\value{ a value of the predicted symbol of the next state of the Markoc Chain corresponding to the type of the imput (numeric, charachter, or factor).
}

\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}

\examples{

\donttest{
set.seed(1)
n = 1000

X = cbind(rnorm(n))

y = rbinom(n,1,.5)
fit = maximum.context(y, X)

## using the original data
predict(fit) 

## using new data
predict(fit, new.y = c(0,0,1,0,0), new.X = c(2.3, 1.1, -.2, -3,1))
}
}
