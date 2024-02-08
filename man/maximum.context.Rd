\name{maximum.context}
\alias{maximum.context}


\title{
Maximum Context Tree
}
\description{
Build the largest context tree, which is the biggest context tree such that all elements in it have been observed at least \code{n.min} times.
}
\usage{
maximum.context(y, X, max.depth = 5, n.min = 5)
}
\arguments{
  \item{y}{
	a "time series" vector (numeric, charachter, or factor)
}
  \item{X}{
	Numeric matrix of predictors with rows corresponding to the y observations (over time) and columns corresponding to covariates.
}
  \item{max.depth}{
	Maximum depth of the desired tree.
}
  \item{n.min}{
	Minimum number of observations per coefficient to be estimated.
}
}

\value{ maximum.context returns an object of class \code{"VLMCX"}. The generic functions \code{\link{coef}}, \code{\link{AIC}},\code{\link{BIC}}, \code{\link{draw}}, and \code{\link{LogLik}} extract various useful features of the value returned by \emph{VLMCX}.
	
	An object of class \code{"VLMCX"} is a list containing at least the following components:
\item{y}{the time series data corresponding to the states inputed by the user.}
\item{X}{the time series covariates data inputed by the user.}
\item{tree}{the estimated rooted tree estimated by the algorithm. Each node contains the \code{context}, the intercept (\code{alpha}) and regression parameters (\code{beta}) corresponding to the covariates of that regression and a list \code{child}, whose entries are \code{nodes} with the same structure.}
\item{LogLik}{the log-likelihood of the data using the estimated context tree.}
\item{baseline.state}{the state used as a baseline fore the multinomial regression.}}

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
}
}

