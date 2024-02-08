\name{context.algorithm}
\alias{context.algorithm}


\title{
Context Algorithm using exogenous covariates
}
\description{
	Prunes the given tree according to the significance of the covariates and the contexts that are determined by a multinomial regression.
	}
\usage{
context.algorithm(fit, node, alpha.level = 0.05, max.depth = 5, n.min = 5, trace = FALSE)
}
\arguments{
  \item{fit}{
	a VLMCX object
}
  \item{node}{
	The top most node up to which the prunning is allowed.
}
  \item{alpha.level}{
	the alpha level for rejection of each hypothesis in the algorithm.
}
  \item{max.depth}{
	the maximum depth of the initial "maximal" tree.
}
  \item{n.min}{
	minimum number of observations for each parameter needed in the estimation of that context
}
  \item{trace}{
	if trace == TRUE then information is printed during the running of the prunning algorithm.
}
}

\value{ context.algorithm returns an object of class \code{"VLMCX"}. The generic functions \code{\link{coef}}, \code{\link{AIC}},\code{\link{BIC}}, \code{\link{draw}}, and \code{\link{LogLik}} extract various useful features of the fitted object returned by \emph{VLMCX}.
	
	An object of class \code{"VLMCX"} is a list containing at least the following components:
\item{y}{the time series data corresponding to the states inputed by the user.}
\item{X}{the time series covariates data inputed by the user.}
\item{tree}{the estimated rooted tree estimated by the algorithm. Each node contains the \code{context}, the intercept (\code{alpha}) and regression parameters (\code{beta}) corresponding to the covariates of that regression and a list \code{child}, whose entries are \code{nodes} with the same structure.}
\item{LogLik}{the log-likelihood of the data using the estimated context tree.}
\item{baseline.state}{the state used as a baseline fore the multinomial regression.}
}

\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}





\examples{
\donttest{
n = 500

X = cbind(rnorm(n), rnorm(n))
y = rbinom(n,1,.5)

fit = maximum.context(y, X, max.depth = 3)
pruned.fit = context.algorithm(fit, fit$tree)
draw(pruned.fit)
}
}