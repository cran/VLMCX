\name{draw}
\alias{draw}


\title{
Draw the Variable Length Markov Chain estimated model
}
\description{
Draws the rooted tree corresponding to the estimated contexts in a \code{VLMCX} object.
}
\usage{
draw(fit, title = "VLMCX Context Tree", print.coef = TRUE)
}
\arguments{
  \item{fit}{
	a VLMCX object.
}
  \item{title}{
	the title in the graph.
}
  \item{print.coef}{
	It TRUE the algorithm prints in the console the list of all contexts and their corresponding alpha and beta coefficients for the multinomial regression. If FALSE, the algorithm prints in the console a text version of the rooted context tree.
}
}
\details{
	The graph contains circles corresponding to the estimated nodes of the contexts estimated by the algorithm but does not include the structure and covariate parameter vectors.
}

\value{ No return value, called for plotting only.

}

\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}

\examples{

\donttest{
n = 1000
d = 2
set.seed(1)
X = cbind(rnorm(n), rnorm(n))

y = rbinom(n,1,.2)
fit = maximum.context(y, X)

draw(fit)

fit = VLMCX(y, X, alpha.level = 0.0001, max.depth = 3, n.min = 15, trace = TRUE)
draw(fit)

draw(fit, print.coef = FALSE)
}


}

