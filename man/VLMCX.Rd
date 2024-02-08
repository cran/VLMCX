\name{VLMCX}
\alias{VLMCX}


\title{
Variable Length Markov Chain with Exogenous Covariates
}
\description{
	Estimates a Variable Length Markov Chain model, which can also be seen as a categorical time series model, where exogenous covariates can compose the multinomial regression that predicts the next state/symbol in the chain. This type of approach is a parsimonious model where only a finite suffix of the past, called "context", is enough to predict the next symbol. The length of the each context can differ depending on the past observations themselves.
	}
\usage{
VLMCX(y, X, alpha.level = 0.05, max.depth = 5, n.min = 5, trace = FALSE)
}
\arguments{
  \item{y}{
	a "time series" vector (numeric, charachter, or factor)
}
  \item{X}{
	Numeric matrix of predictors with rows corresponding to the y observations (over time) and columns corresponding to covariates.
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
\details{
The algorithm is a backward selection procedure that starts with the maximal context, which is the biggest context tree such that all elements in it have been observed at least \code{n.min} times. Then, final nodes (past most state in each context) are prunned according to the p-value from the likelihood ratio test for removing the covariates corresponding to that node and the significance of that node itself. The algorithm continues iteratively prunning until nodes cannot be prunned because the covariates or the node context itself is significant.
}
\value{ VLMCX returns an object of class \code{"VLMCX"}. The generic functions \code{\link{coef}}, \code{\link{AIC}},\code{\link{BIC}}, \code{\link{draw}}, and \code{\link{LogLik}} extract various useful features of the fitted object returned by \emph{VLMCX}.
	
	An object of class \code{"VLMCX"} is a list containing at least the following components:
\item{y}{the time series data corresponding to the states inputed by the user.}
\item{X}{the time series covariates data inputed by the user.}
\item{tree}{the estimated rooted tree estimated by the algorithm. Each node contains the \code{context}, the intercept (\code{alpha}) and regression parameters (\code{beta}) corresponding to the covariates of that regression and a list \code{child}, whose entries are \code{nodes} with the same structure.}
\item{LogLik}{the log-likelihood of the data using the estimated context tree.}
\item{baseline.state}{the state used as a baseline fore the multinomial regression.}
}
\references{
Zambom, Kim, Garcia (2022) Variable length Markov chain with exogenous covariates. Journal of Time Series Analysis, 43, 321-328.
}
\author{
Adriano Zanin Zambom <adriano.zambom@csun.edu> 
}

\examples{

\donttest{


 #### Example 1

 set.seed(1)
 n = 3000
 d = 2

 X = cbind(rnorm(n), rnorm(n))
 alphabet = 0:2
 y = sample(alphabet,2, replace = TRUE)
 for (i in 3:n)
 {
   if (identical(as.numeric(y[(i-1):(i-2)]), c(0,0)))
     value =  c(exp(-0.5 + -1*X[i-1,1] + 2.5*X[i-1,2]),      
            exp(0.5 + -2*X[i-1,1] - 3.5*X[i-1,2]))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(0,1)))
     value = c(exp(-0.5),     exp(0.5))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(0,2)))
     value = c(exp(1),     exp(1))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(2,0)))
     value = c(exp(0.5 + 1.2*X[i-1,1] + 0.5*X[i-1,2] + 2*X[i-2,1] + 1.5*X[i-2,2]),     
               exp(-0.5  -2*X[i-1,1] - .5*X[i-1,2] +1.3*X[i-2,1] + 1.5*X[i-2,2]))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(2,1)))
     value  = c(exp(-1 + -X[i-1,1] + 2.5*X[i-1,2]),       
                exp(0.1 + -0.5*X[i-1,1] - 1.5*X[i-1,2]))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(2,2)))
     value = c(exp(-0.5 + -X[i-1,1] - 2.5*X[i-1,2]),      
               exp(0.5 + -2*X[i-1,1] - 3.5*X[i-1,2]))
   else
     value = c(runif(1,0,3), runif(1,0,3))


     prob = c(1,value)/(1 + sum(value)) ## compute probs with baseline state probability
     y[i] = sample(alphabet,1,prob=prob)
    
 }
  
 fit = VLMCX(y, X, alpha.level = 0.001, max.depth = 4, n.min = 15, trace = TRUE)
 draw(fit) 
 ## Note the only context that was estimated but not in the true 
 ## model is (1): removing it or not does not change the likelihood, 
 ## so the algorithm keeps it.
 coef(fit, c(0,2))

 predict(fit,new.y = c(0,0), new.X = matrix(c(1,1,1,1), nrow=2))
 #[1] 0.2259747309 0.7738175143 0.0002077548

 predict(fit,new.y = c(0,0,0), new.X = matrix(c(1,1,1,1,1,1), nrow=3))
 # [1] 0.2259747309 0.7738175143 0.0002077548


 #### Example 2

 set.seed(1)
 n = 2000
 d = 1

 X = rnorm(n)
 alphabet = 0:1
 y = sample(alphabet,2, replace = TRUE)
 for (i in 3:n)
 {
   if (identical(as.numeric(y[(i-1):(i-3)]), c(0,0, 0)))
     value =  c(exp(-0.5 -1*X[i-1]  + 2*X[i-2]))
   else if (identical(as.numeric(y[(i-1):(i-3)]), c(0, 0, 1)))
     value = c(exp(-0.5))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(1,0)))
     value = c(exp(0.5 + 1.2*X[i-1] + 2*X[i-2] ))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(1,1)))
     value  = c(exp(-1 + -X[i-1] +2*X[i-2]))
   else
     value = c(runif(1,0,3))

     prob = c(1,value)/(1 + sum(value)) ## compute probs with baseline state probability
     y[i] = sample(alphabet,1,prob=prob)
 }
 fit = VLMCX(y, X, alpha.level = 0.001, max.depth = 4, n.min = 15, trace = TRUE)
 draw(fit) 
 coef(fit, c(1,0))



 #### Example 3

 set.seed(1)
 n = 4000
 d = 1

 X = cbind(rnorm(n))
 alphabet = 0:3
 y = sample(alphabet,2, replace = TRUE)
 for (i in 3:n)
 {
   if (identical(as.numeric(y[(i-1):(i-2)]), c(3, 3)))
     value =  c(exp(-0.5 -1*X[i-1] + 2.5*X[i-2]),      
            exp(0.5 -2*X[i-1] - 3.5*X[i-2]),
            exp(0.5 +2*X[i-1] + 3.5*X[i-2]))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(3, 1)))
     value = c(exp(-0.5 + X[i-1]),     
               exp(0.5 -1.4*X[i-1]), 
               exp(0.9 +1.4*X[i-1]))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(1, 0)))
     value = c(exp(-.5),     
               exp(.5), 
               exp(.8))
   else if (identical(as.numeric(y[(i-1):(i-2)]), c(1, 2)))
     value = c(exp(.4),     
               exp(-.5),
               exp(.8))
   else
     value = c(runif(1,0,3), runif(1,0,3), runif(1,0,3))


     prob = c(1,value)/(1 + sum(value)) ## compute probs with baseline state probability
     y[i] = sample(alphabet,1,prob=prob)
    
 }
  
 fit = VLMCX(y, X, alpha.level = 0.00001, max.depth = 3, n.min = 15, trace = TRUE)
 ## The context (0, 1) was not identified because the 
 draw(fit) 
 coef(fit, c(3,1))

}


}