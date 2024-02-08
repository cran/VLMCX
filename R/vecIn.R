#########################################################################################
## returns all the positions in a where b is found.
#########################################################################################

vecIn <- function(a, b)UseMethod("vecIn")

vecIn.default <- function(a, b)
{
	which(Reduce('+', lapply(seq_along(y <- lapply(b, '==', a)), function(x){y[[x]][x:(length(a)-length(b)+x)]})) == length(b))
}   
   
   
   
   
   
   