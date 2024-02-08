estimate <- function(VLMCXtree, y, X)UseMethod("estimate")


#################################################################################
# estimates the parameters in a given tree structure based on the data
#################################################################################
estimate.default <- function(VLMCXtree, y, X)
{
  this.node = VLMCXtree
  
  alphabet = sort(unique(y))
  d = ifelse(is.null(dim(X)), 1, dim(X)[2]) ### number of covariates
  
  is_root = FALSE
  if (length(this.node$context) == 1)
    if (this.node$context == "x") ## when we are at the root
      is_root = TRUE

  if (is_root == FALSE) ## there are nodes with contexts to be estimated
  {
    
    if (length(this.node$child) == length(alphabet)) ## has all children: it is not a final node so there is no estimation here
    {
      this.node$alpha = NULL
      this.node$beta = NULL
    } else
    {
      if (length(this.node$child) == 0) ## it is a leaf = context final node
      {
        this_context = rev(this.node$context)
        
        where = vecIn(y,this_context) ## find where this context happens in the data
      } else ## not final node: need to check if it is a context for SOME children. If it has all children, then this is not a context.
      {
        ## get the "other" elem which are not children *
        alphabet_missed = alphabet
        for (ind in 1:length(this.node$child))
        {
          alphabet_missed = alphabet_missed[-which(this.node$child[[ind]]$context[length(this.node$child[[ind]]$context)] == alphabet_missed)]
        }

        # use each other elem to build the dataset to estimate beta_u*
        where = NULL
        for (item in alphabet_missed)
        {
          this_context = rev(c(this.node$context,item))
          
          where = c(where, vecIn(y,this_context))
        }
        
      }
    #################################################################################
    ##  estimate
      y_t0 = y[where+length(this_context)]
      if (length(unique(y_t0)) >= length(alphabet)) ## enough data to estimate the parameters
        if (!is.null(this.node$beta)) ## check if beta-context has this final node
        {			
          #### ATENTION: new_context is in time order (NOT reverse)
          x_context = matrix(0,length(where), d*dim(this.node$beta)[1]) ## all variables needed to predict this prob
          for (j in 1:dim(this.node$beta)[1])
          {
            if (d == 1) ## univariate X: vector
              x_context[,(j*d-d+1):(j*d)] <- X[where+j-1+length(this_context)-dim(this.node$beta)[1]] ## x_context is in time order (NOT reverse)
            else ## multivariate X: matrix
              x_context[,(j*d-d+1):(j*d)] <- X[where+j-1+length(this_context)-dim(this.node$beta)[1],]
          }
          
          fit = multinom(y_t0~x_context, trace = FALSE)			
          
          #########################################
          # store the estimated parameters
          if (length(alphabet) == 2) ## only 2 states mean logistic regression
          {
            this.node$alpha = as.numeric(summary(fit)$coefficients)[1]
            for (ind2 in 1:dim(this.node$beta)[1])
              this.node$beta[dim(this.node$beta)[1]+1-ind2, ,1] = as.numeric(summary(fit)$coefficients)[-1][(d*(ind2-1)+1):(d*(ind2-1) + d)]
          }
          else ## more than 2 states means we need to store the alphas and betas for multiple response states
            for (ind_alphabet in 1:(length(alphabet)-1))
            {
              this.node$alpha[ind_alphabet] = as.numeric(summary(fit)$coefficients[ind_alphabet,])[1]
              for (ind2 in 1:dim(this.node$beta)[1])
                this.node$beta[dim(this.node$beta)[1]+1-ind2, ,ind_alphabet] = as.numeric(summary(fit)$coefficients[ind_alphabet,])[-1][((ind2-1)*d+1):((ind2-1)*d+d)]
            }
        }		
        else ## beta is NULL: estimation is based on ly on alpha via multinomial regression with intercept only
        {
          fit = multinom(y_t0~1, trace = FALSE)
          if (length(alphabet) == 2)
            this.node$alpha = as.numeric(summary(fit)$coefficients)[1]
          else	
            for (ind_alphabet in 1:(length(alphabet)-1))
              this.node$alpha[ind_alphabet] = as.numeric(summary(fit)$coefficients[ind_alphabet,])[1]
        }
    }  
    
  }

  #################################################################################3
  ## recursive call to estimate params of the children
  if (length(this.node$child) > 0)
    for (ind in 1:(length(this.node$child)))
      this.node$child[[ind]] = estimate(this.node$child[[ind]], y, X)
  
  return(this.node)
}




