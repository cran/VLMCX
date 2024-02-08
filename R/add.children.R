add.children <- function(node, y, d, max.depth = 5, n.min = 5)UseMethod("add.children")

#######################################################################
## add.children is a recursive function that adds nodes (children) down 
## the rooted tree as long as max.depth has not been reached and 
## there is a minimum number of observations of the context in the data
## - called by "maximum.context()"
#######################################################################
add.children.default <- function(node, y, d, max.depth = 5, n.min = 5)
{

    alphabet = sort(unique(y))
    
    this.node = node

    index.child = 0
    for (index in 1:length(alphabet))
    {
      if (length(this.node$context) == 1)
      {
        if (this.node$context == "x") ## when we are at the root we can possibly add nodes that correspond to the alphabet
          possible.child = alphabet[index]
        else
          possible.child = c(this.node$context, alphabet[index]) ## the child is context wu, where w is the parent and u is the alphabet
      }
      else
        possible.child = c(this.node$context, alphabet[index])
      
      if (length(possible.child) <= max.depth) ## do not add if max.depth has been reached
      {
        n.occur = vecIn(y,rev(possible.child))
        if ((length(n.occur)!=0))
          if (length(n.occur) >= n.min*(1+d*length(rev(possible.child))*(length(alphabet)-1))) ## check if there is a minimum number of observations of the context in the data
          {
            index.child = index.child + 1
            
            new_node = NULL
            new_node$context = possible.child
            new_node$alpha = rep(0,length(alphabet)-1)
            new_node$beta = array(0,c(length(new_node$context), d, length(alphabet)-1)) ## steps, d, alphabet
            new_node$child = list()
            
            ### RECURSION: call the same function to add the child node (new_node) to the current one
            new_node = add.children(new_node, y, d = d, max.depth = max.depth, n.min = n.min)
            
            this.node$child[[index.child]] = new_node
          }			
      }
    }
    
    return(this.node)

}