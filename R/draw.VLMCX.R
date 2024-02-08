draw <- function(fit, title = "VLMCX Context Tree", print.coef = TRUE)UseMethod("draw")

## draws a rooted tree from a VLMCX object with circles representing the nodes and includes the names of the contexts in the drawing
draw.VLMCX <- function(fit, title = "VLMCX Context Tree", print.coef = TRUE)
{

  alphabet = sort(unique(fit$y))
  n.alphabet = length(alphabet)

  cat("\n\n=========================================")
  cat("\n", title ,"\n")
  ## this is recursive, so it will write all context nodes
  n.children = length(fit$tree$child)
  if (n.children > 0)
  {
    for (ind in 1:n.children)
      write.node(node = fit$tree$child[[ind]], alphabet = alphabet, print.coef = print.coef, siblings = ifelse(n.children-ind >0,TRUE, FALSE))  ## recursively writes the node content in the R console
    if (print.coef == TRUE)
      cat("\n\n Note: beta matrices rows correspond to time and columns correspont to the covariate index.\n\n")
  } else
    cat("\n\n no contexts in the tree.\n\n")
  cat("\n=========================================")
  
  
  
  
  ######################################################################
  ##### draw in plot form #############################################

    ##### draw root ###################################
    max_len = fit$tree.depth

    r = 4 ## radius of circles
    spacing = 2*r+2
    
    plot(-100,-100, xlim = c(0, n.alphabet^max_len*spacing), ylim = c(-10,max_len*15), xlab = "", ylab = "", xaxt='n', yaxt='n', main = title)
    circle(n.alphabet^max_len*spacing/2,max_len*15,r)
    text(n.alphabet^max_len*spacing/2,max_len*15,"x")  ## writes the context
    n.children = length(fit$tree$child)
    if (n.children > 0)
    for (ind in 1:n.children)
    {
      current_x = n.alphabet^max_len*spacing/2
      space_under_this_x = n.alphabet^max_len*spacing#/n.alphabet
      new_x = current_x - space_under_this_x/2 + (ind*2-1)*space_under_this_x/(n.alphabet*2)
      lines(c(current_x,new_x), c(max_len*15-r,max_len*15-15+r)) ## draws lines to connect parent and children nodes
      draw_node(new_x, max_len*15-15, fit$tree$child[[ind]], r = r, spacing = spacing, max_len = max_len, n.alphabet = n.alphabet)
    }



}














