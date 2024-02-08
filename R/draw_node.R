

draw_node <- function(x, y, node, r, spacing, max_len, n.alphabet)UseMethod("draw_node")

## recursive function going through the nodes of the rooted tree and drawing 
## circles representing the nodes at the location x, y
draw_node <- function(x, y, node, r, spacing, max_len, n.alphabet)
{
    level = length(node$context)+1
    circle(x,y,r)
    text(x,y,node$context[length(node$context)]) ## writes the context
    
    n.children = length(node$child)
    if (n.children > 0)
      for (ind in 1:n.children)
      {
        space_under_this_x = n.alphabet^max_len*spacing/(n.alphabet^level)*n.alphabet  ## spacing for the drawing
        new_x = x - space_under_this_x/2 + (ind*2-1)*space_under_this_x/(n.alphabet)/2
        lines(c(x,new_x), c(y-r,y-15+r))  ## draws lines to connect parent and children nodes
        draw_node(new_x, y-15, node$child[[ind]], r = r, spacing = spacing, max_len = max_len, n.alphabet = n.alphabet)
      }
}





