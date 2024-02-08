

update.node.from.tree <- function(tree, node)UseMethod("update.node.from.tree")

  ##########################################################################################################
  ##########################################################################################################
  ##########################################################################################################
  ## 
update.node.from.tree.default <- function(tree, node)
{
      tree_aux = tree
      its_root = FALSE
      if (length(node$context) == length(tree$context))
        if (sum(node$context == tree$context) == length(node$context)) ## if node is the root, then return the root as it needs update
          its_root = TRUE
          
      if (its_root == FALSE)
        for (level in 1:length(node$context))
        {
          ## just filling in so we get the right type of state (numeric/factor/character)
          for (ind in 1:length(tree_aux$child))
          {
            
            if (tree_aux$child[[ind]]$context[level] == node$context[level]) ## se which index to go down the tree
              get_this_child = ind
          }
            
            tree_aux = tree_aux$child[[get_this_child]]
        }
    
    return(tree_aux)
}







