

get.alphabet.from.tree <- function(tree)UseMethod("get.alphabet.from.tree")

## recursive function
## counts the number of parameters in the entire beta-context tree: both alpha and beta parameters are counted
get.alphabet.from.tree <- function(tree)
{
    is_root = FALSE
    if (length(tree$context) == 1)
      if (tree$context == "x")
        is_root = TRUE
    if (is_root == TRUE)
    {
      alphabet = NULL
      if (length(tree$child) > 0)
        for (ind in 1:length(tree$child))
          alphabet = sort(unique(c(alphabet, get.alphabet.from.tree(tree$child[[ind]]))))
    } else if (length(tree$child) == 0) ## leaf
        alphabet = sort(unique(tree$context))
    else ## in the middle of the tree
    {
      alphabet = sort(unique(tree$context))
      for (ind in 1:length(tree$child))
        alphabet = sort(unique(c(alphabet, get.alphabet.from.tree(tree$child[[ind]]))))
    }
    return(alphabet)
}





