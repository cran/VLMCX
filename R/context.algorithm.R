

context.algorithm <- function(fit, node, alpha.level = 0.05, max.depth = 5, n.min = 5, trace = FALSE)UseMethod("context.algorithm")

 
context.algorithm.default <- function(fit, node, alpha.level = 0.05, max.depth = 5, n.min = 5, trace = FALSE)
{
    if (trace == TRUE && node$context[1] == "x")
    {
      draw(fit, title = "initial maximal context tree", print.coef = FALSE)  
      cat("\n Pruning steps: \n\n")
      n.children = length(fit$tree$child)
      if (n.children == 0)
        stop("\nContext algorithm cannot prune a tree with no nodes\n\n")
    }
  

	  X = fit$X
	  y = fit$y
      alphabet = sort(unique(y))
      d = ifelse(is.null(dim(X)), 1, dim(X)[2])
      result_fit = fit
      if (length(node$child) == 0) ## it is a leaf: test beta only
      {
        result_fit_H0 = result_fit
        node_H0 = remove.last.beta(node, node$context)
        result_fit_H0$tree = update.node(result_fit_H0$tree, node_H0)
        result_fit_H0$tree = estimate(result_fit_H0$tree, y, X)
        p.value = 1-pchisq(-2*(LogLik(result_fit_H0) - LogLik(result_fit)), d*(length(alphabet)-1))
        if (p.value > alpha.level)
        {
          result_fit = result_fit_H0
          node = node_H0
          if (trace == TRUE)
            if (is.null(node$beta))
              cat("\n context",node$context, " -> beta reduced to depth 0")
            else
              cat("\n context",node$context, " -> beta reduced to depth ", dim(node$beta)[1])
        }
      } else ## it has children
      {
        for (ind in 1:length(node$child)) ## send recursive to test children
        {
          result_fit = context.algorithm(result_fit, node$child[[ind]], alpha.level = alpha.level, max.depth = max.depth, n.min = n.min, trace = trace)
          node = update.node.from.tree(result_fit$tree, node)
        }

        
        betas_cut = which.children.had.betas.cut(node)
        
       
        if (length(betas_cut) >= 1)
        {
          ## test all alpha in betas_cut where there are no grandchildren
          betas_not_cut = setdiff(c(1:length(node$child)), betas_cut)

          ## I will only lump states together when there are states whose betas were cut
          ## but not in the case of only 1 state with beta cut when all other siblings are kept (no likelihood difference)
          while ((length(betas_cut) > 0) && (!((length(betas_cut) == 1) && (length(betas_cut) + length(betas_not_cut) == length(alphabet)) ))) ## need to try to group/lump
          {
            if (length(betas_cut) == 1)
              p.value = 0
            else if (length(betas_cut) + length(betas_not_cut) == length(alphabet)) ### test all 2x2 combinations, then do it again when at least one grouped
            {
              combinations = combn(betas_cut,2) ## all tests for possible cutting alpha
              p.value = rep(0, dim(combinations)[2])
            } else ## some were already cut, so now test one by one
              p.value = rep(0, length(betas_cut))
            test_stat = p.value ## I will save the test statistics for untiyng comparison, since then p.value rounded to 1, I cant compare 1 > 1
            
            for (ind_comb in 1:length(p.value))
            {
              if (length(betas_cut) == 1)
                keep = setdiff(c(1:length(node$child)), betas_cut)
              else if (length(betas_cut) + length(betas_not_cut) == length(alphabet))
                keep = setdiff(c(1:length(node$child)), combinations[,ind_comb])
              else
                keep = setdiff(c(1:length(node$child)), betas_cut[ind_comb])
              
              result_fit_H0 = result_fit
              node_H0 = node
              if (length(keep) == 0) ## remove all since there are some cut in betas_cut and none with grandchildren
                node_H0$child = list()
              else if (length(keep) > 0)
              {
                ######## reset the kids by including only those not in betas_cut #################################################
                node_H0$child = list()
                for (k in 1:length(keep))
                  node_H0$child[[k]] = node$child[[keep[k]]]
              }
              if (length(keep) < length(node$child)) ## only test if we wont keep some (if length(keep) == length(node$child) then we wont delete any)
              {
                node_H0$beta = array(0,c(length(node_H0$context), d, length(alphabet)-1)) ## steps, d, alphabet
                result_fit_H0$tree = update.node(result_fit_H0$tree, node_H0)
                result_fit_H0$tree = estimate(result_fit_H0$tree, y, X)
                
                if (length(betas_cut) + length(betas_not_cut) == length(alphabet))
                	df = (length(node$child) - (length(keep) + 1))*(1 + length(node$context)*d)*(length(alphabet)-1)
                else
                	df = (1 + length(node$context)*d)*(length(alphabet)-1)
                p.value[ind_comb] = 1-pchisq(-2*(LogLik(result_fit_H0) - LogLik(result_fit)), df)
                test_stat[ind_comb] = -2*(LogLik(result_fit_H0) - LogLik(result_fit)) ## the tests in this loop have the same df
                if ((ind_comb == 1) || (test_stat[ind_comb] < test_stat[ind_comb-1])) ## same as (p.value[ind_comb] > p.value[ind_comb-1])
                {
                  save_fit_H0 = result_fit_H0
                  save_node_H0 = node_H0
                  if (length(betas_cut) == 1)
                    save_combinations_deleted = betas_cut
                  else if (length(betas_cut) + length(betas_not_cut) == length(alphabet))
                    save_combinations_deleted = combinations[,ind_comb]
                  else
                    save_combinations_deleted = betas_cut[ind_comb]
                  save_keep = keep
                }

              }              
            }
            
            if (max(p.value) > alpha.level) ## need to group the combination with largest p-value together
            {
              if (trace == TRUE)
              {
                cutting = setdiff(c(1:length(node$child)), save_keep)
                if (length(cutting) == 1)
                  cat("\n pruning last node of context ", node$child[[cutting]]$context)
                else
                {
                  cat("\n pruning/lumping contexts ")
                  for (c in cutting)
                    cat(node$child[[c]]$context,",")
                  cat(" together into their parent ", node$context)
                }
              }
              result_fit = save_fit_H0
              node = save_node_H0
                
              betas_cut = which.children.had.betas.cut(node)
              
              
            } else ## these nodes had their betas cut but were not lumped together (pruned), so need to backtrack their betas
              betas_cut = NULL


          }


        }
        ######### backtrack-beta for the lumped states #########
        if (length(node$child) > 0) ## all children that were not lumped together need to backtrack beta
          for (ind in 1:length(node$child))
          {
            result_fit = backtrack.beta(result_fit, node$child[[ind]], alpha.level = alpha.level)
            node = update.node.from.tree(result_fit$tree, node)
          }
        if ((length(node$child) > 0) && (length(node$child) < length(alphabet))) ## some children were removed
        {
          result_fit = backtrack.beta(result_fit, node, alpha.level = alpha.level)
          node = update.node.from.tree(result_fit$tree, node)
        }
        if (length(node$child) == 0) ## it had children and they were prunned -> need to test this node again
        {
          if (node$context[1] == "x") ## if we pruned and the roor is called again, then it would print a repetition of "initial tree": so I set trace = FALSE
            result_fit = context.algorithm(result_fit, node, alpha.level = alpha.level, max.depth = max.depth, n.min = n.min, trace = FALSE)
          else
            result_fit = context.algorithm(result_fit, node, alpha.level = alpha.level, max.depth = max.depth, n.min = n.min, trace = trace)
          node = update.node.from.tree(result_fit$tree, node)
        }
      
      }
        
      return(result_fit)
}







