#
#  The random forest function
#
rforest <- function(formula, data, weights = NULL, method, parms = NULL, control = NULL, ncand, ntrees, subsample = 1, redmem = FALSE){
  subsamp <- subsample
  # Collect the arguments for rpart in a list
  rpart_args <- list('formula' = formula,
                     'data' = substitute(data[sample.int(nrow(data), size = subsamp*nrow(data), replace = subsamp < 1),]),
                     'weights' = unname(as.list(match.call())[['weights']]),
                     'method' = method,
                     'control' = control,
                     'parms' = parms,
                     'ncand' = ncand,
                     'seed' = substitute(x),
                     'redmem' = redmem)
  
  # Create the random forest
  rf.fit <- lapply(1:ntrees, function(x) {
                              set.seed(x)
                              do.call('rpart', rpart_args)})
  
  # Make the random forest of the class 'rf' and subclass 'list'
  class(rf.fit) <- append('rf', class(rf.fit))
  
  # Return the random forest
  return(rf.fit)  
}



