#
#  The random forest function
#
rforest <- function(formula, data, weights = NULL, method, parms = NULL, control = NULL, ncand, ntrees, subsample = 1){
  subsamp <- subsample
  # Collect the arguments for rpart in a list
  rpart_args <- list('formula' = formula,
                     'data' = substitute(data[sample.int(nrow(data), size = subsamp*nrow(data), replace = TRUE)]),
                     'weights' = unname(as.list(match.call())[['weights']]),
                     'method' = method,
                     'control' = control,
                     'parms' = parms,
                     'ncand' = ncand,
                     'seed' = substitute(x))
  
  # Create the random forest
  return(lapply(1:ntrees, function(x) {
    set.seed(x)
    do.call('rpart', rpart_args)}))  
}



