#
#  The random forest function
#
rforest <- function(formula, data, method, weights = NULL, parms = NULL, control = NULL, ncand, ntrees, subsample = 1, redmem = FALSE){

  # Collect the arguments for rpart in a list
  rpart_args <- list('formula' = formula,
                     'method' = method,
                     'weights' = substitute(weights),
                     'parms' = parms,
                     'control' = control,
                     'ncand' = ncand,
                     'redmem' = redmem,
                     'seed' = substitute(rf_id))
  
  # Create the random forest
  rf_fit <- vector('list', length = ntrees)
  for (rf_id in seq_len(ntrees)) {
    indx_smpl <- sample(seq_len(nrow(data)), size = subsample * nrow(data), replace = TRUE)
    data_smpl <- data[indx_smpl, ]
    rf_fit[[rf_id]] <- do.call('rpart', c(list(data = quote(data_smpl)), rpart_args))
  }
  
  # Make the random forest of the class 'rf' and subclass 'list'
  class(rf_fit) <- append('rforest', class(rf_fit))
  
  # Return the random forest
  return(rf_fit)  
}



