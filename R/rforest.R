#
# Random forest function
#
rforest <- function(formula, data, method, weights = NULL, parms = NULL, control = NULL, ncand, ntrees, subsample = 1, track_oob = FALSE, redmem = FALSE){
  
  # Some checks wrt the implementation of OOB error tracking
  if (track_oob) {
    if (! method %in% c('class', 'anova', 'poisson', 'gamma', 'lognormal')) stop('Tracking the OOB error is only implemented for the following methods: class, anova, poisson, gamma and lognormal.')
    if (method == 'class') if (length(unique(data[, as.character(formula[[2]])])) > 2) stop('Tracking the OOB error is only implemented for binary classification.')
  }

  # Collect the arguments for rpart in a list
  rpart_args <- list('formula' = formula,
                     'method' = method,
                     'weights' = substitute(weights),
                     'parms' = parms,
                     'control' = control,
                     'ncand' = ncand,
                     'redmem' = redmem,
                     'seed' = substitute(rf_id))
  
  # Create a list to save the individual trees in the random forest
  rf_fit <- vector('list', length = ntrees)
  
  # Initialize components to track the OOB error
  if (track_oob) {
    oob_err <- rep(NA, ntrees)
    pred_vec <- rep(0, nrow(data))
  }
  
  # Build the idividual trees
  for (rf_id in seq_len(ntrees)) {
    
    # Sample from the original data to build the tree
    indx_smpl <- sample(seq_len(nrow(data)), size = subsample * nrow(data), replace = TRUE)
    data_smpl <- data[indx_smpl, ]
    
    # Fit the tree on the sampled data
    rf_fit[[rf_id]] <- do.call('rpart', c(list(data = quote(data_smpl)), rpart_args))
    
    # Track the OOB error
    if (track_oob) {
      # Calculate aggregate predictions for random forest at current iteration (mean for regression, majority vote for classification)
      pred_vec <- ((rf_id - 1) * pred_vec / rf_id) + (predict(rf_fit[[rf_id]], newdata = data, type = 'vector') / rf_id)
      # Select the predictions for the OOB observations
      oob_obs <- setdiff(seq_len(nrow(data)), indx_smpl)
      pred_oob <- pred_vec[oob_obs]
      # Use the formula to extract the true values of the response variable
      lhs_form <- as.character(formula[[2]])
      if (length(lhs_form) == 1) {
        true_oob <- data[oob_obs, lhs_form]
      } else if (method == 'poisson' & length(lhs_form) == 3 & lhs_form[1] == 'cbind') {
        # Poisson tree can be specified via cbind in the formula lhs so we need to deal with this case separately
        true_oob <- data[oob_obs, lhs_form[3]]
        pred_oob <- pred_oob * data[oob_obs, lhs_form[2]] # take the observation time into account in the prediction
      } else {
        stop('Multiple elements are found in the lhs of the formula, but it is not the cbind construct for Poisson trees. Do not know how to handle this.
             Possible issue: a function is applied to the response in the formula. Please refrain from doing this, but rather add the transformed variable to the data.')
      }
      # Calculate the (weighted) OOB error
      oob_err[rf_id] <- switch(as.character(is.null(substitute(weights))),
                               'TRUE' = error_funcs(ytrue = true_oob, ypred = pred_oob, method = method),
                               'FALSE' = error_funcs(ytrue = true_oob, ypred = pred_oob, wcase = data[oob_obs, as.character(substitute(weights))], method = method))
    }
  }
  
  # Save the trees in a list under the $trees attribute
  rf_obj <- list('trees' = rf_fit)
  # Add the tracked OOB error to the list
  if (track_oob) rf_obj[['oob_error']] <- oob_err
  
  # Make the random forest object of the class 'rforest' and subclass 'list'
  class(rf_obj) <- append('rforest', class(rf_obj))
  
  # Return the random forest
  return(rf_obj)  
}



