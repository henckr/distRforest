#' Build a random forest
#'
#' This function acts as a user-friendly interface to build a random forest
#' based on individual \code{\link{rpart}} trees.
#'
#' @param formula object of the class \code{formula} with a symbolic description
#'   of the form \code{response ~ var1 + var2 + var3} without interactions.
#'   Please refrain from applying transformation functions to the response, but
#'   add the transformed variable to the \code{data} beforehand. Two exceptions
#'   exist, see \code{method = 'poisson'} and \code{method = 'exp'} below.
#' @param data data frame containing the training data observations.
#' @param method string specifying the type of forest to build. Options are:
#'   \describe{ \item{'class'}{classification forest (OOB error tracking only
#'   implemented for binary classification).} \item{'anova'}{standard regression
#'   forest with a squared error loss.} \item{'poisson'}{poisson regression
#'   forest for count data. The left-hand-side of \code{formula} can be
#'   specified as \code{cbind(observation_time, number_of_events)} to include
#'   time exposures.} \item{'gamma'}{gamma regression forest for strictly
#'   positive long-tailed data.} \item{'lognormal'}{lognormal regression forest
#'   for strictly positive long-tailed data.} \item{'exp'}{exponential scaling
#'   for survival data. The left-hand-side of \code{formula} is specified as
#'   \code{Surv(observation_time, event_indicator)} to include time exposures.}}
#' @param weights optional name of the variable in \code{data} to use as case
#'   weights. Either as a string or simply the variable name should work.
#' @param parms optional parameters for the splitting function, see
#'   \code{\link{rpart}} for the details and allowed options.
#' @param control list of options that control the fitting details of the
#'   \code{rpart} trees. Use \code{\link{rpart.control}} to set this up.
#' @param ncand integer specifying the number of randomly chosen variable
#'   candidates to consider at each node to find the optimal split.
#' @param ntrees integer specifying the number of trees in the ensemble.
#' @param subsample numeric in the range [0,1]. Each tree in the ensemble is
#'   built on randomly sampled data of size \code{subsample * nrow(data)}.
#' @param track_oob boolean to indicate whether the out-of-bag errors should be
#'   tracked (TRUE) or not (FALSE). This option is not implemented for
#'   \code{method = 'exp'} or multi-class classification. For the other methods,
#'   these errors are tracked:  \describe{ \item{'class'}{Matthews correlation
#'   coefficient for binary classification.} \item{'anova'}{mean squared error.}
#'   \item{'poisson'}{Poisson deviance.} \item{'gamma'}{gamma deviance.}
#'   \item{'lognormal'}{mean squared error.}} All these errors are evaluated in
#'   a weighted version if \code{weights} are supplied.
#' @param keep_data boolean to indicate whether the \code{data} should be saved
#'   with the fit. Not advised to set this to \code{TRUE} for large data sets.
#' @param red_mem boolean whether to reduce the memory footprint of the
#'   \code{rpart} trees by eliminating non-essential elements from the fits. It
#'   is adviced to set this to \code{TRUE} for large values of \code{ntrees}.
#' @return object of the class \code{rforest}, which is a list containing the
#'   following elements: \describe{ \item{trees}{list of length equal to
#'   \code{ntrees}, containing the individual \code{rpart} trees.}
#'   \item{oob_error}{numeric vector of length equal to \code{ntrees},
#'   containing the OOB error at each iteration (if \code{track_oob = TRUE}).}
#'   \item{data}{the training \code{data} (if \code{keep_data = TRUE}).}}
rforest <- function(formula, data, method, weights = NULL, parms = NULL, control = NULL, ncand, ntrees, subsample = 1, track_oob = FALSE, keep_data = FALSE, red_mem = FALSE){
  
  if (is.character(substitute(weights))) weights <- as.name(weights)
  
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
                     'redmem' = red_mem,
                     'seed' = substitute(rf_id))
  
  # Create a list to save the individual trees in the random forest
  rf_fit <- vector('list', length = ntrees)
  
  # Initialize components to track the OOB error
  if (track_oob) {
    oob_err <- rep(NA, ntrees) # OOB error
    pred_vec <- rep(0, nrow(data)) # OOB predictions
    freq_oob <- rep(0, nrow(data)) # OOB count
    # Use the formula to extract the true values of the response variable
    lhs_form <- as.character(formula[[2]])
    if (length(lhs_form) == 1) {
      true_resp <- data[, lhs_form]
    } else if (method == 'poisson' & length(lhs_form) == 3 & lhs_form[1] == 'cbind') {
      # Poisson tree can be specified via cbind in the formula lhs so we need to deal with this case separately
      true_resp <- data[, lhs_form[3]]
    } else {
      stop('Multiple elements are found in the lhs of the formula, but it is not the cbind construct for Poisson trees. Do not know how to handle this.
           Possible issue: a function is applied to the response in the formula. Please refrain from doing this, but rather add the transformed variable to the data.')
    }
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
      
      # Update the frequency count and predictions for the OOB observations
      oob_obs <- setdiff(seq_len(nrow(data)), indx_smpl)
      freq_oob[oob_obs] <- freq_oob[oob_obs] + 1
      pred_vec[oob_obs] <- pred_vec[oob_obs] + predict(rf_fit[[rf_id]], newdata = data[oob_obs, ], type = 'vector')
      
      # Calculate current OOB predictions
      oob_pred <- pred_vec / freq_oob
      
      if (method == 'poisson' & length(lhs_form) == 3 & lhs_form[1] == 'cbind') {
        # Take the observation time into account in the prediction
        oob_pred <- oob_pred * data[, lhs_form[2]] 
      } 
      
      # Calculate the (weighted) OOB error
      oob_err[rf_id] <- switch(as.character(is.null(substitute(weights))),
                               'TRUE' = error_funcs(ytrue = true_resp, ypred = oob_pred, method = method),
                               'FALSE' = error_funcs(ytrue = true_resp, ypred = oob_pred, wcase = data[, as.character(substitute(weights))], method = method))
    }
  }
  
  # Save the trees in a list under the $trees attribute
  rf_obj <- list('trees' = rf_fit)
  # Add the tracked OOB error / training data to the list
  if (track_oob) rf_obj[['oob_error']] <- oob_err
  if (keep_data) rf_obj[['data']] <- data
  
  # Make the random forest object of the class 'rforest' and subclass 'list'
  class(rf_obj) <- append('rforest', class(rf_obj))
  
  # Return the random forest
  return(rf_obj)  
}

# library(CASdatasets)
# 
# control <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
# ncand <- 3 ; ntrees <- 5 ; subsample <- 0.5
# # Fit the random forest
# set.seed(54321)
# formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge
# data(ausprivauto0405)
# data = ausprivauto0405
# method = 'poisson'
# weights = NULL
# parms = list('shrink' = 10000000)
# track_oob = TRUE
# keep_data = FALSE
# red_mem = TRUE
# 
