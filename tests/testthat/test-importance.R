context('Random forest importance')
library(distRforest)

# Use a gbm fit on the mtpl_be data to test the partial dependence function
if (!requireNamespace('CASdatasets', quietly = TRUE)) {
  stop('Package "CASdatasets" needed for this function to work. Please install it.',
       call. = FALSE)
}
library(CASdatasets)
data(ausprivauto0405)


test_that('importance scores are being calculated correctly when all variables have a contibution', {
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_poiss <- distRforest::rforest(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE)
  # Get the importance scores
  var_imp <- importance_rforest(rf_poiss) 
  # Check whether the importance scores make sense
  expect_equal(nrow(var_imp), 5)
  expect_equal(ncol(var_imp), 4)
  expect_true(all(names(var_imp) %in% c('variable', 'importance', 'scale_sum', 'scale_max')))
  expect_true(all(c('VehValue', 'VehAge', 'VehBody', 'Gender', 'DrivAge') %in% var_imp$variable))
  expect_true(all(diff(var_imp$importance) <= 0))
  expect_equal(sum(var_imp$scale_sum), 1)
  expect_equal(max(var_imp$scale_max), 1)
})


test_that('importance scores are being calculated correctly when some variables have a contibution', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 1, maxsurrogate = 0)
  ncand_val <- 3 ; ntrees_val <- 3 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_gamma <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE)
  # Get the importance scores
  var_imp <- importance_rforest(rf_gamma) 
  # Check whether the importance scores make sense
  expect_equal(nrow(var_imp), 5)
  expect_equal(ncol(var_imp), 4)
  expect_true(all(names(var_imp) %in% c('variable', 'importance', 'scale_sum', 'scale_max')))
  expect_true(all(c('VehValue', 'VehAge', 'VehBody', 'Gender', 'DrivAge') %in% var_imp$variable))
  expect_true(all(diff(var_imp$importance) <= 0))
  expect_equal(sum(var_imp$scale_sum), 1)
  expect_equal(max(var_imp$scale_max), 1)
})


test_that('error is generated when all the trees are root-node-trees', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 1)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_empty <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE)
  # Get the importance scores
  expect_error(importance_rforest(rf_empty),
               'Can not compute importance scores for a forest of only root-node-trees.')
})