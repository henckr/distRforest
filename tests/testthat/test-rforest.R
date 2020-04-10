context('Random forest build')
library(distRforest)

# Use a gbm fit on the mtpl_be data to test the partial dependence function
if (!requireNamespace('CASdatasets', quietly = TRUE)) {
  stop('Package "CASdatasets" needed for this function to work. Please install it.',
       call. = FALSE)
}
library(CASdatasets)
data(ausprivauto0405)


test_that('The weights parameter can be specified as string or variable name', {
  # Set some global settings
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest with variable name as weight
  set.seed(54321)
  rf_gamma <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val)
  # Fit the random forest with string as weight
  set.seed(54321)
  rf_gamma_chr <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = 'ClaimNb',
                                       data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                       ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val)
  # Check that they are exactly the same
  for(i in seq_len(ntrees_val)) expect_true(all.equal(rf_gamma[['trees']][[i]]$frame, rf_gamma_chr[['trees']][[i]]$frame))
})


test_that('The data is saved properly when asked', {
  # Set some global settings
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest with variable name as weight
  set.seed(54321)
  rf_gamma <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, keep_data = TRUE)
  # Check that they are exactly the same
  expect_true(all.equal(ausprivauto0405_claims, rf_gamma[['data']]))
})


test_that('The random forest results in the same Poisson trees with exposure as one would get with rpart', {
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_poiss <- distRforest::rforest(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val)
  # Fit the individual trees
  set.seed(54321)
  rpart_poiss <- vector('list', length = ntrees_val)
  for(i in seq_len(ntrees_val)) {
    rpart_poiss[[i]] <- distRforest::rpart(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                           data = ausprivauto0405[sample(seq_len(nrow(ausprivauto0405)), size = subsample_val * nrow(ausprivauto0405), replace = TRUE), ],
                                           method = 'poisson', control = ctrl, parms = list('shrink' = 10000000), ncand = ncand_val, seed = i)
  }
  # Check that they are exactly the same
  for(i in seq_len(ntrees_val)) expect_true(all.equal(rf_poiss[['trees']][[i]]$frame, rpart_poiss[[i]]$frame))
})


test_that('The random forest results in the same gamma trees with weights as one would get with rpart', {
  # Set some global settings
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_gamma <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val)
  # Fit the individual trees
  set.seed(54321)
  rpart_gamma <- vector('list', length = ntrees_val)
  for(i in seq_len(ntrees_val)) {
    rpart_gamma[[i]] <- distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                           data = ausprivauto0405_claims[sample(seq_len(nrow(ausprivauto0405_claims)), size = subsample_val * nrow(ausprivauto0405_claims), replace = TRUE), ],
                                           method = 'gamma', control = ctrl, ncand = ncand_val, seed = i)
  }
  # Check that they are exactly the same
  for(i in seq_len(ntrees_val)) expect_true(all.equal(rf_gamma[['trees']][[i]]$frame, rpart_gamma[[i]]$frame))
})


test_that('The random forest results in the same lognormal trees with weights as one would get with rpart', {
  # Set some global settings
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_gamma <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'lognormal', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val)
  # Fit the individual trees
  set.seed(54321)
  rpart_gamma <- vector('list', length = ntrees_val)
  for(i in seq_len(ntrees_val)) {
    rpart_gamma[[i]] <- distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                           data = ausprivauto0405_claims[sample(seq_len(nrow(ausprivauto0405_claims)), size = subsample_val * nrow(ausprivauto0405_claims), replace = TRUE), ],
                                           method = 'lognormal', control = ctrl, ncand = ncand_val, seed = i)
  }
  # Check that they are exactly the same
  for(i in seq_len(ntrees_val)) expect_true(all.equal(rf_gamma[['trees']][[i]]$frame, rpart_gamma[[i]]$frame))
})


test_that('The random forest results in the same classification trees as one would get with rpart', {
  # Set some global settings
  ausprivauto0405_balanced <- rbind(ausprivauto0405[ausprivauto0405$ClaimOcc == 1, ],
                                    ausprivauto0405[ausprivauto0405$ClaimOcc == 0, ][1:5000, ])
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_class <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405_balanced, method = 'class', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val)
  # Fit the individual trees
  set.seed(54321)
  rpart_class <- vector('list', length = ntrees_val)
  for(i in seq_len(ntrees_val)) {
    rpart_class[[i]] <- distRforest::rpart(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                           data = ausprivauto0405_balanced[sample(seq_len(nrow(ausprivauto0405_balanced)), size = subsample_val * nrow(ausprivauto0405_balanced), replace = TRUE), ],
                                           method = 'class', control = ctrl, ncand = ncand_val, seed = i)
  }
  # Check that they are exactly the same
  for(i in seq_len(ntrees_val)) expect_true(all.equal(rf_class[['trees']][[i]]$frame, rpart_class[[i]]$frame))
  
  # Change response to factor and check again
  
  # Set some global settings
  ausprivauto0405_balanced$ClaimOcc <- factor(ausprivauto0405_balanced$ClaimOcc)
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_class_fact <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                        data = ausprivauto0405_balanced, method = 'class', control = ctrl,
                                        ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val)
  # Fit the individual trees
  set.seed(54321)
  rpart_class_fact <- vector('list', length = ntrees_val)
  for(i in seq_len(ntrees_val)) {
    rpart_class_fact[[i]] <- distRforest::rpart(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                                data = ausprivauto0405_balanced[sample(seq_len(nrow(ausprivauto0405_balanced)), size = subsample_val * nrow(ausprivauto0405_balanced), replace = TRUE), ],
                                                method = 'class', control = ctrl, ncand = ncand_val, seed = i)
  }
  # Check that they are exactly the same
  for(i in seq_len(ntrees_val)) expect_true(all.equal(rf_class[['trees']][[i]]$frame, rf_class_fact[['trees']][[i]]$frame))
  for(i in seq_len(ntrees_val)) expect_true(all.equal(rf_class[['trees']][[i]]$frame, rpart_class_fact[[i]]$frame))
})
