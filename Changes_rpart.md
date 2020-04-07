This document list all the changes that are made to the source code of `rpart` to tranfsorm the package to `distRforest`.

Name change from `rpart` to `distRforest`
-----------------------------------------

-   DESCRIPTION
    -   update package name, title, description, url,...
-   NAMESPACE
    -   line 1: change *rpart* to *distRforest* in `useDynLib(distRforest, .registration = TRUE, .fixes = "C_")`
-   src/init.c
    -   line 23: change *rpart* to *distRforest* in `R_init_distRforest(DllInfo * dll)`
-   R/rpart.R
    -   line 78: change *rpart* to *distRforest* in `ns <- asNamespace("distRforest")`
-   R/zzz.R
    -   line 1: change *rpart* to *distRforest* in `library.dynam.unload("distRforest", libpath)`

Random forest functionality
---------------------------

-   src/rpartproto.h
    -   line 39: add `SEXP ncand2` and `SEXP seed2` as input arguments to `SEXP rpart(...)`
    -   line 64: add `void sample(int *vector, int x, int size);`
-   src/rpart.h
    -   line 43: add `int ncand;` and `int seed;` to `EXTERN struct {...} rp;`
-   src/rpart.c
    -   line 41: add `SEXP ncand2` and `SEXP seed2` as input arguments to `rpart(...)`
    -   line 116-117: add both to the `rp` struct by `rp.ncand = asInteger(ncand2);` and `rp.seed = asInteger(seed2);`
    -   line 119: set the seed via `srand(rp.seed);`
-   R/rpart.R
    -   line 4: add `ncand`, `seed` and `redmem = FALSE` as input arguments to `rpart <- function(...)`
    -   line 9: add `if (missing(parms)) parms <- NULL`
    -   line 50 and 71: change condition `missing(parms)` to `is.null(parms)`
    -   line 153-154: set `ncand` equal to number of available variables when input argument is missing
    -   line 157: set `seed` equalt to 1 when input argument is missing
    -   line 163: add `as.integer(ncand)` and `as.integer(seed)` as input arguments to `rpfit <- .Call(C_rpart,...)`
    -   line 301: add `if (redmem) for(x in c(...)) ans[[x]] <- NULL` to reduce memory of fitted `rpart` object
-   src/init.c
    -   line 14: change the number of input parameters from 11 to 13 in `{"rpart", (DL_FUNC) &rpart, 13}`
-   src/sample.c
    -   create this file to be able to sample in the split candidates
-   src/bsplit.c
    -   line 39: add `int candidates[rp.ncand];` to allocate a vector to store the split candidates in
    -   line 40: add `sample(candidates, rp.nvar, rp.ncand);` to sample the split candidates
    -   line 50: change `rp.nvar` to `rp.ncand` in `for (i = 0; i < rp.ncand; i++)` to iterate over subset
    -   line 51-94: change `i` to `candidates[i]` in `for` body to select correct split candidate from the sample

User-friendly random forest function
------------------------------------

-   R/rforest.R
    -   add this file to make it easier for the user to build a random forest
-   NAMESPACE
    -   line 3: add `rforest` to `export(...)`

Log-normal and gamma distributions
----------------------------------

-   src/anova.c
    -   line 191-196: merge `anovapred.c` file in `anova.c` as the `anovapred(...)` function
-   src/lognormal.c
    -   add this file containing the functions necessary to support lognormal splitting
-   src/gamma.c
    -   add this file containing the functions necessary to support gamma splitting
-   src/func\_table.h
    -   line 21-24: add `extern int lognormalinit(...)` and `extern int gammainit(...)`
    -   line 35-38: add `extern int lognormaldev(...)` and `extern int gammadev(...)`
    -   line 52-57: add `extern int lognormal(...)` and `extern int gammasplit(...)`
    -   line 63-64: add `extern int lognormalpred(...)` and `extern int gammapred(...)`
-   R/rpart.lognormal.R
    -   add this file to initialize the lognormal tree and check whether the response input is strictly positive
-   R/rpart.gamma.R
    -   add this file to initialize the gamma tree and check whether the response input is strictly positive
-   R/rpart.R
    -   line 59 and 61: add `"lognormal"` and `"gamma"` to vector of possible methods
    -   line 109: add warning message that CV is not supported when `method %in% c('lognormal','gamma') && xval > 0L`

Miscellaneous
-------------

-   vignettes/
    -   remove original `rpart` vignettes in the `distRforest` package
-   tests/
    -   remove original `rpart` tests in the `distRforest` package
    -   test-distributions.R: test the `method = 'gamma'` and `method = 'lognormal'` options added to `rpart(...)`
