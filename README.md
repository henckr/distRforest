# distRforest
R implementation for random forests with distribution-based loss functions

This is an extension of the **rpart** package, which is on [CRAN](https://cran.r-project.org/web/packages/rpart/index.html), and has the original source code on [GitHub](https://github.com/cran/rpart). By no means is this meant to be a new R package. This is merely an extension of the **rpart** package for specific use-cases. The original rpart functionality stays valid and copyright belongs to the authors Terry Therneau, Beth Atkinson and Brian Ripley.

I added two specific features to this package:
1. Develop regression trees with the Lognormal and Gamma deviance as loss functions (besides MSE and Poisson deviance which were already implemented in the original package).
2. Build random forests from these regression trees.



## How to install
Remove the package **rpart** from your library if you have installed it before:
```
remove.packages('rpart')
```
Run this command twice. You can be sure that it has been removed if it results in the following error message the second time:
> Error in remove.packages : there is no package called ‘rpart’

Now **_restart your R session_** and use one of the following options to install the adjusted package.

#### Option 1 (direct download from GitHub)
```
install.packages(devtools)
devtools::install_github('RoelHenckaerts/distRforest')
library(rpart)
```

#### Option 2 (download source files and build the package yourself)
Download the code such that you get the folder 'distRforest-master' containing all the source files.
Change the working directory to the location of this folder. You can build the package and install it as follows:
```
system('R CMD build distRforest-master')
install.packages(list.files(pattern='^rpart.*tar.gz$'), repos = NULL)
library(rpart)
```
The first line works on Mac OS, for Windows you will need Rtools.



## How to use
There is no help documentation available on the added features, so I explain all the necessary information here.

#### Gamma and Lognormal regression trees
To build a regression tree with the Gamma or Lognormal deviance as loss function, invoke the _rpart_ function as follows:
```
rpart(formula, data, method = 'gamma') # for Gamma trees
rpart(formula, data, method = 'lognormal') # for Lognormal trees
```

#### Random forest
The random forest function looks like this:
```
rforest <- function(formula, data, weights = NULL, method, parms = NULL, control = NULL, ncand, ntrees, subsample = 1)
```
The _rforest_ function contains three new parameters compared to the _rpart_ fucntion:
- ncand: the number of variables to consider at each split (defaults to all availabe variables if not set).
- ntrees: the number of trees to build in the forest.
- subsample: a fraction specifying which proportion of the original data size we want to use to build each tree. This should be a number in the interval (0,1].

The output of the _rforest_ function is a list containing all the individual trees. If you only care about predictions and variable importance, you can reduce the memory footprint drastically in the following way:
```
forest_fit <- rforest(...)
for(i in 1:length(forest_fit)) for(x in c('y','where','call','cptable','parms','control','functions','numresp','ordered')) forest_fit[[i]][[x]] <- NULL
```



