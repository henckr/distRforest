# distRforest
R implementation for random forests with distribution-based loss functions

This is an extension of the **rpart** package, which is on [CRAN](https://cran.r-project.org/web/packages/rpart/index.html), and has the original source code on [GitHub](https://github.com/cran/rpart). By no means is this meant to be a completely new R package or a replacement for **rpart**. This is merely an extension of the **rpart** package for specific use-cases. The original rpart functionality stays valid and copyright belongs to the authors Terry Therneau, Beth Atkinson and Brian Ripley.

I added two specific features to this package:
1. Develop regression trees with the Lognormal and Gamma deviance as loss functions (besides MSE and Poisson deviance which were already implemented in the original package).
2. Build random forests from these regression trees.



## How to install?

#### Option 1 (direct download from GitHub)
```
install.packages(devtools)
devtools::install_github('henckr/distRforest')
library(distRforest)
```

#### Option 2 (download source files and build the package yourself)
Download the code such that you get the folder 'distRforest-master' containing all the source files.
Change the working directory to the location of this folder. You can build the package and install it as follows:
```
system('R CMD build distRforest-master')
install.packages(list.files(pattern='^distRforest.*tar.gz$'), repos = NULL)
library(distRforest)
```
The first line works on Mac OS, for Windows you will need Rtools.


## How to use?
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
rforest <- function(formula, data, weights = NULL, method, parms = NULL, control = NULL, ncand, ntrees, subsample = 1, redmem = FALSE)
```
The _rforest_ function contains four new parameters compared to the _rpart_ fucntion:
- ncand: the number of variables to consider at each split (defaults to all availabe variables if not set).
- ntrees: the number of trees to build in the forest.
- subsample: a fraction specifying which proportion of the original data size we want to use to build each tree. This should be a number in the interval (0,1].
- redmem: a boolean indicating whether you want to reduce the memory of the underlying trees. This is really a necessary step when building large random forests, but important elements to inspect the trees and to predict from the trees are of course never removed.

The output of the _rforest_ function is a list containing all the individual trees. The classes of this object are 'rf' and 'list'.


## Harmony between rpart and distRforest
Note that the basic function to create a regression tree is still called _rpart_ in the **distRforest** package. If you have both the **rpart** and **distRforest** package loaded in your R session, it is best to explicitly call the function via distRforest::rpart() to make sure you can access the extra functionality.
