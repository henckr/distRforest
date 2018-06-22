# distRforest
R implementation for random forests with distribution-based loss functions

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

#### Option 2 (download source files and build the package)
Download the code such that you get the folder 'distRforest-master' containing all the source files.
Change the working directory to the location of this folder. You can build the package and install it as follows:
```
system('R CMD build distRforest-master')
install.packages(list.files(pattern='^rpart.*tar.gz$'), repos = NULL)
library(rpart)
```
The first line works on Mac OS, for Windows you will need Rtools.


