This document list all the changes that are made to the source code of `rpart` to tranfsorm the package to `distRforest`.

Name change from `rpart` to `distRforest`
-----------------------------------------

-   DESCRIPTION:
    -   update package naame, title, description, url,...
-   NAMESPACE:
    -   line 1: change *rpart* to *distRforest* in `useDynLib(distRforest, .registration = TRUE, .fixes = "C_")`
-   src/init.c:
    -   line 23: change *rpart* to *distRforest* in `R_init_distRforest(DllInfo * dll)`
-   R/rpart.R:
    -   line 78: change *rpart* to *distRforest* in `ns <- asNamespace("distRforest")`
