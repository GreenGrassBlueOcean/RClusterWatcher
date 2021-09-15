# RClusterWatcher
[![R-CMD-check](https://github.com/GreenGrassBlueOcean/RClusterWatcher/workflows/R-CMD-check/badge.svg)](https://github.com/GreenGrassBlueOcean/RClusterWatcher/actions)
[![Codecov test coverage](https://codecov.io/gh/GreenGrassBlueOcean/RClusterWatcher/branch/master/graph/badge.svg)](https://codecov.io/gh/GreenGrassBlueOcean/RClusterWatcher?branch=master)

kills zombie workers from parallell clusters when pressing CTRL + C  or when the host suddenly dies on windows

# Installation of the package

```r
install.packages("devtools")
devtools::install_github("GreenGrassBlueOcean/RClusterWatcher")
```
load the package
```r
library(RClusterWatcher)
```
