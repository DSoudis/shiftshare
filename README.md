# shiftshare

The package contains two functions, namely `ssa` and `dssa`, that perform simple and dynamic shift share analysis.
This is a first working version so errors might occur. Comments are welcome!

# Installation

`devtools::install_github("DSoudis/shiftshare")`

# Usage
```
library(shiftshare)
data('usregions')
my.ssa <- ssa(usregions, y.var = 'TOT_EMP' ,region.var = 'ST', sector.var = 'OCC_TITLE',
              gregion.id = 'US', year.var = 'Year', sector.id = 'All Occupations',
              start.year = 2013, end.year = 2015)
```
