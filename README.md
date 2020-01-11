
<!-- README.md is generated from README.Rmd. Please edit that file -->
gen5helper
==========

A Collection of Functions for Processing Gen5 2.06 Exported Data

Installation
------------

You can install gen5helper from CRAN with:

``` r
install.packages("gen5helper")
```

Example
-------

After exporting tab-delim ascii files (named 'data.txt' for instance), this is a basic example for further cleaning and annotating:

``` r
## clean
df <- g5h.clean2('data.txt')
## add time interval.
df <- g5h.annotate(df)
```
