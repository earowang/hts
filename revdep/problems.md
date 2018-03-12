# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.4.0 (2017-04-21) |
|system   |x86_64, darwin16.5.0         |
|ui       |RStudio (1.1.247)            |
|language |(EN)                         |
|collate  |en_AU.UTF-8                  |
|tz       |Australia/Brisbane           |
|date     |2017-06-18                   |

## Packages

|package   |*  |version    |date       |source                             |
|:---------|:--|:----------|:----------|:----------------------------------|
|hts       |*  |5.1.0      |2017-06-18 |local (earowang/hts@NA)            |
|rmarkdown |   |1.6.0.9000 |2017-06-18 |Github (rstudio/rmarkdown@7669d66) |

# Check results

1 packages with problems

|package |version | errors| warnings| notes|
|:-------|:-------|------:|--------:|-----:|
|corset  |0.1-3   |      1|        0|     0|

## corset (0.1-3)
Maintainer: Fran Urbano <viraltux@gmail.com>

1 error  | 0 warnings | 0 notes

```
checking tests ... ERROR
  Running ‘testthat.R’
Running the tests in ‘tests/testthat.R’ failed.
Last 13 lines of output:
  1. Error: Corset - class gts/hts  (@test-bezier.R#142) -------------------------
  invalid 'length.out' value
  1: corset(x, "bezier") at testthat/test-bezier.R:142
  
  2. Error: Corset - class gts/hts  (@test-exp.R#143) ----------------------------
  invalid 'length.out' value
  1: corset(x, "exp") at testthat/test-exp.R:143
  
  testthat results ================================================================
  OK: 21 SKIPPED: 4 FAILED: 2
  1. Error: Corset - class gts/hts  (@test-bezier.R#142) 
  2. Error: Corset - class gts/hts  (@test-exp.R#143) 
  
  Error: testthat unit tests failed
  Execution halted
```

