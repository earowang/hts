# hts 6.0.2

* Removed dependency on matrixcalc

# hts 6.0.1

* Fixed bug in `forecast(nonnegative = TRUE)` for erroring "object not found". (#58, @apantovic)

# hts 6.0.0

* Added the support for non-negative forecast reconciliation. (@ShanikaLW)
* Officially retired in favour of `fable`.
* Depended on `forecast (>= v8.12)` due to the change in `accuracy()` signature.
* Fixed bug in `forecast(weights = "wls")` for removing the squared root, as it's been done in following functions.

# hts 5.1.5

* Fixed hts authorship in the DESCRIPTION file
* Updated reference
* Replaced `rBind` with `rbind` due to Matrix new release
* Depends on R (>= 3.2.0)

# hts 5.1.4

* The `hts` and `gts` don't actually fit into `mts`, `ts` and `matrix` classes, and hence `mts`, `ts` and `matrix` classes are dropped.
* Added helper functions `get_nodes` for `hts` and `get_groups` for `gts`.
* Fixed `forecast(method = "tdfp", h = 1)` issue ([#32](https://github.com/earowang/hts/issues/32)).
* Fixed `forecast(FUN = hybridModel)` when `newxreg` is present ([#28](https://github.com/earowang/hts/issues/28)).

# hts 5.1.0

* Earo Wang took over maintenance of the package from Rob J Hyndman.
* Replaced `ChangeLog` with a `NEWS.md` file to track changes to the package.
* Used `roxygen2` to generate and manage reference. Thanks to Yihui's `Rd2roxygen` package for hassle-free conversion.
* Enabled `pkgdown` support.
* Allowed more control of colour in `plot.gts` (Thanks to @ellisp for the pull request #25).
* Exported the `is.hts` and `is.gts` functions (as per [#29](https://github.com/earowang/hts/issues/29)).
* Registered `accurary` as an S3 method from `forecast::accuracy`.

# hts 5.0

* Added mint option in forecast.gts (written by Shanika Wickramasuriya)
* Allowed arbitrary ordering of bottom level time series in hts()
* Added QR decomposition if LU decomposition fails
* Bug fixes

# hts 4.5

* Fixed bugs in accuracy.gts().
* Fixed bug in forecast.gts() detecting the right default forecasting horizon.
* Fixed bug in gts() for a gts with one grouping variable.
* Speeded up Smatrix().
* Make SparseM package as dependency.
* Better handle missing values.
* Speeded up combinef() for Alan's algorithm.
* Added 3 new algorithms to the optimally reconciled approach.
* Fixed bug in forecast.gts() when using external regressors with parallel.
* Fixed time attributes of fitted and residual series.

# hts 4.4

* Allowed multiple seasonal objects "msts" in hts() and gts().
* Fixed bug of MASE in accuracy.gts().
* Allowed user's defined forecasting function in forecast.gts().

# hts 4.3

* Fixed bug of the arg "include" in plot.gts, when there are not yearly data.
* Fixed bug in combinef(), when it handles a simple hierarchy with 2 levels.
* Improved arg "characters" in hts() to automate the node structure.
* Added a new arg "characters" in gts(), which will automate the grouping matrix.
* Added a new reference to combinef() man.
* Made 'sd' the default weight in forecast.gts().

# hts 4.2

* Fixed Next.Generic error in accuracy.gts.
* Adjust weights = sd.

# hts 4.1

* Set the default parallel processes to 2.
* Fixed three dots in parallel computing for forecast.gts.
* Speeded up the topdown approaches.

# hts 4.0

* Speeded up all existing functions.
* Restructured hts function. Argument g is replaced with nodes. Added bnames and
	characters to allow custom names.
* Added argument gnames to gts function.
* Argument groups in gts function allows the gmatrix with characters.
* Added function aggts in which users can specify whatever levels they like.
* Renamed Smatrix to smatrix.
* Added summary.gts function.
* Added more arguments to forecast.gts such as keep.fitted, keep.resid, lambda,
	weights.
* When setting method = "mo" in forecasts.gts, argument level started with 0
	rather than 1.
* Import parallel package in order to support for parallel processing.
* Added more criterions to accuracy.gts function. When keep.fitted = TRUE in
	forecast.gts function, accuracy.gts can also return in-sample error measures
	at the bottom level.
* Dropped argument criterions in accuracy.gts function.
* Added argument weights to combinef function. When it's a hts object, argument
	nodes is used instead of the summing matrix.
* combinef function returns either a gts object or time series at the bottom
  level. Dropped arguments return and hierarchical.
* Argument levels in plot.gts function allows more flexibilities.

# hts 3.01

* Added the infantgts data
* Added the vignette

# hts 3.00

* Restructured gts objects and dropped hts objects. A flag indicates if gts is
	hierarchical.
* gts objects can now contain forecasts as well as historical data.
* Updated plot.gts function with an option to show historical data as well as
	forecasts
* Added the window.gts function
* Moved SparseM from Depends to Imports

# hts 2.02

* Bug fixes to cope with much bigger hierarchies

# hts 2.01

* Changed hierarchical naming convention to allow much bigger hierarchies.

# hts 2.0

* Added grouped time series (gts) objects, and re-wrote many functions in order
	to handle them.
* hts objects are now a subset of gts objects.
* Some old hts methods have been replaced by gts methods.
* Rob J Hyndman took over maintenance of the package from Han Lin Shang
