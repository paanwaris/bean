## Resubmission

This is a resubmission. Version 0.2.1 makes two related changes:

* `predict.bean_ellipsoid()` has been removed. The canonical implementation
  now lives in the companion package nicheR.
* `bean_ellipsoid` objects returned by `fit_ellipsoid()` carry a second S3
  class, `"nicheR_ellipsoid"`, and include the fields nicheR's predict
  method requires. As a result, once the user attaches nicheR its
  `predict()` method dispatches directly on bean ellipsoids without any
  conversion step.

`nicheR` is not declared in `Imports`, `Suggests`, or `Enhances`: bean does
not depend on nicheR, and the second class string is simply a label that
allows downstream packages (including nicheR) to dispatch on bean objects.
No `R CMD check` reverse-dependency consequences are expected.

## Test environments

* local Windows 11, R 4.5.2 (release)
* win-builder, R-devel
* win-builder, R 4.5 (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

I have checked the submission using `R CMD check --as-cran` and a current
version of R-devel via the win-builder service at
<https://win-builder.r-project.org>, as mandated by the CRAN Repository
Policy.

## Notes for the reviewer

* `rgl` is a Suggests dependency, used only for optional 3-D plots in
  `plot.bean_ellipsoid()`. When `rgl` is not installed, the method falls
  back to a 2-D plot and emits a `message()`.

## Downstream dependencies

There are currently no downstream dependencies.
