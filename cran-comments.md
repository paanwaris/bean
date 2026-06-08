## Submission

This is a new submission of `bean` (version 0.2.1).

## Test environments

* local Windows 11, R 4.5.2 (release)
* win-builder (R-devel)
* win-builder (R-release)
* GitHub Actions: windows-latest, macOS-latest, ubuntu-latest, R devel and
  R release

I have checked the submission using `R CMD check --as-cran` and a current
version of R-devel, as mandated by the CRAN Repository Policy, using the
win-builder service at <https://win-builder.r-project.org>.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

* `rgl` is a Suggests dependency, used only for optional 3-D plots in
  `plot.bean_ellipsoid()`. When `rgl` is not installed, the method falls
  back to a 2-D plot and emits a `message()` rather than failing.
* Objects returned by `fit_ellipsoid()` carry a second S3 class
  (`"nicheR_ellipsoid"`) so that the `predict()` method provided by the
  companion package nicheR can dispatch on them directly. `bean` itself
  does not import nicheR; the second class string is only a label.
* All examples that require network access or large external data are
  wrapped in `\donttest{}`; nothing writes outside `tempdir()`.

## Downstream dependencies

There are currently no downstream dependencies on CRAN.
