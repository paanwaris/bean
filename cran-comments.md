## Resubmission (bean 0.2.1)

This is a resubmission of `bean`. In this version (0.2.1) I have:

* Removed `predict.bean_ellipsoid()`. The canonical implementation lives
  in the companion package nicheR; objects returned by `fit_ellipsoid()`
  now carry a second S3 class (`"nicheR_ellipsoid"`) so that nicheR's
  `predict()` method dispatches on them directly. `bean` itself does not
  import, suggest, or enhance nicheR — the second class string is only
  a label.
* Confirmed that the test suite passes locally and on win-builder
  R-devel and R-release.

### Test environments

* local Windows 11, R 4.5.2 (release)
* win-builder (R-devel)
* win-builder (R-release)
* GitHub Actions: windows-latest, macOS-latest, ubuntu-latest, R devel
  and R release

I have checked the submission using `R CMD check --as-cran` and a current
version of R-devel, as mandated by the CRAN Repository Policy, using the
win-builder service at <https://win-builder.r-project.org>.

### R CMD check results

0 errors | 0 warnings | 0 notes

### Notes for the reviewer

* `rgl` is a Suggests dependency, used only for optional 3-D plots in
  `plot.bean_ellipsoid()`. When `rgl` is not installed, the method falls
  back to a 2-D plot and emits a `message()` rather than failing.
* Objects returned by `fit_ellipsoid()` carry the S3 class
  `"nicheR_ellipsoid"` as a second class string so the `predict()` method
  provided by the companion package nicheR can dispatch on them. `bean`
  does not import, suggest, or enhance nicheR.
* All examples that require network access or large external data are
  wrapped in `\donttest{}`; nothing writes outside `tempdir()`.

### Downstream dependencies

There are currently no downstream dependencies on CRAN.

---

## First submission (bean 0.2.0)

`bean` is a new package being submitted to CRAN for the first time.

### Test environments

* local Windows 11, R 4.5.2 (release)
* win-builder (R-devel)
* win-builder (R-release)
* GitHub Actions: windows-latest, macOS-latest, ubuntu-latest, R devel
  and R release

### R CMD check results

0 errors | 0 warnings | 0 notes

This is a new submission.

### Notes for the reviewer

* `rgl` is a Suggests dependency, used only for optional 3-D plots in
  `plot.bean_ellipsoid()`. When `rgl` is not installed, the method falls
  back to a 2-D plot and emits a `message()`.
* All examples that require network access or large external data are
  wrapped in `\donttest{}`; nothing writes outside `tempdir()`.

### Downstream dependencies

There are currently no downstream dependencies.
