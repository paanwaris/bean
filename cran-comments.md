## Test environments

* local Windows 11, R 4.5.2 (release)
* win-builder, R-devel
* win-builder, R 4.5 (release)

## R CMD check results

0 errors | 0 warnings | 0 notes

This is a new submission.

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
