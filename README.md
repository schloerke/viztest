# viztest

`viztest` executes all examples of a local R package against itself and a CRAN or GitHub version of a package using knitr.  `viztest` then pixel compares any images or webshots produced while running the local examples.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## While the current directory is the base of an R package...

# Regular viztest against the current cran version
viztest::viztest(".")
# Save the output to viztest-cran
viztest::viztest(".", output_dir = "viztest-cran")
# Save the output to viztest-cran2 and do not resize the diff images
viztest::viztest(".", "viztest", "viztest-cran2", resize = FALSE)

# Compare against master branch of schloerke/viztest on GitHub
viztest::viztest(".", "schloerke/viztest", resize = "50%")
```
