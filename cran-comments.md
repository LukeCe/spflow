## Test environments
  - local R installation: ubuntu 20.04, R 4.1.1
  - with github actions:
    - ubuntu 20.04 (release), R 4.1.1
    - ubuntu 20.04 (devel), R 4.1.1
    - windows (release), R 4.1.1
    - macOS (release), R 4.1.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

This is a resubmission

* Added a return value to all exported functions (shown by ls("package:spflow"))
    Some of the .Rd files contains documentation for non-exported function, which is written for the developer team (and less a bit less rigorous)
* Removed \dontrun{} environments from examples
* The vignette is adapted to restore the user options and graphical parameters
  
  
