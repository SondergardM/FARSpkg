
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FARSpkg

## Create an Example Package in R

Package: FARSpkg

GitHub: <https://github.com/SondergardM/FARSpkg>

## Data Source

The functions in this R package utilize data from the US National
Highway Traffic Safety Administration’s [Fatality Analysis Reporting
System](https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars),
which is a nationwide census providing the American public yearly data
regarding fatal injuries suffered in motor vehicle traffic crashes.

## Package Summary

The functions in this package are designed to utilize data from the US
National Highway Traffic Safety Administration’s Fatality Analysis
Reporting System (FARS), which is a nationwide census that provides the
American public with yearly data concerning fatal injuries suffered in
motor vehicle traffic crashes.

Data is provided only for the years 2013, 2014 and 2015.

The package consists of five (5) functions:

-   **make\_filename** This function takes an input year and returns a
    file name matching the FARS data files in the package.

-   **fars\_read** This function loads the FARS data for the specific
    years of interest.

-   **fars\_read\_years** This function reads in the FARS data for a
    range of years (2003 - 2015)

-   **fars\_summarize\_years** This function produces a summary of the
    number of fatal traffic accidents per month for the specific year of
    interest.

-   **fars\_map\_state** This function generates a map showing the
    location of fatal traffic accidents for a given state and year, as
    requested by the user.

Further documentation can be found in the package documentation for each
individual function.

## Useful Reference Links:

-   [Common `roxygen2`
    tags](https://bookdown.org/rdpeng/RProgDA/documentation.html#common-roxygen2-tags)
-   [R Packages (book)](https://r-pkgs.org/)
-   [Writing R
    Extensions](https://cran.r-project.org/doc/manuals/R-exts.html#Creating-R-packages)
-   [Travis: Building R
    packages](https://docs.travis-ci.com/user/languages/r/)
-   [Package Development with devtools Cheat
    Sheet](https://github.com/rstudio/cheatsheets/raw/master/package-development.pdf)
-   [Testing packages](http://r-pkgs.had.co.nz/tests.html)

## Travis badge

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/SondergardM/FARSpkg.svg?branch=main)](https://travis-ci.com/SondergardM/FARSpkg)
<!-- badges: end -->
