# simplepx

[![R build status](https://github.com/paasim/simplepx/workflows/R-CMD-check/badge.svg)](https://github.com/paasim/simplepx/actions)
[![codecov](https://codecov.io/gh/paasim/simplepx/branch/master/graphs/badge.svg)](https://codecov.io/gh/paasim/simplepx)

An R package for downloading data from the [PX-Web API of Statistics Finland](http://pxnet2.stat.fi/api1.html) in a tidy format for personal use. The package might work with other PX-Web APIs as well, but so far has only been tested with the API of Statistics Finland. Inspired by the [pxweb](https://github.com/rOpenGov/pxweb)-package, but with a lot less functionality.

Installation
------------

    devtools::install_github("paasim/simplepx")


Usage
-----

    library(simplepx)
    
    # use the english api for this; the default is in finnish
    api <- "https://pxnet2.stat.fi/PXWeb/api/v1/en/"
    # Navigate through the data set listing
    px_nav(api = api)
    # check what data is available under StatFin
    px_nav("StatFin/", api = api)
    # navigate through the directories,
    # check what variables are available for population statistics
    var <- px_var("StatFin/vrm/synt/statfin_synt_pxt_011.px", api = api)
    # select years after 1900
    var_1900 <- dplyr::filter(var, Year >= 1900)
    # Download the data, starting from year 1900,
    # omitting the var-argument would download the data for all the years.
    data <- px_dl("StatFin/vrm/synt/statfin_synt_pxt_011.px", var_1900,
                  simplify_colnames = TRUE, api = api)


