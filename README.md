
# comRex <img src="man/figures/logo.png" align="right" height="139" />

# comRex

<!-- badges: start -->

<!-- badges: end -->

The goal of comRex is to provide a very simple wrapper for the new [SDMX
API of
Eurostat](https://wikis.ec.europa.eu/display/EUROSTATHELP/API+-+Detailed+guidelines+-+DS-+prefixed+datasets+from+Comext+database),
specifically the Comext database, which is not yet covered in the
wonderful [eurostat](https://github.com/rOpenGov/eurostat) package, nor
in the [restatapi-package](https://github.com/eurostat/restatapi).

## Installation

You can install the development version of comRex like so:

``` r
devtools::install_github("datapumpernickel/comRex")
```

The package is currenty only implementing access to the probably most
commonly used dataset by Eurostat, which is “EU trade since 2002 by
HS2-4-6 and CN8 (new content) (ds-059322)”. The methodology can be found
here:
<https://ec.europa.eu/eurostat/cache/metadata/en/ext_go_detail_sims.htm>

## Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(comRex)

data <- cr_get_data(
  freq = "A",
  reporter = "DE",
  partner = "GB",
  product = "2701",
  stat_procedure = NULL,
  indicators = "VALUE_IN_EUR",
  time = NULL,
  flow = "1",
  update = F, 
  verbose = T
)
```

## Controlled vocabularies

The respective arguments all have controllec vocabularies, which you can
check by looking ab the argument in this functions:

``` r
products <- cr_get_ref_table("product")

valid_codes <- products$code
```

In all cases, the variable `code` contains the valid codes, whereas
there is descriptions, which you can later merge back to the dataset, to
get a more nicely formatted dataset.

If you leave an argument on `NULL`, all possible values will be
returned.

This is under development, but most likely will only be used as is to
get quick access to larger amounts of data.

## Asychronous evaluation

Comext might sometimes not return the data, but tell you that it will be
provided asynchroniously, in that case, you need to execute the request
again after some time. This still needs to be implemented properly.
