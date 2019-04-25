# rdoc
[![Build Status](https://travis-ci.org/mdequeljoe/rdoc.svg?branch=master)](https://travis-ci.org/mdequeljoe/rdoc)
[![CRAN Status](https://www.r-pkg.org/badges/version/rdoc)](https://cran.r-project.org/package=rdoc)
[![codecov](https://codecov.io/gh/mdequeljoe/rdoc/branch/master/graph/badge.svg)](https://codecov.io/gh/mdequeljoe/rdoc)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

rdoc = `help` + `tools::Rd2txt` + [cli](https://github.com/r-lib/cli) + [crayon](https://github.com/r-lib/crayon) + [prettycode](https://github.com/r-lib/prettycode)

![](man/img/rdoc.png)


`rdoc` can also be used in console sessions whereby the output will be interactively printed by section to avoid flooding it with too much text. Any keypress except the 'Q' key will show the next doc section.

![](man/img/console.png)

## install

```r
devtools::install_github("mdequeljoe/rdoc")
```
## setting style options

text formatting can be set with `rdoc_text_formats` and then set via `options`

```r
library(crayon)
pkg <- combine_styles(bold, cyan)
options(rdoc.text_formats = rdoc_text_formats(pkg = pkg))
```
similarly, setting the overall style of the doc can be set with `rdoc_style`

```r
format_args <- function(x) paste0("@", x)
options(rdoc.style = rdoc_style(arguments = format_args))
```

## .Rd files

Refer directly to .Rd file content with `rdoc_rd`

## replacing `help` and `?`

`help` and `?` can be overridden via:

```r
rdoc::use_rdoc()
```

Returning to the base functions is possible with:

```r
rdoc::rm_rdoc()
```

## contributing

contributions/suggestions welcome!

