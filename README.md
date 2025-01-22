
# markovRt

markovRt is a toy-package to make random aRt based on a very simplified
Markov chain algorithm. It does not solve any real-world problems, but
might be fun to some fellow nerds.

## Installation

You can install the development version of markovRt from
[GitHub](https://github.com/LarsKul/markovRt) with:

``` r
install.packages("devtools")
devtools::install_github("LarsKul/markovRt")
```

## Example

Make a random aRtpiece, suitable as a screensaver

``` r
library(markovRt)
raw_art <- make_markov_art(size = 100, asp = 19.5/9, seed = 42)
plot_markov_art(data = raw_art, colour = viridisLite::mako(999), save = FALSE)
```

<img src="man/figures/README-example-1.png" width="100%" />
