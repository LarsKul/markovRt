% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_markov_art.R
\name{plot_markov_art}
\alias{plot_markov_art}
\title{Plot a Random Artpiece Generated by \code{make_markov_art} function}
\usage{
plot_markov_art(data, colour = viridisLite::mako(9999), save = FALSE)
}
\arguments{
\item{data}{a matrix generated by \code{make_markov_art}}

\item{colour}{a colour pallet to use for plotting}

\item{save}{should plot be saved to disc?}
}
\value{
a plot generated by \code{image}
}
\description{
Plot a Random Artpiece Generated by \code{make_markov_art} function
}
\examples{
raw_data <- make_markov_art(size = 10, asp = 1, seed = 42)
plot_markov_art(data = raw_data, colour = viridisLite::mako(9999), save = FALSE)
}
