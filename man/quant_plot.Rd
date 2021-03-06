% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/quant_plot.R
\name{quant_plot}
\alias{quant_plot}
\title{A quantile plot function}
\usage{
quant_plot(d, ylabel = "Quantity", quantiles = seq(0, 1, by = 0.25),
  color = "red")
}
\arguments{
\item{d}{An atomic vector of numbers to examine using the quantile plot}

\item{ylabel}{Label for the quantity being plotted; defaults to "Quantity"}

\item{quantiles}{Vector of numeric vector of probabilities with values in [0,1]
representing the quantile boundaries. This defaults to quartiles, e.g.: seq(0,1,by=.25).}

\item{color}{R color name (e.g., "blue") in which to render the colors indicating the quantiles; defaults to "red"}
}
\description{
This function takes an atomic vector of numbers and
creates a quantile plot of the data. This function requires
GGally, ggplot2, reshape2 (probably)
}
\examples{
quant_plot(d=df$height,ylabel="Height")
quant_plot(d=rnorm(1000),ylabel="Normal Distribution",quantiles=c(0,.33,.667,1),color="blue")
}
\keyword{plot,}
\keyword{quant_plot}
\keyword{quantile}

