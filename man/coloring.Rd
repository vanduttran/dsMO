% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/client_func.R
\name{coloring}
\alias{coloring}
\title{Coloring schema for plotting}
\usage{
coloring(
  logins,
  func,
  symbol,
  what,
  continuous_scale = rep(TRUE, length(what)),
  nbreaks = 2,
  colors = c("orange", "blue"),
  ...
)
}
\arguments{
\item{logins}{Login information of data repositories, where dsMOprimal is
installed.}

\item{func}{Definition of a function for preparation of raw data matrices.}

\item{symbol}{The symbol provided when calling the function \code{func} for
data preparation.}

\item{what}{The variable names used to map to color codes, in form of
'data.frame$variable' or c('data.frame$variable1', ...).}

\item{continuous_scale}{A logical value indicating whether the coloring
mapping is continuous. Default, TRUE.}

\item{nbreaks}{An integer indicating the number of intervals into which x is
to be cut, less than 1/10 of number of samples, when x is the coloring scale
is continuous.}

\item{colors}{A vector of colors to interpolate, must be a valid argument to
col2rgb(). Default: \code{c('orange', 'blue')}.}

\item{...}{arguments to pass to \code{colorRampPalette}}
}
\value{
A vector of The color codes for all samples
}
\description{
This functions mapped data values from a variable in the
virtual cohort to color codes.
}
\examples{
data(logindata)
data(procFunc)
\donttest{
resColors <- coloring(logins=logindata[1:2,],
                      func=procFunc$SingleOmics,
                      symbol='rawDataX',
                      what='rawDataX_discrete$PM_BMI_CONTINUOUS',
                      continuous_scale=TRUE, nbreaks=4)
resColors <- coloring(logins=logindata[1:2,],
                      func=procFunc$SingleOmics,
                      symbol='rawDataX',
                      what='rawDataX_discrete$PM_BMI_CATEGORICAL',
                      continuous_scale=FALSE, levels=NULL)
}
}
