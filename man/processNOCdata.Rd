% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processNOCdata.R
\name{processNOCdata}
\alias{processNOCdata}
\title{Simulate NOC Observations from a Single-State or Multi-State Process}
\usage{
processNOCdata(
  startTime = "2015-05-16 10:00:00 CST",
  period = 7 * 24 * 60,
  stateDuration = 60,
  increment = "min",
  multiState = TRUE,
  autocorellation = 0.75,
  tLower = 0.01,
  tUpper = 2,
  errVar = 0.01
)
}
\arguments{
\item{startTime}{a POSIXct object specifying the day and time for the
starting observation.}

\item{period}{The observation cycle length. Defaults to one week's worth of
minute-level observations (10,080 observations).}

\item{stateDuration}{The number of observations generated during a stay in
each state. Defaults to 60.}

\item{increment}{The time-sequence base increment. See "Details" of the
seq.POSIXt() function options. Defaults to "min" for minutes.}

\item{multiState}{Should the observations be generated from a multi-state
process? Defaults to TRUE.}

\item{autocorellation}{The autocorrelation parameter. Must be less than 1 in
absolute value, or the process generated will be nonstationary. Defaults to
0.75 in accordance to Kazor et al (2016).}

\item{tLower}{Lower bound of the latent $t$ variable. Defaults to 0.01.}

\item{tUpper}{Upper bound of the latent $t$ variable. Defaults to 2.}

\item{errVar}{Error variance of the normal white noise process on the feature
variables.}
}
\value{
An data frame with the following information: \describe{
  \item{dateTime -- }{A POSIXct column of times starting at the user-defined
    `startTime` argument, length given by the `period` argument, and spacing
    given by the `increment` argument. For example, if the starting value is
    "2016-01-10", period is 10080, and the incrementation is in minutes, then
    this sequence will be one week's worth of observations recorded every
    minute from midnight on the tenth of January.}
  \item{state -- }{An integer column of all 1's (when the `multiState`
    argument is FALSE), or a column of the state values (1, 2 or 3).}
  \item{x -- }{A double column of generated values for the first feature.}
  \item{y -- }{A double column of generated values for the second feature.}
  \item{z -- }{A double column of generated values for the third feature.}
  }
}
\description{
This function generates data under normal operating conditions
  from a single-state or multi-state process model.
}
\details{
This function randomly generates a non-stationary (sinusoidal) and
  autocorrelated latent variable t with lower and upper bounds given by the
  arguments "tLower" and "tUpper", respectively, with autocorrelation
  governed by the "autocorrelation" argument. Necessarily, this coefficient
  must be less than 1 in absolute value, otherwise the latent variable will
  be unbounded. Next, this function draws a realization of this random
  variable t and calculates three functions of it, then jitters these
  functions with a normal white noise variable (with variance set by
  "errVar"). These three functions are: \describe{
    \item{x : }{x(t) = t + error}
    \item{y : }{y(t) = t ^ 2 - 3t + error}
    \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
  }
  This function is called by the mspProcessData() function. See
  ?mspProcessData for more details.
}
\examples{
processNOCdata()
}
\seealso{
Called by: \code{\link{mspProcessData}}.
}
