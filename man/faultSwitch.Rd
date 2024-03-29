% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/faultSwitch.R
\name{faultSwitch}
\alias{faultSwitch}
\title{Induce the Specified Fault on NOC Observations}
\usage{
faultSwitch(
  df,
  fault,
  period = 7 * 24 * 60,
  faultStartIndex = round(0.8433 * period),
  shift = 2,
  postStateSplit = FALSE
)
}
\arguments{
\item{df}{A data frame returned by the processNOCdata() function.}

\item{fault}{A character string. Options are "NOC", "A1", "B1", "C1", "A2",
"B2", "C2", "A3", "B3", or "C3". See "details" of mspProcessData() for more
information.}

\item{period}{The observation cycle length. Defaults to one week's worth of
minute-level observations (10,080 observations).}

\item{faultStartIndex}{An integer specifying the index at which the faults
will start. Defaults to roughly 85 percent through the cycle.}

\item{shift}{The fault parameter for faults "A1" and "B1" corresponding to
the positive shock value added to features. Defaults to 2. See "details" of
mspProcessData() for more information.}

\item{postStateSplit}{Should we induce faults before or after state-splitting?
Defaults to FALSE. Make this argument TRUE for faults 1C, 2C, 3C.}
}
\value{
A data frame with the same structure as df, but with faults induced
  across all observations. The mspProcessData() function then subsets the
  observations necessary to corrupt the normal data frame, and binds them
  together by row. This function is called by mspProcessData(). See
  ?mspProcessData for more details.
}
\description{
Infect the input data frame with a specific fault, then return
  the infected data frame.
}
\details{
The faults return data frames as follows: \describe{
  \item{A1 -- }{A data frame with 10080 rows and five columns, corresponding
    by default to one week worth of data recorded at a 1-minute interval (as
    defined by the "period" argument of this function and the "increment"
    argument of the processNOCdata() function). The fault is a system shift
    to each of the three features by 2 (the "shift" argument). The fault
    starts at row 8500 (specified by the argument "faultStartIndex"), and the
    five columns under the fault state are defined here: \describe{
      \item{dateTime : }{a POSIXct column}
      \item{state : }{the state indicator for the multivariate system, with
        three levels when the argument "multiState" is TRUE and one level
        otherwise}
      \item{x : }{x(t) = t + shift + error}
      \item{y : }{y(t) = t ^ 2 - 3t + shift + error}
      \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + shift + error}
    }
    where t is a 10080-entry vector of autocorrelated and non-stationary
    hidden process realizations generated within the processNOCdata() function.
  }
  \item{B1 -- }{A matrix as defined in A1, but with x, y, and z feature
    columns defined as follows: \describe{
      \item{x : }{x(t) = t + shift + error}
      \item{y : }{y(t) = t ^ 2 - 3t + error}
      \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
    }
  }
  \item{C1 -- }{A matrix as defined in A1, but with x, y, and z feature
    columns defined as follows: \describe{
      \item{x : }{x(t) = t + shift / 4 + error}
      \item{y : }{y(t) = t ^ 2 - 3t + error}
      \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + shift / 4 + error}
    }
    This shift is applied only in State 3.
  }
  \item{A2 -- }{The fault is a drift on each feature by
    (s - faultStartIndex / 10 ^ 3, where s is the observation index. The
    fault starts at "faultStartIndex", and the x, y, and z feature columns
    are defined as follows: \describe{
      \item{x : }{x(t) = t + drift + error}
      \item{y : }{y(t) = t ^ 2 - 3t + drift + error}
      \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + drift + error}
    }
  }
  \item{B2 -- }{The fault is a drift a drift on the "y" and "z" feature by
    (s - faultStartIndex / 10 ^ 3, where s is the observation index. The
    fault starts at "faultStartIndex", and the x, y, and z feature columns
    are defined as follows: \describe{
      \item{x : }{x(t) = t + error}
      \item{y : }{y(t) = t ^ 2 - 3t + drift + error}
      \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + drift + error}
    }
  }
  \item{C2 -- }{The fault is a negative drift on the "y" feature by 1.5 *
    (s - faultStartIndex) / (period - faultStartIndex). Thus, \describe{
      \item{x : }{x(t) = t + error}
      \item{y : }{y(t) = t ^ 2 - 3t - drift + error}
      \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
    }
    This drift is applied only in State 2.
  }
  \item{A3 -- }{The fault is a signal amplification in the determining latent
    t vector. The fault starts at "faultStartIndex", and the x, y, and z
    features under the fault state are defined here: \describe{
      \item{x : }{x(t_*) = t_* + error}
      \item{y : }{y(t_*) = (t_*) ^ 2 - 3t_* + error}
      \item{z : }{z(t_*) = -(t_*) ^ 3 + 3(t_*) ^ 2 + error}
    }
    where t_* = 5 x t x (period - s) / (period - faultStartIndex) and s is
    the observation index.
  }
  \item{B3 -- }{The fault is a signal amplification in the determining latent
    t vector for the "z" feature only. The fault starts at "faultStartIndex",
    and the x, y, and z features under the fault state are defined here:
    \describe{
      \item{x : }{x(t) = t + error}
      \item{y : }{y(t) = (t) ^ 2 - 3t + error}
      \item{z : }{z(t_*) = -(t_*) ^ 3 + 3(t_*) ^ 2 + error}
    }
    where t_* = 3 x t x (period - s) / (2 x period) and s is the observation
    index.
  }
  \item{C3 -- }{This fault is a change in the error structure of feature "y".
    We let errorNew = 2 * error - 0.25, so that \describe{
      \item{x : }{x(t) = t + error}
      \item{y : }{y(t) = t ^ 2 - 3t + errorNew}
      \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
    }
    This new error structure is applied only in State 2.
  }
}
}
\examples{
nrml <- processNOCdata()
faultSwitch(nrml, fault = "NOC")
}
\seealso{
Called by: \code{\link{mspProcessData}}.
}
