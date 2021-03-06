% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plot accidents in a state}
\usage{
fars_map_state(state.num, year, fname_transform = identity)
}
\arguments{
\item{state.num}{state number (matching with state number in
FARS data)}

\item{year}{year to plot accidents for}

\item{fname_transform}{Read \link{fars_read_years} documentation to find out.}
}
\value{
draws a plot if inputs are valid, otherwise returns
without plotting
}
\description{
Given a state number which matches with state numbers in FARS
data, this function draws map of the state overlaid with dots
representing accidents.
}
\details{
Function throws error if state number is invalid, and does
not plot anything if there are no accidents to plot.
}
\examples{
\dontrun{
fars_map_state(1, 2013)
}

}
