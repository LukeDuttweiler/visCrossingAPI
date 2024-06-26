% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRegion.R
\name{getRegionTemp}
\alias{getRegionTemp}
\title{Get Overall Temperature Values for a Requested Region}
\usage{
getRegionTemp(
  apiKey,
  region,
  table = c("forecast", "history", "historySummary"),
  ...
)
}
\arguments{
\item{apiKey}{User-specific API Key provided by Visual Crossing Weather}

\item{region}{Character vector or function returning a character vector containing the names
of representative cities/locations in the chosen region. See USregions for
built-in continental US regions and examples for creating your own.}

\item{table}{Character vector determining which VC table the data will come from. Options are 'forecast', 'history', and 'historySummary'.}

\item{...}{Arguments passed to the selected table. See vc.forecast, vc.history, vc.historySummary.}
}
\value{
A data.frame with columns 'datetime', 'mint', 'maxt' and 'temp'. See details.
}
\description{
Get Overall Temperature Values for a Requested Region
}
\details{
Records are retrieved for all cities described in 'region' and for each date given
by the table and supplied arguments. Then, per date, the average temperature of all cities
is returned as 'temp', the minimum temperature out of all cities min temps is given as
'mint' and the maximum temperature out of all cities max temps is given as 'maxt'.
}
\examples{
\dontrun{getRegionTemp('APIKEY', northwest, 'forecast', forecastDays = '3')}
}
