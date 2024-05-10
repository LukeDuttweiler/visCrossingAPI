#' Get data from the 'historysummary' table on VC Weather
#'
#' @description
#' This function guides construction of a request to the 'historysummary' table on the VC Weather API. The parameter descriptions are quoted directly from the VC weather-api documentation and should be entered as character strings.
#'
#'
#' @inheritParams buildURL
#' @param locations One or more address, partial address or latitude, longitude values for the required locations . Addresses can be specified as full addresses. The system will also attempt to match partial addresses such as city, state, zip code, postal code and other common formats.
#'
#' When specifying a point based on longitude, latitude, the format must be specified as latitude,longitude where both latitude and longitude are in decimal degrees. latitude should run from -90 to 90 and longitude from -180 to 180 (with 0 being at the prime meridian through London, UK).
#'
#' Data for multiple locations can be requested in a single request by concatenating multiple locations using the pipe (|) character.
#' @param chronoUnit The unit of time used to create the summary report. Supported values are years, months, weeks, days. Default is weeks.
#' @param breakBy How to aggregate the data across years and time periods. Breaking by ‘years’ indicates that individual years are returned in the output, breaking by ‘self’ indicates the same time period for all years is summarized. Breaking by ‘none’ collapses all records to a single summary row.
#'
#' Supported values are years,self,none. Default is years.
#' @param minYear The initial year in the range of years to be summarized by this query. The weather database begins on 1/1/1970, so 1970 is the earliest allowable year.
#' @param maxYear The final year in the range of years to be summarized by this query. You can specify any year between the minYear and the current calendar year inclusive.
#' @param unitGroup The system of units used for the output data. Supported values are us,uk,metric,base. See Unit groups and measurement units for more information. Default is us.
#' @param dailySummaries Whether or not to include data for the day level means within the dataset. Default is false.
#' @param shortColumnNames When false, the returned dataset includes descriptive column names. When true, returns shorter, abbreviated column names with only alphanumeric characters. The short names are useful for programmatic use of the data. Default is false.
#' @param maxDistance The maximum distance in meters used to search for local weather stations (default 50000m). This setting is combined with the maxStations parameter to find local weather stations.
#' @param maxStations Maximum number of weather stations used to calculate a weather record (default 3). Closer weather stations are weighted signficantly more heavily than farther stations.
#' @param contentType Selected between CSV or json output format. Supported values are ‘csv’ or ‘json’. Default is 'csv'.
#' @param locationMode Requests the structure that will be used for the output locations in the JSON output format. Supported values: single,array, or lookup. See the VC Weather JSON documentation for more information on how this parameters affects the output JSON structure. Default is single.
#'
#' @return t
#' @export
#'
#' @examples
#' #Something
vc.historySummary <- function(apiKey,
                              locations = NULL,
                              chronoUnit = 'weeks',
                              breakBy = 'years',
                              minYear = NULL,
                              maxYear = NULL,
                              unitGroup = 'us',
                              dailySummaries = 'false',
                              shortColumnNames = 'false',
                              maxDistance = '50000',
                              maxStations = '3',
                              contentType = 'csv',
                              locationMode = 'single'){

  #Get all arguments except key
  callArgs <- as.list(environment())
  callArgs$table <- 'historysummary'
  callArgs$aggregateHours = '24'

  #Get URL
  urlString <- do.call('buildURL', callArgs)

  #Read correct type
  if(contentType == 'csv'){
    return(read.csv(urlString))
  }else if(contentType == 'json'){
    return(RJSONIO::fromJSON(urlString))
  }else{
    'contentType is invalid'
  }
}