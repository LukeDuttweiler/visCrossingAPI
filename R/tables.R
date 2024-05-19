#' Get data from the 'forecast' table on VC Weather
#'
#' @description
#' This function guides construction of a request to the 'forecast' table on the VC Weather API. The parameter descriptions are quoted directly from the VC weather-api documentation and should be entered as character strings.
#'
#'
#' @inheritParams buildURL
#' @param locations One or more address, partial address or latitude, longitude values for the required locations . Addresses can be specified as full addresses. The system will also attempt to match partial addresses such as city, state, zip code, postal code and other common formats.
#'
#' When specifying a point based on longitude, latitude, the format must be specified as latitude,longitude where both latitude and longitude are in decimal degrees. latitude should run from -90 to 90 and longitude from -180 to 180 (with 0 being at the prime meridian through London, UK).
#'
#' Data for multiple locations can be requested in a single request by concatenating multiple locations using the pipe (|) character.
#' @param forecastDays Number of days to forecast.
#' @param alertLevel Logical. Include additional alerts if TRUE.
#' @param aggregateHours The interval between weather history data in the output. 1 represent hourly records, 24 represents a daily forecast. As the source data is recorded at the hourly level, 24 hour records are aggregated to indicate the predominant weather conditions during that time period. See also ‘aggregateMinutes’. Supported values 1 or 24, default is 24.
#' @param unitGroup The system of units used for the output data. Supported values are us,uk,metric,base.
#' @param outputDateTimeFormat specifies the date and time format used when returning the output.
#' @param includeAstronomy Retrieve astronomical data such as moon phase, sunrise and sunset times. Set to true to enable.
#' @param shortColumnNames When false, the returned dataset includes descriptive column names. When true, returns shorter, abbreviated column names with only alphanumeric characters. The short names are useful for programmatic use of the data and database data import.
#' @param contentType Selected between CSV or json output format. Supported values are ‘csv’ or ‘json’
#' @param locationMode Requests the structure that will be used for the output locations in the JSON output format. Supported values: single,array, or lookup. See the VC JSON documentation for more information on how this parameters affects the output JSON structure.
#' @param iconSet returns fixed set of of the icons names based on the weather conditions. Currently supported value is ‘ icons1’
#' @param lang  Sets the language of the translatable parts of the output such as the conditions field. Available languages are en, de, fr and es. In addition passing in ‘id’ will result in the raw descriptor IDs.
#'
#' @return data.frame with output of VC Weather call
#' @export
#'
#' @examples
#' \dontrun{tst <- vc.forecast('APIKEY', locations = 'Fort Wayne,IN', forecastDays = '2')}
vc.forecast <- function(apiKey,
                        locations = NULL,
                        forecastDays = '5',
                        aggregateHours = '24',
                        alertLevel = 'false',
                        includeAstronomy = 'false',
                        unitGroup = 'us',
                        outputDateTimeFormat = NULL,
                        shortColumnNames = 'false',
                        contentType = 'csv',
                        locationMode = 'single',
                        iconSet = 'icons1',
                        lang = 'en'){
  #Filter arguments
  if(is.null(outputDateTimeFormat)){rm(outputDateTimeFormat)}

  #Warn about asking for too long a forecast
  if(as.numeric(forecastDays) > 16){
    warning('Visual Crossing Weather only provides up to 16 days of forecasting.')
  }

  #Get all arguments except key
  callArgs <- as.list(environment())
  callArgs$table <- 'forecast'

  #Get URL
  urlString <- do.call('buildURL', callArgs)

  #Read correct type, check for errors/messages
  if(contentType == 'csv'){
    tab <- suppressWarnings(read.csv(urlString))

    if(nrow(tab) == 0){
      warning(paste0('VC API Returned Message: ', colnames(tab)))
    }

    return(tab)
  }else if(contentType == 'json'){
    tab <- RJSONIO::fromJSON(urlString)

    if(!is.null(tab$message) & !is.null(tab$errorCode)){
      warning(paste0('VC API Returned Message: ', tab$message, ' and error code ', tab$errorCode))
    }

    return(tab)
  }else{
    'contentType is invalid'
  }
}

#' Get data from the 'history' table on VC Weather
#'
#' @description
#' This function guides construction of a request to the 'history' table on the VC Weather API. The parameter descriptions are quoted directly from the VC weather-api documentation and should be entered as character strings.
#'
#'
#' @inheritParams buildURL
#' @param locations One or more address, partial address or latitude, longitude values for the required locations . Addresses can be specified as full addresses. The system will also attempt to match partial addresses such as city, state, zip code, postal code and other common formats.
#'
#' When specifying a point based on longitude, latitude, the format must be specified as latitude,longitude where both latitude and longitude are in decimal degrees. latitude should run from -90 to 90 and longitude from -180 to 180 (with 0 being at the prime meridian through London, UK).
#'
#' Data for multiple locations can be requested in a single request by concatenating multiple locations using the pipe (|) character.
#' @param aggregateHours The interval between weather history data in the output. 1 represent hourly records, 24 represents a daily forecast. As the source data is recorded at the hourly level, 24 hour records are aggregated to indicate the predominant weather conditions during that time period. See also ‘aggregateMinutes’. Supported values 1 or 24, default is 24.
#' @param aggregateMinutes The interval between weather history data in the output in minutes. This requires a plan that includes access to sub-hourly data. This parameter is used instead of ‘aggregateHours’. Use a factor of 60, such as 5,10,15 or 30.
#' @param combinationMethod Describes how multiple individual weather station observations should be combined. Options are aggregate, best and list.
#' @param unitGroup The system of units used for the output data. Supported values are us,uk,metric,base.
#' @param outputDateTimeFormat specifies the date and time format used when returning the output.
#' @param startDateTime The date time for the start of the data request using the time zone of the location. In the ISO format: yyyy-MM-ddTHH:mm:ss. Hours should be specified in 24 hour clock format.
#' @param endDateTime The date time for the end of the data request using the time zone of the location. In the ISO format: yyyy-MM-ddTHH:mm:ss. Hours should be specified in 24 hour clock format.
#' @param period Named data time range used instead of the startDateTime and endDateTime parameters. Available period values are “today”,”yesterday”,tomorrow”,”yeartodate”,”last30days”,”lastyear”,”last24hours”,”next30days”,”next7days” and “nextyear”.
#' @param dayStartTime When present and not set to the same as the dayEndTime, Filters the output to records that between the specified day times. This is useful for setting filters for business hours. Format h:m:ss (eg 9:00:00 would be 9am). When specified, the result can have no more than 30 days.
#' @param dayEndTime when present and not set to the same as the dayEndTime, filters the output to records that between the specified day times. When specified, the result can have no more than 30 days.
#' @param timezone specifies the timezone of the input and result dates and times. When not specified, all date times are considered local times. if you would like to specify that all dates are entered as UTC dates and times, use timezone=Z parameter.
#' @param collectStationContribution Whether to include a column describing the weather stations that were used for a particular output record. This can vary at the individual record level if weather stations do not transmit a full record and other stations are used as back ups.
#' @param includeAstronomy Retrieve astronomical data such as moon phase, sunrise and sunset times. Set to true to enable.
#' @param extendedStats Retrieve additional statistical values in the weather request including mean, maximum and minimum of the wind speed, wind chill and heat index. Set to true to enable.
#' @param maxDistance The maximum distance in meters used to search for local weather stations (default 50000m). This setting is combined with the maxStations parameter to find local weather stations.
#' @param shortColumnNames When false, the returned dataset includes descriptive column names. When true, returns shorter, abbreviated column names with only alphanumeric characters. The short names are useful for programmatic use of the data and database data import.
#' @param contentType Selected between CSV or json output format. Supported values are ‘csv’ or ‘json’
#' @param locationMode Requests the structure that will be used for the output locations in the JSON output format. Supported values: single,array, or lookup. See the VC JSON documentation for more information on how this parameters affects the output JSON structure.
#' @param iconSet returns fixed set of of the icons names based on the weather conditions. Currently supported value is ‘ icons1’
#' @param lang  Sets the language of the translatable parts of the output such as the conditions field. Available languages are en, de, fr and es. In addition passing in ‘id’ will result in the raw descriptor IDs.
#'
#' @return data.frame with output of VC Weather call
#' @export
#'
#' @examples
#' \dontrun{tst <- vc.history('APIKEY', locations = 'Fort Wayne,IN',
#'                             startDateTime = '1983-11-06',
#'                             endDateTime = '1983-11-09')}
vc.history <- function(apiKey,
                       locations = NULL,
                       startDateTime = NULL,
                       endDateTime = NULL,
                       period = NULL,
                       aggregateHours = '24',
                       aggregateMinutes = NULL,
                       combinationMethod = 'aggregate',
                       unitGroup = 'us',
                       outputDateTimeFormat = NULL,
                       dayEndTime = '0:0:00',
                       dayStartTime = dayEndTime,
                       timezone = NULL,
                       collectStationContribution = 'false',
                       includeAstronomy = 'false',
                       extendedStats = 'false',
                       maxDistance = '50000',
                       shortColumnNames = 'false',
                       contentType = 'csv',
                       locationMode = 'single',
                       iconSet = 'icons1',
                       lang = 'en'){
  #Errors for mistakes
  if(!is.null(startDateTime) & !is.null(period)){
    stop('Cannot specify both start/endDateTime and period.')
  }

  #Filter arguments
  if(!is.null(aggregateHours)){rm(aggregateMinutes)}else if(!is.null(aggregateMinutes)){rm(aggregateHours)}
  if(is.null(outputDateTimeFormat)){rm(outputDateTimeFormat)}
  if(!is.null(startDateTime)){
    rm(period)
  }else if(!is.null(period)){
    rm(startDateTime) ; rm(endDateTime)
  }

  #if(is.null(dayEndTime)){rm(dayEndTime) ; rm(dayStartTime)}
  if(is.null(timezone)){rm(timezone)}


  #Get all arguments except key
  callArgs <- as.list(environment())
  callArgs$table <- 'history'

  #Get URL
  urlString <- do.call('buildURL', callArgs)

  #Read correct type, check for errors/messages
  if(contentType == 'csv'){
    tab <- suppressWarnings(read.csv(urlString))

    if(nrow(tab) == 0){
      warning(paste0('VC API Returned Message: ', colnames(tab)))
    }

    return(tab)
  }else if(contentType == 'json'){
    tab <- RJSONIO::fromJSON(urlString)

    if(!is.null(tab$message) & !is.null(tab$errorCode)){
      warning(paste0('VC API Returned Message: ', tab$message, ' and error code ', tab$errorCode))
    }

    return(tab)
  }else{
    'contentType is invalid'
  }
}

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
#' \dontrun{tst <- vc.historySummary('APIKEY', locations = 'Fort Wayne,IN',
#'                                    minYear = '1983', maxYear = '1988',
#'                                    contentType = 'csv')}
vc.historySummary <- function(apiKey,
                              locations = NULL,
                              minYear = NULL,
                              maxYear = NULL,
                              chronoUnit = 'weeks',
                              breakBy = 'years',
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

  #Read correct type, check for errors/messages
  if(contentType == 'csv'){
    tab <- tryCatch(read.csv(urlString),
                    error = function(e){
                      warning('Could not read table. Not sure why.')
                      print(e)
                      data.frame(0)
                    },
                    warning = function(w){
                      if(grepl('400 Bad Request', as.character(w))){
                        warning('Most likely issue: Your API key does not have access to this table. HistorySummary requires Corporate or Enterprise subscription.')
                        data.frame(0)
                      }else if(grepl('incomplete final line', as.character(w))){
                        warning('Most likely issue: Your API key is incorrect. Below is the warning returned by VC Weather.')
                        warning(w)
                        data.frame(0)
                      }else{
                        print(w)
                        data.frame(0)
                      }
                    })

    return(tab)
  }else if(contentType == 'json'){
    tab <- tryCatch(RJSONIO::fromJSON(urlString),
                    error = function(e){
                      print('Could not read table. Not sure why.')
                      print(e)
                      data.frame(0)
                    },
                    warning = function(w){
                      if(grepl('400 Bad Request', as.character(w))){
                        warning('Most likely issue: Your API key does not have access to this table. HistorySummary requires Corporate or Enterprise subscription.')
                        data.frame(0)
                      }else if(grepl('incomplete final line', as.character(w))){
                        warning('Most likely issue: Your API key is incorrect. Below is the warning returned by VC Weather.')
                        warning(w)
                        data.frame(0)
                      }else{
                        print(w)
                        data.frame(0)
                      }
                    })

    if(!is.null(tab$message) & !is.null(tab$errorCode)){
      warning(paste0('VC API Returned Message: ', tab$message, ' and error code ', tab$errorCode))
    }

    return(tab)
  }else{
    'contentType is invalid'
  }
}
