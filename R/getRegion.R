#' Get Overall Temperature Values for a Requested Region
#'
#' @inheritParams buildURL
#'
#' @param region Character vector or function returning a character vector containing the names
#' of representative cities/locations in the chosen region. See USregions for
#' built-in continental US regions and examples for creating your own.
#' @param table Character vector determining which VC table the data will come from. Options are 'forecast', 'history', and 'historySummary'.
#' @param ... Arguments passed to the selected table. See vc.forecast, vc.history, vc.historySummary.
#'
#' @return A data.frame with columns 'datetime', 'mint', 'maxt' and 'temp'. See details.
#' @export
#'
#' @details Records are retrieved for all cities described in 'region' and for each date given
#' by the table and supplied arguments. Then, per date, the average temperature of all cities
#' is returned as 'temp', the minimum temperature out of all cities min temps is given as
#' 'mint' and the maximum temperature out of all cities max temps is given as 'maxt'.
#'
#'
#' @examples
#' \dontrun{getRegionTemp('APIKEY', northwest, 'forecast', forecastDays = '3')}
getRegionTemp <- function(apiKey,
                          region,
                          table = c('forecast',
                                    'history',
                                    'historySummary'),
                          ...){
  #Take first option for table
  table <- table[1]

  #Region input handling
  if(is.function(region)){
    region <- region()
  }else if(is.character(region)){
    region <- region
  }else{
    stop('region must be a character string or function that returns a character string.')
  }

  #Table handling
  if(!(table %in% c('forecast', 'history', 'historySummary'))){
    stop('Currently supported tables are forecast, history and historySummary.')
  }

  tableFunc <- eval(parse(text = paste0('vc.', table)))

  #Get all values
  resTab <- do.call('rbind', lapply(region, function(loc){
    return(tableFunc(apiKey = apiKey, locations = loc, shortColumnNames = 'true', ...))
  }))

  #(PER DATE) Average means and keep lowest min and highest max
  mins <- tapply(resTab$mint, INDEX = resTab$datetime, min, simplify = TRUE)
  maxs <- tapply(resTab$maxt, INDEX = resTab$datetime, max, simplify = TRUE)
  means <- tapply(resTab$temp, INDEX = resTab$datetime, mean, simplify = TRUE)

  overalls <- data.frame('datetime' = names(mins),
                         'mint' = mins,
                         'maxt' = maxs,
                         'temp' = means)
  return(overalls)
}
