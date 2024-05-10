#' Helper function to build a URL matching the VC API format
#'
#' @param apiKey User-specific API Key provided by Visual Crossing Weather
#' @param table Name of the table the data will come from
#' @param ... Named arguments specific to the selected table
#'
#' @return String. The URL required to access the data at VC Weather
#'
buildURL <- function(apiKey = NULL, table = NULL, ...){
  #Check for apiKey
  if(is.null(apiKey)){
    stop('API key (from Visual Crossing account) must be provided.')
  }

  #Check for other missing inputs (which come in as NULL values)
  argg <- list(...)
  if(any(sapply(argg, is.null))){
    stop(paste0('Input must be supplied for ',
                paste0(names(argg)[which(sapply(argg, is.null))], collapse = ', ')))
  }

  #All tables start with this header
  header <- 'https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/'

  #Attach table
  urlString <- paste0(header, table, '?')

  #Attach table specific arguments and API Key
  tabArgs <- c(argg, key = apiKey)
  tabArgsExp <- paste(names(tabArgs), tabArgs, sep = '=', collapse = '&')
  urlString <- paste0(urlString, tabArgsExp)

  return(urlString)
}
