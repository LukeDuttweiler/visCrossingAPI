getRegion <- function(apiKey,
                      region,
                      table = c('forecast',
                                'history',
                                'historySummary'),
                      ...){
  #Take first option
  table <- table[1]

  #Region input handling
  if(is.function(region)){
    region <- region()
  }else if(is.character(region)){
    region <- region
  }else{
    stop('region must be a character string or funciton that returns a character string.')
  }

  #Table handling
  if(!(table %in% c('forecast', 'history', 'historySummary'))){
    stop('Currently supported tables are forecast, history and historySummary.')
  }

  tableFunc <- eval(parse(text = paste0('vc.', table)))

  #Get all values
  resTab <- do.call('rbind', lapply(region, function(loc){
    return(tableFunc(apiKey = apiKey, locations = loc, ...))
  }))

  #GOTTA DO STUFF WITH THEM
}
