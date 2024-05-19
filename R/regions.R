#This file contains helper functions that provide 4 cities for each
#Continental US climate region as defined by Karl and Koss (1984)
#The 4 cities were selected to give a general idea of weather patterns
#in each region, and so are spaced out (somewhat) evenly across the region

#' @name USregions
#' @rdname USregions
#'
#' @title Selection of cities in Continental US Climate Regions
#'
#' @description These functions return (somewhat haphazardly chosen) cities in Continental US climate
#' regions as defined by Karl and Koss (1984).
#'
#' @return A character vector in the format Cityname, ST.
NULL


#'
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' northwest()
northwest <- function(){
 return(c('Seattle, WA',
          'Boise, ID',
          'Spokane, WA',
          'Eugene, OR'))
}

#' Gives 4 locations in the 'Northern Rockies and Plains' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' northRockies()
northRockies <- function(){
  return(c('Great Falls, MT',
           'Lincoln, NE',
           'Casper, WY',
           'Bismarck, ND'))
}

#' Gives 4 locations in the 'Upper Midwest' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' upperMidwest()
upperMidwest <- function(){
  return(c('Minneapolis, MI',
           'Des Moines, IA',
           'Green Bay, WI',
           'Ann Arbor, MI'))
}

#' Gives 4 locations in the 'Northeast' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' northeast()
northeast <- function(){
  return(c('Rochester, NY',
           'Philadelphia, PA',
           'Boston, MA',
           'Waterville, ME'))
}

#' Gives 4 locations in the 'West' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' west()
west <- function(){
  return(c('Los Angeles, CA',
           'Redding, CA',
           'Las Vegas, NV',
           'Wells, NV'))
}

#' Gives 4 locations in the 'Southwest' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' southwest()
southwest <- function(){
  return(c('Phoenix, AZ',
           'Albuquerque, NM',
           'Denver, CO',
           'Salt Lake City, UT'))
}

#' Gives 4 locations in the 'South' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' south()
south <- function(){
  return(c('San Antonio, TX',
           'Wichita, KS',
           'Little Rock, AR',
           'New Orleans, LA'))
}

#' Gives 4 locations in the 'Ohio Valley' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' ohioValley()
ohioValley <- function(){
  return(c('St. Louis, MO',
           'Chicago, IL',
           'Cleveland, OH',
           'Nashville, TN'))
}

#' Gives 4 locations in the 'Southeast' US Climate Region
#'
#' @export
#' @rdname USregions
#'
#' @examples
#' southeast()
southeast <- function(){
  return(c('Richmond, VA',
           'Charleston, SC',
           'Birmingham, AL',
           'Orlando, FL'))
}
