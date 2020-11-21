#' Indicator for Music Observatory
#'
#' The class inherits all methods from a data frame, but has
#' many fixed attributes.
#'
#' @param x A data.frame or tibble.
#' @param  shortcode A short, unique, programatically usable indicator
#' ID.
#' @param description A precise character string describing the indicator 
#' for the data catalogue of the data observatory.
#' @param date_created The creation day or the refreshment day of the indicator.
#' @param date_earliest The date of the earliest observation. Can be a numeric containing a year.
#' @param date_latest The date of the latest observation.
#' @param original_source Defaults to {NA_character_}.
#' @param original_code The identifier in the original source, if applicable.
#' @param keyword1 The first keywords, must be one of the pillars of the 
#' observatory.
#' @param keyword2 The second keyword must be a topic within a pillar.
#' @param keyword3 A search term within the topic.
#' @param keywords A character vector of any optional, further keywords.
#' @rdname indicator
#' @return A data.frame or tibble with indicator attributes.
#' @importFrom dplyr distinct_all
#' @import assertthat
#' @importFrom pillar pillar_shaft
#' @examples
#' test_indicator <- indicator (
#'                      x <- data.frame ( 
#'                      geo = rep(c("NL", "BE", "LU"), 4), 
#'                      time = rep(c(2016:2019),3), 
#'                      values = runif(12, 1,100)
#'                      ), 
#'    shortcode = "observatory_test_1", 
#'    description = "A test indicator with random numbers", 
#'    date_created = as.Date ( "2020-08-24"),
#'    date_earliest  = min (x$time, na.rm=TRUE),
#'    date_latest  =  max(x$time, na.rm=TRUE),
#'    keyword1 = "test",  keyword2 = "random",  keyword3 = "Benelux"
#' )
#' 
#' ## Only the first 10 observations are printed 
#' print (test_indicator)

#' @export

indicator <- function(x, 
                      shortcode, 
                      indicator_name = NULL,
                      description,
                      date_created = NULL, 
                      date_earliest = NULL,
                      date_latest = NULL,
                      original_source = NA_character_,
                      original_code = NA_character_,
                      keyword1, 
                      keyword2, 
                      keyword3, 
                      keywords = NA_character_ ) {
  
  if ( is.null(date_created) ) {
    date_created <- Sys.Date() }
  
  
  assertthat::assert_that(is.data.frame(x))
  assertthat::assert_that(nchar(shortcode)>1, msg = 'Shortcode is too short or missing')
  assertthat::assert_that(nchar(keyword1)>1, msg = "keyword1 is too short or missing")
  assertthat::assert_that(nchar(keyword2)>1, msg = "keyword2 is too short or missing")
  assertthat::assert_that(nchar(keyword3)>1, msg = "keyword3 is too short or missing")
  assertthat::assert_that(nchar(description)>3, msg = "description is too short or missing")
  
  if ( is.null(indicator_name)) indicator_name <- shortcode
  if ( is.null(date_earliest)) {
    date_earliest = min(x$time, na.rm=TRUE)
  }
  
  if ( is.null(date_latest)) {
    date_latest = max(x$time, na.rm=TRUE)
  }

  if ( length(keywords) == 1 &  is.na(keywords[1])) {
    keywords <- c(keyword1, keyword2, keyword3)
  } else {
    keywords <- c(keyword1, keyword2, keyword3, keywords)
  }
  
  if (is.na(original_source)) {
    original_source <- "music.dataobservatory.eu"
  }
  
  new_indicator (x = x, 
                 shortcode = shortcode, 
                 indicator_name = indicator_name,
                 description = description,
                 date_created = date_created, 
                 date_earliest = date_earliest,
                 date_latest = date_latest, 
                 original_source = "music.dataobservatory.eu",
                 original_code = original_code,
                 keyword1 = keyword1, 
                 keyword2 = keyword2, 
                 keyword3 = keyword3, 
                 keywords = keywords )
  
}


#' @rdname indicator
#' @export
is.indicator <- function (x) inherits(x, "indicator")

#' @rdname indicator
#' @export
print.indicator <- function(x, ... ) {
  
  cat(paste0("indicator [", attr(x, "shortcode"), "] ", 
  paste( c( attr(x, "keyword1"), 
            attr(x, "keyword2"), 
            attr(x, "keyword3")
            ), 
            collapse = " - "), 
  "\n", attr(x, "description")))
  
  n_observations <- attr(x, "observations") 
  
  if ( n_observations > 10 ) {
    cat (paste0("\nThe first 10 observations of ", n_observations, "\n" ))
    print(head(as.data.frame(x),10))
  } else {
    print(as.data.frame(x))
  }
}

## not exported

new_indicator <- function(x, 
                          shortcode, 
                          indicator_name,
                          description,
                          date_created, 
                          date_earliest,
                          date_latest, 
                          observations, 
                          original_source = "music.dataobservatory.eu",
                          original_code = NA_character_,
                          keyword1, 
                          keyword2, 
                          keyword3, 
                          keywords = NA_character_ ) {
  
  ## This is an internal function for creating an object of 
  ## class indicator.
  
  
  if ( is.na(original_code) ) original_code <- shortcode
  
  indicator <- x
  attr(indicator, "shortcode") <- shortcode
  attr(indicator, "indicator_name") <- indicator_name
  attr(indicator, "description") <- description
  attr(indicator, "date_created") <- date_created
  attr(indicator, "date_earliest") <- date_earliest
  attr(indicator, "date_latest") <- date_latest
  attr(indicator, "observations") <- nrow(dplyr::distinct_all ( x ))
  
  attr(indicator, "keyword1") <- keyword1
  attr(indicator, "keyword2") <- keyword2
  attr(indicator, "keyword3") <- keyword3
  attr(indicator, "keywords") <- keywords
  
  attr(indicator, "observatory") <-  'music.dataobservatory.eu'
  
  class(indicator) <- c("indicator", class(indicator) ) 
  
  indicator
}
