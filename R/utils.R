#' Create Eurostat Source Description
#' 
#' @importFrom eurostat label_eurostat_tables
#' @keywords internal
 
create_eurostat_source <- function(code) {
  paste0( eurostat::label_eurostat_tables(code), " [Eurostat]")
}


eurostat_download_label <- function (code) {
  
  tmp <- eurostat::get_eurostat(code, time_format = "num") 
  join_by_vars <- names(tmp)[! names(tmp) %in% c("time", "values", "geo")]
  
  tmp %>% left_join ( eurostat_create_metadata_information(tmp), by = join_by_vars )
}

#' @importFrom dplyr select distinct_all left_join bind_cols
#' @importFrom eurostat label_eurostat
#' @importFrom purrr set_names
#' @keywords internal
 
eurostat_create_metadata_information <- function (dat) {
    
    unique_metadata <- dat %>%
      select ( -all_of(c("values", "time", "geo")) ) %>%
      distinct_all()
    
    unique_metadata %>% bind_cols( 
      unique_metadata %>% 
        eurostat::label_eurostat() %>%
        purrr::set_names (paste0(names(.), "_label")))
    
}

#' Small utils functions, not to be exported.
#' @param dat A data.frame acquired by eurostat::get_eurostat and created
#' by create_indicator
#' @param values_var The name of the column that has the indicator values,
#' defaults to \code{'values'}.
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select group_by_all add_count mutate ungroup
#' @importFrom dplyr filter
#' @importFrom tidyselect all_of
#' @importFrom purrr safely possibly set_names
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom stringr str_sub
#' @keywords internal
#' 

select_toc_elements <- function( toc, code ) {
  toc %>%
    dplyr::filter ( code %in% code ) %>%
    dplyr::select ( all_of(c("code", "last update of data")) ) %>%
    purrr::set_names ( c("code", "last_update_at_source"))
  
}

create_eurostat_source <- function(code) {
  paste0( eurostat::label_eurostat_tables(code), " [Eurostat]")
}

check_duplicates <- function(dat, values_var = 'values') {
  
  n <- NULL
  
  duplicates <- dat %>%
    select ( tidyselect::all_of(c("time", "geo", values_var)) ) %>%
    group_by_all () %>%
    add_count () %>%
    filter ( n > 1 )
  
  if ( nrow(duplicates)>0) {
    warning ( paste ( duplicates$geo, collapse = ", " ), " are duplicated")
  }
  dat
}

check_dataframe <- function(dat) {
  
  if ( ! all(c("time", "values", "geo") %in% names(dat)) )
  {
    stop("Columns 'time', 'values' and 'geo' must be present in the data frame")
  }
  
  
}

add_method <- function(dat) {
  
  values <- NULL
  
  dat %>% mutate (  method = ifelse ( is.na(values),
                                      yes = 'missing',
                                      no = 'actual'))
  
}

download_eurostat <- function ( code ) {
  
  retrieve_date <- NULL
  
  safely_get_eurostat <- purrr::safely (eurostat::get_eurostat)
  this_table <- safely_get_eurostat(code)
  
  if ( is.null(this_table$error) & !is.null(this_table$result) ) {
    this_table$result
  } else {
    data.frame(
      eurostat_code = code,
      geo = NA_character_,
      values = NA_real_,
      unit = "missing_data",
      retrieve_date = retrieve_date,
      missing_rate = 1)
  }
}


#missing_rate <- sum(is.na(eurostat_wide))/sum( ! is.na(dat$values))


create_eurostat_table <- function ( dat, code, retrieve_date ) {
  
  . <- create_tables <- NULL
  
  dat %>%
    create_tables ( eurostat_table =.,
                    code = code ) %>%
    mutate ( eurostat_code = code,
             retrieve_date = retrieve_date)
}

count_unique_code16 <- function(df) {
  code16 <- unique(df$code16)
  length(code16[which(nchar(code16)==4)])
}

count_unique_code16_2013 <- function(df) {
  
  code16 <- NULL
  
  df %>%
    dplyr::filter ( time == as.Date ("2013-01-01") ) %>%
    dplyr::select ( code16 ) %>%
    dplyr::filter ( nchar(code16) == 4 ) %>%
    unlist () %>% unique () %>% length()
  
}

count_unique_code16_2013 <- function(df) {
  
  code16 <- time <- NULL
  
  df %>%
    dplyr::filter ( time == as.Date ("2013-01-01") ) %>%
    dplyr::select ( code16 ) %>%
    dplyr::filter ( nchar(code16) == 4 ) %>%
    unlist () %>% unique () %>% length()
  
}

remove_extraterritorial <- function ( dat )  {
  
  geo <- NULL
  
  not_xx <- dat$geo[stringr::str_sub ( dat$geo, -2,-1) != "XX"]
  not_zz <- dat$geo[stringr::str_sub ( dat$geo, -2,-1) != "ZZ"]
  dat %>%
    filter ( geo %in% not_xx ) %>%
    filter ( geo %in% not_zz )
}


interpolate_values <- function ( dat,
                                 approx_method = 'linear',
                                 maxgap_interpolate = 3,
                                 maxgap_nocb = 2,
                                 maxgap_locf = 1,
                                 min_date = as.Date ("2011-01-01"),
                                 max_date = as.Date ("2020-01-01")
) {
  . <- time <- geo <- cod16 <- NULL
  
  dat <- dplyr::ungroup(dat)
  
  approx <- dat %>%
    approximate_interpolate(., approx_method = approx_method,
                            maxgap = maxgap_interpolate ) %>%
    filter ( time  >= min_date) %>%
    filter ( time  <= max_date )
  names ( approx )
  
  check_duplicates ( approx )
  
  dat <- approx
  nocb <-   approx %>%
    approximate_nocb ( ., maxgap_nocb )
  
  nocb16 <- nocb %>%
    filter ( geo == code16 )
  
  check_duplicates ( nocb )
  
  locf <- nocb %>%
    approximate_locf ( ., maxgap_locf )
  
  check_duplicates ( locf )
  
  locf
}

mutate_constants <- function(dat, constant_df) {
  
  
  add_df <- as.data.frame(t(as.matrix(sapply (constant_df, class))))
  
  factor_df <- constant_df[, which (is.factor(constant_df))]
  character_df <- constant_df[, which(is.character(constant_df))]
  numeric_df <- constant_df[!names(constant_df) %in% c(names(character_df), names(factor_df))]
  
  which(sapply(constant_df, class)=="factor")
  
  character_df
  
  add_factor_df <- add_df[, which(sapply(constant_df, class)=="factor")]
  
  add_character_df <- apply ( )
  
  
}


#possibly_interpolate <- purrr::possibly(interpolate_values, NULL)


check_fr <- function(dat) {
  
  . <- time  <- country_code <- values <- geo <- NULL
  code16 <- NULL
  
  dat %>%
    mutate ( country_code = ifelse(is.na(geo), code16, geo )) %>%
    mutate ( country_code = stringr::str_sub(country_code,1,2)) %>%
    filter ( country_code %in% c("FR", "PL")) %>%
    filter ( time > as.Date('2011-01-01'),
             time < as.Date('2018-01-01'))
  
  
}

fix_indicator <- function ( eurostat_table, id) {
  
  unit <- unit_name <- indic_sb <- id <- indic_sb <- NULL
  
  if (! 'unit' %in% names(eurostat_table) ) {
    if ( id == "sbs_r_nuts06_r2") {
      
      metadata <- create_metadata_table(eurostat_table, id) %>%
        dplyr::rename  (unit_correct = unit )
      
      eurostat_table %>%
        mutate ( unit = tolower(indic_sb)) }
    
  }
}

