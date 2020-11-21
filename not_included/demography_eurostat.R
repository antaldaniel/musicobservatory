#' Download Demographic Data From Eurostat
#' 
#' Download the demographic data from Eurostat.
#' @importFrom dplyr bind_rows left_join select filter mutate
#' @importFrom dplyr distinct_all inner_join
#' @importFrom eurostat get_eurostat label_eurostat get_eurostat_toc
#' @examples
#' \dontrun{national_accounts_data()} 
#' @export 

demography_eurostat <- function( path = "not_included/music_society") {
  
  path <- add_path_indicator( path = path )
  toc <- eurostat::get_eurostat_toc()
  
  toc  %>%
    filter ( code == 'demo_pjan')
 
  population_raw <- eurostat_download_label('demo_pjan') %>%
    mutate ( geo = case_when ( 
      geo == "UK" ~ "GB", 
      geo == "EL"  ~ "GR", 
      TRUE ~ geo )
      )
  
  population <- population_raw %>%
    filter ( unit == "NR", 
             age == "TOTAL") %>%
    filter ( sex == "T") %>%
    mutate ( source = create_eurostat_source('demo_pjan'), #internal function in utils
             retrieve_date = Sys.Date(), 
             code = 'demo_pjan', 
             indicator_description = "National population on 1 January, both sexes, all ages",
             indicator = 'demo_pjan_TOTAL_T' ) %>%
    left_join (  select_toc_elements(toc, 'demo_pjan'),  # internal function in utils
                 by = "code" ) %>%
    dplyr::select (-code ) %>%
    select ( -all_of(c("sex", "age", "sex_label", "age_label")) ) %>%
    filter ( time  >= 1990 ) %>%
    mutate ( code = 'demo_pjan_TOTAL_T', 
             shortcode = "population_national")
  
  
  music_discovery_age <- c("Y15", "Y16",  "Y17", 
                           "Y18", "Y19", "Y20", "Y21", 
                           "Y22", "Y23", "Y24")
  
  music_discovery_population <- population_raw %>%
    filter ( unit == "NR", 
             age %in% music_discovery_age ) %>%
    filter ( sex == "T") %>%
    mutate ( source = create_eurostat_source('demo_pjan'), #internal function in utils
             retrieve_date = Sys.Date(), 
             code = 'demo_pjan' ) %>%
    left_join (  select_toc_elements(toc, 'demo_pjan'),  # internal function in utils
                 by = "code" ) %>%
    dplyr::select (-code ) %>%
    select ( -all_of(c("sex", "age", "age_label", "sex_label")) ) %>%
    filter ( time  >= 1990 ) %>%
    group_by_at( 
      all_of(c("unit", "geo", "time", "unit_label", "source", 
               "retrieve_date" ))) %>%
    summarise ( values = sum (values) ) %>%
    ungroup () %>%
    mutate ( indicator = 'demo_pjan_music_discovery', 
             shortcode = "population_national_music_discovery", 
             code = 'demo_pjan_TOTAL_T', 
             indicator_description = "National population on 1 January, both sexes, music discovery age 15-24")

  population_tables <- bind_rows ( population,
                                   music_discovery_population) 
  
  save_indicator_versions (population_tables,
                           path,
                           keyword1 = "economy",
                           keyword2 = "demand",
                           keyword3 = "driver",
                           keywords = c("population", "national population"))
}


