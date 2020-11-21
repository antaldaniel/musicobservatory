#' Add saving path to indicator
#' 
#' Checks the saving path to the indicator, and if it does not exist, 
#' it tries to create it. If it fails, it returns a temporary directory 
#' for the current R session.
#' @param path A directory to save the indicators to. If \code{NULL} then
#' creates a \code{tempdir()} to save the data.
#' @return Returns the updated \code{path}.
#' @importFrom fs dir_exists dir_create
#' @importFrom purrr possibly

add_path_indicator <- function ( path = NULL, create_dir = TRUE ) {
  
  if (is.null(path)) {
    new_path = tempdir()
  } else if ( ## given path exists 
    fs::dir_exists(path) ) {
    new_path <- path
  } else {
    ## do you want to create non-existing directory or use tmpdir ?
    if ( create_dir ) { 
      
      message ( path,  " does not exists, creating it first.")
      is_created <- purrr::possibly(.f=  fs::dir_create, 
                                    otherwise = NULL)(path = path)
      
      if ( is.null(is_created)) {
        # Check if the directory could actually be created
        new_path <- tempdir() 
        warning(path, " could not be created, saving to ", new_path )
      } else {
        new_path <- as.character (is_created)
      }
    } else {
      # As a last possibility, return a temporary path
      new_path <- tempdir() 
      warning(path, " does not exists, saving to ", new_path )
    }
  }
  new_path
}
