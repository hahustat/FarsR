
#' Reading data from file
#' Funtions reads data from Fatality Analysis Reporting System
#' The function will read the defined data file into data table.
#' If the data file is not available it will give an error message
#'
#' @param filename string indicating the data name
#'
#' @return The function will read the file extablishe in the function parameter, and loads it in to a data table
#' If file does not exist, the function will return an error message.
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Generate filename
#'
#' The function generates a "accident_year.csv.bz2" filename,
#' where "year" is the parameter given by the user.
#'
#' @param year an integer
#'
#' @return The function generates a filename with the year number given by the user.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata",
              sprintf("accident_%d.csv.bz2", year),
              package = "Fars",
              mustWork = TRUE
              )
}

#' Reads several years of data from the data set
#'
#' The function takes the years specified by the user has a parameter and
#' creates a table for each year. Each table is a list with two collumns (year, month)
#'
#' @param years a list of integers representing the years
#'
#' @return The function a data table for each year
#' for year not present in the data, the function gives a warning message
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Outputs a summary of the years and months in the data set
#'
#' Based on the years provided by the user has a parameter,
#' the funcion prints out the summary of observations grouped by years and months
#'
#' @param years a list of integers representing the years
#'
#' @return The function a prints out a summary table of the observations
#' grouped by year/month
#'
#' @examples
#' \dontrun{
#' fars_summarize_years (2013:2015)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#' Plots the acidents on a map for a user specified year and state
#'
#' User specifies year and state (as function parameters) and the function
#' will plot the acidents for that given year, on a map of the state
#' Acidents will be ploted has dots, based on their latitude and longitude
#'
#' @param year interger specifying the year of data to be ploted
#' @param state.num interger specifying the state
#'
#' @return The function plots the acidents for a given year
#' on a map of the state
#'
#' @examples
#' \dontrun{
#' fars_map_state (45,2014)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
