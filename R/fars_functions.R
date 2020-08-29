#' @title fars_read
#' @description
#' Given a path to a csv file, return a tibble
#' which contains the file data. Will stop
#' if the file does not exist.
#'
#' @param filename A string which contain the path
#' to the file you want to read.
#' @return A tibble containing the file data
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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

#' @title make_filename
#' @description
#' Given a year, return the filename of the
#' FARS csv file for that year. Will throw an
#' error if the year cannot be converted into
#' a number.
#'
#' @param year A year in yyyy format
#' @return A string with the filename
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' @title fars_read_years
#' @description
#' Accepts a list or vector of years. Returns a
#' list of tibbles with the casualty data for each
#' year. Will throw an error for an invalid year.
#'
#' @param years A list or vector of years to read
#' @return A list of tibbles each with FARS
#' casualty data for the corresponding years.
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014))
#' }
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr "%>%"
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

#' @title fars_summarize_years
#' @description
#' Fetches the accident summary for each month for
#' each of the years given as input.
#'
#' @param years A list or vector of years to read
#' @return A tibble whose rows are months.
#' Each column represents one year and the values
#' are the number of accidents in that month
#' and year
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014))
#' }
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr "%>%"
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title fars_map_state
#' @description
#' Accepts a FARS state code and a year and returns
#' a map where every accident in that state for
#' that year is represented by a point. Will throw
#' an error for invalid state codes.
#' x = longitude and y = latitute
#'
#' @param state.num State code in FARS data
#' @param year A numeric year in yyyy format
#' @return A map with the location of every accident
#' in the specified state during the specified
#' year.
#' @examples
#' \dontrun{
#' fars_map_state("01", 2013)
#' }
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
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
