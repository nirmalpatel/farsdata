#' Read FARS data
#'
#' This function is used to import Fatality Analysis Reporting System (FARS) data.
#' It is not specific to FARS. Error will be thrown if file does not exist.
#'
#' @param filename path to CSV file to be read (compressed CSV files will be
#' uncompressed automatically)
#'
#' @return a tibble containing data
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    read_csv(filename, progress = FALSE)
  })
  tbl_df(data)
}

#' Make FARS data filename from year
#'
#' This functions creates a full FARS data file name given a year
#'
#' @param year year of FARS data (coerced to integer)
#'
#' @return a FARS data file name corresponding to given year
#'
#' @examples
#' make_filename(2002)
#' make_filename(1995)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data of multiple years
#'
#' This function is for reading FARS data of multiple years. You
#' can provide a vector of years which will be converted to appropriate
#' file name using \link{make_filename}. After that, function will attempted to read
#' each file name. If filename does not exist, warning will be shown and \code{NULL}
#' will be returned for that years data.
#'
#' @param years a vector of years (will be coerced to integer)
#' @param fname_transform a function to be applied to filename that \code{make_filename}
#' contructs, default is \code{identity} which does not change filename. This is helpful
#' in cases where you want to add custom path prefix to all filenames.
#'
#' @return list of dataframes containing data for each year. names of the list
#' will be years and their corresponding values will have data for that year.
#'
#' @details If data for a year does not exist, returning list will have
#' \code{NULL} value associated with it
#'
#' @importFrom dplyr mutate_ select_
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @export
fars_read_years <- function(years, fname_transform = identity) {
  lapply(years, function(year) {
    file <- make_filename(year)
    file <- fname_transform(file)
    tryCatch({
      dat <- fars_read(file)
      mutate_(dat, year = ~ year) %>%
        select_(~ MONTH, ~ year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarise FARS data
#'
#' This function derives summary metrics for every year and month
#' for FARS data. First it imports data using \link{fars_read_years} and
#' then produces a pivot table showing row count for every year and month.
#' Months are in rows and years are in column.
#'
#' @param years a vector of years (will be coerced to integer)
#' @param ... further arguments to be passed to \code{fars_read_years}
#'
#' @return tibble having summary metrics
#'
#' @importFrom dplyr group_by_ summarize_ bind_rows
#' @importFrom tidyr spread_
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2014)
#' fars_summarize_years(2013:2015)
#' }
#'
#' @export
fars_summarize_years <- function(years, ...) {
  dat_list <- fars_read_years(years, ...)

  bind_rows(dat_list) %>%
    group_by_(~ year, ~ MONTH) %>%
    summarize_(n = ~ n()) %>%
    spread_("year", "n")
}

#' Plot accidents in a state
#'
#' Given a state number which matches with state numbers in FARS
#' data, this function draws map of the state overlaid with dots
#' representing accidents.
#'
#' Function throws error if state number is invalid, and does
#' not plot anything if there are no accidents to plot.
#'
#' @param state.num state number (matching with state number in
#' FARS data)
#' @param year year to plot accidents for
#' @param fname_transform Read \link{fars_read_years} documentation to find out.
#'
#' @return draws a plot if inputs are valid, otherwise returns
#' without plotting
#'
#' @importFrom dplyr filter_
#' @import maps
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
#'
#' @export
fars_map_state <- function(state.num, year, fname_transform = identity) {

  filename <- make_filename(year)
  filename <- fname_transform(filename)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- filter_(data, ~ STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
        xlim = range(LONGITUD, na.rm = TRUE))
    points(LONGITUD, LATITUDE, pch = 46)
  })
}
