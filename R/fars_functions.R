## Coursera - Building R Packages - May 2021
## Documenting Code
## M.A. Sondergard

#' fars_read
#'
#' This is a function that reads in This function reads data from .csv file
#' from the US ational Highway Traffic Safety Administration's Fatality Analysis
#' Reporting System (FARS), which is a nationwide census that provides yearly data
#' regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @param filename The name of a file containing the annual data for a specific
#' year of the survey. The file is assumed to be in comma separated values (csv)
#' format.
#'
#' @return This function returns the data contained in the file as a tibble.  A
#' tibble is a special version of a data frame designed to work with the "tidyverse"
#' set of R packages.  If the file cannot be found, an error message "file ...
#' does not exist" is generated.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom tibble as_tibble
#' @importFrom readr read_csv
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        tibble::as_tibble(data)
  }

#' make_filename
#'
#' This is a function that takes a given year as a string or integer (e.g. 2015)
#' and inserts into the name of a file that saves Fatality Analysis Reporting System
#' (FARS) information.
#'
#' @param year The year for which the data was collected.
#'
#' @return The function returns a character vector (the filename) containing a
#' combination of text and the year passed to the function.
#'
#' @examples
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years
#'
#' This function reads in a vector of the years to be examined in the Fatality
#' Analysis Reporting System (FARS) data and tests whether the information for the
#' specific year chosen is available.  If not, the function informs the user that
#' the year is invalid. Ancillary function used by \code{fars_summarize_years}.
#'
#' @param years A vector of years.
#'
#' @return A data set covering the indicated years, loaded into the R environment.
#'
#' @examples
#' fars_read_years(c(2013, 2014, 2015))
#'
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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

#' fars_summarize_years
#'
#' This function summarizes accident data, by year and month, for the years input
#' by the user.
#'
#' @param years A vector with a list of years to be summarized.
#'
#' @return
#' A data set summarizing accidents by year and month, with other relevant
#' information.
#'
#' @examples
#' fars_summarize_years(c(2014, 2015))
#'
#' @import magrittr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
  }

#' fars_map_state
#'
#' Displays a plot with a state map including the accidents location by year
#' If the state.num is invalid, the function shows an error.
#'
#' @param state.num An integer the provides the number of the state whose data
#' is to be retrieved.  See (link) for a listing of state numbers.
#'
#' @param year An integer which contains the year of interest.
#'
#' @return A plot with accident locations plotted on a map of the selected state.
#'
#' @examples
#' fars_map_state(25, 2013)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
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
