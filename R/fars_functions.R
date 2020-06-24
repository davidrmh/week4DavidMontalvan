#' Reads the csv file
#' @description
#' `fars_read` reads the csv file with the data from
#'  the US National Highway Traffic Safety Administration's
#'  Fatality Analysis Reporting System.
#'
#'  @param filename Character with the path to the file.
#'
#'  @details If filename is not found then stops.
#'
#'  @return A tibble dataframe
#'
#'  @examples
#'  \dontrun{
#'  fars_read("path/to/file")
#'  }
#'
#'  @import readr
#'  @import dplyr
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates filename
#'
#' @description
#' `make_filename` creates a character vector using
#' a numeric vector with the years.
#'
#' @param year Numeric vector
#'
#' @return
#' A character vector following the pattern
#' "accident_%d.csv.bz2"
#'
#' @section
#' Warning: Non-integer numbers are converted to integers.
#'
#' @examples
#' make_filename(2010)
#' make_filename(c(2010,2020))
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#' Reads files for particular years
#'
#' @description
#' `fars_read_years` takes a numeric vector with the
#' years to read.
#'
#' @param years Numeric (integer) vector.
#'
#' @return
#' List of tibbles, each for one element in `years`.
#'
#' @details
#' For any element in `years` if this element is an
#' invalid year, the function returns `NULL`.
#'
#' @seealso
#' [make_filename][fars_read].
#'
#' @import dplyr

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

#' Creates a summary for a set of years
#'
#' @description
#' This functions takes a numeric vector
#' that contains the years that you want
#' to summarize
#'
#' @param years Numeric (integer) vector with the years
#' you want to summarize.
#'
#' @return
#' tibble with the summarized data
#'
#' @seealso
#' [fars_read_years]
#'
#' @import dplyr
#' @import tidyr
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Function to create a map for a given state
#' and given year.
#'
#' @description
#' This functin takes a state number and a year and
#' creates a map for the subset of data corresponding
#' to those values.
#'
#' @param state.num Integer represeting the state's number.
#' @param year Integer for the year.
#'
#' @return a map plot.
#'
#' @import maps
#' @import dplyr
#' @import graphics

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
