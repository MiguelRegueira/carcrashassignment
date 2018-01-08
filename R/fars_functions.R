#' Read csv file
#'
#' Read csv file
#'
#' @param filename Path to the file to be read
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @details This function relays on libraries \code{dplyr::tbl_df}
#'   and \code{readr::read_csv}
#'
#' @return A tible with the data read from the csv file \code{filename}
#'
#' @seealso \code{\link{tbl_df}}
#' @seealso \code{\link{read_csv}}
#'
#' @examples
#' \dontrun{
#' data <- fars_read(filename = "./data/accident_2013.csv.bz2")
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

#' Create data file name including year
#'
#' Create data file name including year
#'
#' @param year Year from which to create the data file name. Year needs
#'   to be a number or a character string representing a number, in
#'   other case a warning will be raised and returned name will include
#'   NA
#'
#' @return A character string of the file name include the year
#'
#' @examples
#' \dontrun{
#' filename <- make_filename(year = 2014)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read years information
#'
#' Read month and year from one or several years passed in \code{year}
#'   input
#'
#' @param years List of years from which to extract data
#'
#' @return A tible with the month and year read from the different csv files
#'   of the different years provided in \code{years}
#'
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_read}}
#'
#' @examples
#' \dontrun{
#' data <- fars_read_years(years = c(2014, 2015))
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

#' Summarize years information
#'
#' Summarize the information of the years  providing the number of
#'   accidents per month and year
#'
#' @inheritParams fars_read_years
#'
#' @return A tible with the number of accidents per month and year
#'   of the different years provided in \code{years} input
#'
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{fars_read_years}}
#'
#' @examples
#' \dontrun{
#' data <- fars_summarize_years(years = c(2014, 2015))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Accidents map
#'
#' Represent on a map the accidents occurs on a state and a year
#'
#' @param state.num Number of the state to extract the information
#' @param year Year from which represent the data
#'
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_read}}
#'
#' @details This function requires libraries \code{dplyr},
#' \code{maps} and \code{graphics}
#'
#' @examples
#' \dontrun{
#' data <- fars_map_state(state.num = 12, year = 2014)
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
