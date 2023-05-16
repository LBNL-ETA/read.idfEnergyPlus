#' @importFrom magrittr %>%
NULL
#' Convert timestamp
#'
#' @param df eplusout.csv directly
#' @param year year of the simulation result. EnergyPlus output doesn't have year in the timestamp
#' @return a data frame with `Date/Time` converted to POSIXct
convert.timestamp.eplusout <- function(df, target.year) {
    df %>%
        tidyr::separate(`Date/Time`, into = c("day", "hour"), sep = "  ") %>%
        dplyr::mutate(hour = as.numeric(gsub(":00:00", "", hour))) %>%
        dplyr::mutate(hour = hour - 1) %>%
        dplyr::mutate(`Date/Time` = as.POSIXct(sprintf("%04d/%s %02d:00:00", target.year, day, hour), format="%Y/%m/%d %H:%M:%S", tz = "GMT")) %>%
        dplyr::select(-hour, -day) %>%
        dplyr::select(`Date/Time`, everything()) %>%
        {.}
}
