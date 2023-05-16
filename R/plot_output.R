#' @importFrom magrittr %>%
NULL
#' Plot comparison between two eplusout columns
#'
#' @param df.before baseline case data frame, read in from eplusout.csv.
#'     `Date/Time` data frame converted with something like this
#'     read.idfEnergyPlus::convert.timestamp.eplusout(target.year = 2023)
#' @param df.after feature case data frame, read in from eplusout.csv. Need to
#'     have `Date/Time` converted similarly as df.before
#' @param col column name to plot
#' @param ylabel yaxis label, something like "Temperature (C)"
#' @param month filter to a certain month
#' @param day filter to a certain day. The combination of the two facilitates
#'     plotting of a typical summer day or winter day
#' @return a ggplot2 object of line plot
compare.before.after.col <- function(df.before, df.after, col, ylabel, label.before = "before",
                                     label.after = "after", month=NULL, day=NULL) {
    df.before <- df.before %>%
        dplyr::select(`Date/Time`, col) %>%
        dplyr::mutate(status = label.before) %>%
        {.}
    print(df.before %>% head())
    df.after <- df.after %>%
        dplyr::select(`Date/Time`, col) %>%
        dplyr::mutate(status = label.after) %>%
        {.}
    print(df.after %>% head())
    to.plot <- df.before %>%
        dplyr::bind_rows(df.after) %>%
        read.idfEnergyPlus::convert.timestamp.eplusout(target.year=2023) %>%
        {.}
    if (!is.null(month)) {
        to.plot <- to.plot %>%
            dplyr::mutate(mon = lubridate::month(`Date/Time`)) %>%
            dplyr::filter(mon %in% month)
    }
    if (!is.null(day)) {
        to.plot <- to.plot %>%
            dplyr::mutate(day.of.mon = lubridate::day(`Date/Time`)) %>%
            dplyr::filter(day.of.mon %in% day)
    }
    to.plot %>%
        ggplot2::ggplot(ggplot2::aes_string(x = "`Date/Time`", y = sprintf("`%s`", col), color = "status")) +
        ggplot2::geom_line() +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(col) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme()
}
