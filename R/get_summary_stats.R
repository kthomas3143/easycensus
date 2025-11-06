#' Summarize numeric columns
#'
#' Gives quick summary statistics (mean) for all numeric variables,
#' grouped by something like borough or year.
#'
#' @param df A data frame to summarize.
#' @param group_var The column name to group by (optional).
#' @return A summarized data frame.
#' @examples
#' \dontrun{
#' df <- data.frame(borough = c("Bronx", "Bronx", "Queens"), income = c(40000, 42000, 50000))
#' get_summary_stats(df, "borough")
#' }
#' @import dplyr
#' @export
get_summary_stats <- function(df, group_var = NULL) {
  if (is.null(group_var)) {
    dplyr::summarise(df, dplyr::across(where(is.numeric), mean, na.rm = TRUE))
  } else {
    df %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  }
}
