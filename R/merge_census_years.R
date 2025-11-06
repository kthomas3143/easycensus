#' Merge Census Data Across Years
#'
#' Combines two or more census datasets by a common ID column (like "tract_id").
#' This helps you compare variables across different years.
#'
#' @param ... Two or more data frames to merge.
#' @param id_col The column name that identifies each area (default is "tract_id").
#' @return A merged data frame containing all years.
#' @examples
#' \dontrun{
#' df1 <- data.frame(tract_id = 1:2, income_2010 = c(50000, 55000))
#' df2 <- data.frame(tract_id = 1:2, income_2020 = c(60000, 65000))
#' merge_census_years(df1, df2, id_col = "tract_id")
#' }
#' @import dplyr purrr
#' @export
merge_census_years <- function(..., id_col = "tract_id") {
  dfs <- list(...)
  stopifnot(length(dfs) >= 2)
  purrr::reduce(dfs, dplyr::full_join, by = id_col)
}
