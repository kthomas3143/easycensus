utils::globalVariables(c(":=", "povrate", "medinc", "ownerocc"))

#' Rename LTDB variable names
#'
#' Renames hard-to-read variable names from LTDB or ACS data to clearer ones.
#'
#' @param df A data frame with LTDB-style column names.
#' @return A data frame with renamed columns.
#' @examples
#' \dontrun{
#' df <- data.frame(povrate = 10, medinc = 55000)
#' rename_ltdb_vars(df)
#' }
#' @import dplyr
#' @export
rename_ltdb_vars <- function(df) {
  dplyr::rename(df,
                pct_poverty = povrate,
                median_income = medinc,
                pct_owner_occ = ownerocc)
}
