utils::globalVariables(c(":=", "povrate", "medinc", "ownerocc"))
# This function cleans address text and tries to guess the NYC borough.

#' Clean and fix address text
#'
#' Removes extra spaces and commas from addresses,
#' makes all letters uppercase, and adds a borough name if it can
#' find one in the text.
#'
#' @param df A data frame with an address column.
#' @param address_col The name of the column with addresses.
#' @param borough_col The name of the new borough column.
#' @return A data frame with a cleaned address and borough column.
#' @examples
#' \dontrun{
#' library(dplyr)
#' tibble(address = c("123 Main St, Brooklyn, NY", "55 E 10th St New York NY")) |>
#'   clean_addresses()
#' }
#' @export
clean_addresses <- function(df, address_col = "address", borough_col = "borough") {
  stopifnot(is.data.frame(df))
  address_sym <- rlang::sym(address_col)
  borough_sym <- rlang::sym(borough_col)

  df |>
    dplyr::mutate(
      !!address_sym := stringr::str_squish(!!address_sym),
      !!address_sym := stringr::str_replace_all(!!address_sym, ",", ""),
      !!address_sym := stringr::str_to_upper(!!address_sym),
      !!borough_sym := dplyr::case_when(
        stringr::str_detect(!!address_sym, "\\bBRONX\\b") ~ "Bronx",
        stringr::str_detect(!!address_sym, "\\bBROOKLYN\\b") ~ "Brooklyn",
        stringr::str_detect(!!address_sym, "\\bQUEENS\\b") ~ "Queens",
        stringr::str_detect(!!address_sym, "\\bSTATEN ISLAND\\b|\\bSTATEN\\b") ~ "Staten Island",
        stringr::str_detect(!!address_sym, "\\bNEW YORK\\b|\\bMANHATTAN\\b") ~ "Manhattan",
        TRUE ~ NA_character_
      )
    )
}
