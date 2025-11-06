utils::globalVariables(c(":=", "povrate", "medinc", "ownerocc"))
#' This function cleans address text and tries to guess the NYC borough.

#' Clean and fix address text
#'
#' Cleans messy address text by removing commas, spacing, and fixing
#' abbreviations like "ST" → "STREET". Also detects the borough name
#' and returns it in uppercase.
#'
#' @param df A data.frame (or tibble) containing an address column.
#' @param address_col The name of the address column (string). Default: "address".
#' @param borough_col Optional name for output borough column. Default: "borough".
#'
#' @return A data.frame with a cleaned address column and uppercase borough name (if found).
#' @examples
#' \dontrun{
#' library(tibble)
#' df <- tibble(address = c(
#'   "123 Main St, Brooklyn, NY",
#'   "55 E 10th St NEW YORK NY",
#'   "789 staten island ny"
#' ))
#' clean_addresses(df)
#' }
#' @import dplyr stringr rlang
#' @export
clean_addresses <- function(df, address_col = "address", borough_col = "borough") {
  stopifnot(is.data.frame(df))
  address_sym <- rlang::sym(address_col)
  borough_sym <- rlang::sym(borough_col)

  df |>
    dplyr::mutate(
      # Remove commas, extra spaces, make uppercase
      !!address_sym := stringr::str_squish(!!address_sym),
      !!address_sym := stringr::str_replace_all(!!address_sym, ",", ""),
      !!address_sym := stringr::str_to_upper(!!address_sym),

      # Expand common street abbreviations
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bST\\b", "STREET"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bAVE\\b", "AVENUE"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bRD\\b", "ROAD"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bBLVD\\b", "BOULEVARD"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bDR\\b", "DRIVE"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bLN\\b", "LANE"),

      # Detect and capitalize borough names
      !!borough_sym := dplyr::case_when(
        stringr::str_detect(!!address_sym, "\\bBRONX\\b") ~ "BRONX",
        stringr::str_detect(!!address_sym, "\\bBROOKLYN\\b") ~ "BROOKLYN",
        stringr::str_detect(!!address_sym, "\\bQUEENS\\b") ~ "QUEENS",
        stringr::str_detect(!!address_sym, "\\bSTATEN ISLAND\\b|\\bSTATEN\\b") ~ "STATEN ISLAND",
        stringr::str_detect(!!address_sym, "\\bNEW YORK\\b|\\bMANHATTAN\\b") ~ "MANHATTAN",
        TRUE ~ NA_character_
      )
    )
}
