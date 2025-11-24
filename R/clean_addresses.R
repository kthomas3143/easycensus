utils::globalVariables(c(":=", "povrate", "medinc", "ownerocc"))

#' Clean and fix address text
#'
#' Cleans messy address text by removing commas, spacing, fixing
#' abbreviations, expanding single-letter directions (N → NORTH),
#' removing ordinal number suffixes (38TH → 38), and detecting boroughs.
#'
#' @param df A data.frame or tibble containing an address column.
#' @param address_col The name of the address column (string). Default: "address".
#' @param borough_col Optional name for output borough column. Default: "borough".
#'
#' @return A cleaned data.frame with standardized address text and a borough column.
#'
#' @examples
#' \dontrun{
#' library(tibble)
#' df <- tibble(address = c(
#'   "123 Main St, Brooklyn, NY",
#'   "55 E 10th St NEW YORK NY",
#'   "37 N 38th St Queens NY"
#' ))
#' clean_addresses(df)
#' }
#'
#' @import dplyr stringr rlang
#' @export
clean_addresses <- function(df, address_col = "address", borough_col = "borough") {
  stopifnot(is.data.frame(df))

  address_sym <- rlang::sym(address_col)
  borough_sym <- rlang::sym(borough_col)

  df |>
    dplyr::mutate(
      # Basic cleaning: remove commas, extra spaces, uppercase
      !!address_sym := stringr::str_squish(!!address_sym),
      !!address_sym := stringr::str_replace_all(!!address_sym, ",", ""),
      !!address_sym := stringr::str_to_upper(!!address_sym),

      !!address_sym := stringr::str_replace_all(
        !!address_sym,
        "(?<=\\d)(ST|ND|RD|TH)(?=\\b)",
        ""
      ),

      # Expand common street abbreviations
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bST\\b", "STREET"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bAVE\\b", "AVENUE"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bRD\\b", "ROAD"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bBLVD\\b", "BOULEVARD"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bDR\\b", "DRIVE"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bLN\\b", "LANE"),

      # Expand standalone direction letters (N, S, E, W)
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bN\\b", "NORTH"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bS\\b", "SOUTH"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bE\\b", "EAST"),
      !!address_sym := stringr::str_replace_all(!!address_sym, "\\bW\\b", "WEST"),

      # Detect boroughs in uppercase
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
