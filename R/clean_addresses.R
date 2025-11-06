utils::globalVariables(c(":=", "povrate", "medinc", "ownerocc"))
#' This function cleans address text and tries to guess the NYC borough.

#' Clean and fix address text
#'
#' This function cleans up messy address text and adds a borough column
#' if the address mentions one of NYC’s five boroughs.
#' It also expands common abbreviations like "St" → "STREET".
#'
#' @param df A data.frame (or tibble) containing an address column.
#' @param address_col The name of the address column (string). Default: "address".
#' @param borough_col Optional name for output borough column. Default: "borough".
#'
#' @return A data.frame with cleaned address and a borough column (if detected).
#' @examples
#' \dontrun{
#' library(dplyr)
#' tibble(address = c("123 Main St, Brooklyn, NY", "55 E 10th St NEW YORK NY")) |>
#'   clean_addresses()
#' }
#' @import dplyr stringr rlang
#' @export
clean_addresses <- function(df, address_col = "address", borough_col = "borough") {
  stopifnot(is.data.frame(df))
  address_sym <- rlang::sym(address_col)
  borough_sym <- rlang::sym(borough_col)

  df |>
    dplyr::mutate(
      # Clean whitespace and punctuation
      !!address_sym := stringr::str_squish(!!address_sym),
      !!address_sym := stringr::str_replace_all(!!address_sym, ",", ""),
      !!address_sym := stringr::str_to_upper(!!address_sym),

      # Replace ' ST ' or end-of-line ' ST' with ' STREET'
      !!address_sym := stringr::str_replace_all(
        !!address_sym,
        "\\bST\\b", "STREET"
      ),

      # Detect boroughs
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
