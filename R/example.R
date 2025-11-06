# library(easyCensus)
library(tibble)

# --- Clean addresses ---
df <- tibble(address = c("123 Main St, Brooklyn, NY",
                         "55 E 10th St NEW YORK NY"))
clean_addresses(df)
#> # A tibble: 2 × 2
#>   address               borough
#>   <chr>                 <chr>
#> 1 123 MAIN ST BROOKLYN NY Brooklyn
#> 2 55 E 10TH ST NEW YORK NY Manhattan


# --- Merge census data example ---
df1 <- data.frame(tract_id = 1:2, income_2010 = c(50000, 55000))
df2 <- data.frame(tract_id = 1:2, income_2020 = c(60000, 65000))
merge_census_years(df1, df2, id_col = "tract_id")
#>   tract_id income_2010 income_2020
#> 1        1       50000       60000
#> 2        2       55000       65000


# --- Rename LTDB-style variables ---
df3 <- data.frame(
  tract_id = c(1, 2),
  povrate = c(10, 12),
  medinc = c(55000, 60000),
  ownerocc = c(45, 50)
)
rename_ltdb_vars(df3)
#>   tract_id pct_poverty median_income pct_owner_occ
#> 1        1          10         55000            45
#> 2        2          12         60000            50


# --- Get summary statistics ---
df4 <- data.frame(
  borough = c("Bronx", "Bronx", "Queens", "Queens"),
  income = c(40000, 42000, 50000, 48000),
  poverty_rate = c(10, 12, 7, 9)
)

# Overall mean (no grouping)
get_summary_stats(df4)
#>   income poverty_rate
#> 1  45000          9.5

# Mean by borough
get_summary_stats(df4, "borough")
#>   borough income poverty_rate
#> 1   Bronx  41000          11
#> 2  Queens  49000           8
