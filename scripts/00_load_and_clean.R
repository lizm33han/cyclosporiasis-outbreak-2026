# =============================================================================
# 00_load_and_clean.R
#
# Shared loader for every script and the report. Run (or source) this first.
#
# What it does:
#   1. Reads two NNDSS exports of the Cyclosporiasis weekly tables:
#        - baseline: 2022-2025, from the 2026-07-03 export
#        - current:  2026 through the latest published week, from the 2026-10-10 export
#   2. Cleans the flagged numeric columns (see the data-cleaning notes below)
#   3. Builds one tidy data frame, `cyclo`, plus `latest_week`
#
# Why two files? The 2026-10-10 export only contains 2026. NNDSS rows are
# snapshots of what was published in each week and are never rewritten (checked:
# all 1,750 rows for 2026 weeks 1-25 are identical between the July and October
# exports), so the 2022-2025 rows in the July export are still the right
# baseline. Revisions only show up in the "Previous year" cumulative column,
# which we keep as `cum_prior_year_settled` (see 02_nndss_analysis.R).
#
# Run from an R project opened at the repo root so relative paths work.
# =============================================================================

library(tidyverse)

# ---- Parameters --------------------------------------------------------------
baseline_file  <- "data/NNDSS_Weekly_Data_2022-2026_20260703.csv"
current_file   <- "data/NNDSS_Weekly_Data_2026_20261010.csv"
baseline_years <- 2022:2025
current_year   <- 2026
export_date    <- "2026-10-10"

# ---- Data-cleaning notes (all four found while working with these files) ----
# 1. Reporting-area names change case across years (2022-2024 ALL CAPS,
#    2025-2026 Title Case). We normalize with str_to_title() before any
#    filtering or joining, otherwise exact-match filters silently drop years.
# 2. Counts over 999 are stored as text with a thousands separator ("1,171").
#    as.numeric("1,171") silently returns NA, so we strip commas first.
# 3. The "-" flag does NOT reliably mean zero. It appears on blank cells (zero)
#    and on populated cells (an annotation only). Trust a populated value;
#    only read the flag when the cell is blank. "N", "NC", "U" mean not
#    reportable / not calculated / unavailable, so they become NA.
# 4. Aggregate and territory labels do not unify with case normalization
#    ("US RESIDENTS" becomes "Us Residents", not "U.S. Residents"). We only use
#    "Total" and the 50 states + DC, which do unify.

clean_flagged_col <- function(value_col, flag_col) {
  numeric_val <- suppressWarnings(as.numeric(str_remove_all(as.character(value_col), ",")))
  case_when(
    !is.na(numeric_val)             ~ numeric_val,   # a real value was reported: always trust it
    flag_col == "-"                 ~ 0,              # blank + "-" flag = zero
    flag_col %in% c("N", "NC", "U") ~ NA_real_,       # not reportable / not calculated / unavailable
    TRUE ~ NA_real_
  )
}

read_nndss <- function(path) {
  # Read everything as text so nothing is silently coerced, then clean explicitly
  read_csv(path, col_types = cols(.default = col_character()), progress = FALSE) %>%
    transmute(
      area  = str_to_title(`Reporting Area`),
      year  = as.integer(`Current MMWR Year`),
      week  = as.integer(`MMWR WEEK`),
      label = Label,
      current_week = clean_flagged_col(`Current week`, `Current week, flag`),
      prev52_max   = clean_flagged_col(`Previous 52 week Max`, `Previous 52 weeks Max, flag`),
      cum_ytd      = clean_flagged_col(`Cumulative YTD Current MMWR Year`,
                                       `Cumulative YTD Current MMWR Year, flag`),
      cum_prior_year_settled = clean_flagged_col(`Cumulative YTD Previous MMWR Year`,
                                                 `Cumulative YTD Previous MMWR Year, flag`)
    )
}

# ---- Load and combine --------------------------------------------------------
baseline <- read_nndss(baseline_file) %>% filter(year %in% baseline_years)
current  <- read_nndss(current_file)  %>% filter(year == current_year)

# Week 53 exists only in some years (2025 here) and has no equivalent in the
# others, so it is excluded from cross-year comparisons.
cyclo <- bind_rows(baseline, current) %>%
  filter(label == "Cyclosporiasis", week <= 52)

# Latest week published for the current year (national total row has a value)
latest_week <- current %>%
  filter(area == "Total", !is.na(cum_ytd)) %>%
  summarise(w = max(week)) %>%
  pull(w)

# ---- Weekly additions --------------------------------------------------------
# "Current week" only captures about 4 in 10 of the cases that end up in the
# cumulative total (see the reporting-lag section of the report), so for weekly
# trends we use the change in the cumulative column: the number of reports added
# to the year-to-date total that week, including late reports for earlier weeks.
# This is a REPORTING curve, not an epidemic curve by onset date.
cyclo <- cyclo %>%
  arrange(area, year, week) %>%
  group_by(area, year) %>%
  mutate(weekly_additions = cum_ytd - lag(cum_ytd, default = 0)) %>%
  ungroup()

# ---- The 50 states + DC (NYC reports separately and is not included) --------
us_states <- c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
  "Delaware","District Of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois",
  "Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts",
  "Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
  "New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota",
  "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
  "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
  "West Virginia","Wisconsin","Wyoming"
)

# ---- Missing-value-safe min/max ----------------------------------------------
# Some states were not reportable in some years (e.g. Kentucky in 2022), so those
# cells are NA. These return NA instead of -Inf/Inf (and a warning) when every
# value in a group is NA.
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
max_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

# ---- Shared plot style -------------------------------------------------------
# One accent color for the year being evaluated, greys for the baseline years.
accent_color <- "#C44E52"
theme_epi <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey90"),
          plot.title.position = "plot")
}

# ---- Quick checks (printed when run interactively) ---------------------------
message("Years loaded: ", paste(sort(unique(cyclo$year)), collapse = ", "))
message("Latest published week in ", current_year, ": ", latest_week)
message("Areas: ", n_distinct(cyclo$area))
