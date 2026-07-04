# =============================================================================
# 02_nndss_analysis.R
#
# Purpose: Analyze real NNDSS weekly surveillance data for Cyclosporiasis,
#          replacing the news-derived estimates used in script 01.
#          This script covers 2026 only - see 03_seasonal_baseline.R for the
#          multi-year (2022-2026) seasonal comparison.
#
# Data source: NNDSS Weekly Data, data.cdc.gov
#              https://data.cdc.gov/NNDSS/NNDSS-Weekly-Data/x9gk-5huc
# File covers: MMWR weeks 1-25, 2026 (through ~late June 2026), filtered from
#              the combined 2022-2026 export (see data/ folder)
# Downloaded/retrieved: 2026-07-03
#
# Run this from an R project opened at the repo root so relative paths work.
# =============================================================================

# ---- 0. Packages -----------------------------------------------------------
# install.packages(c("tidyverse")) # uncomment on first run in Posit Cloud
library(tidyverse)

# ---- 1. Import ---------------------------------------------------------------
# NOTE: as of 2026-07-04 this project consolidated to a single multi-year
# file (data/NNDSS_Weekly_Data_2022-2026_20260703.csv) that supersedes the
# old single-year file. We filter to 2026 here to preserve this script's
# original scope; see 03_seasonal_baseline.R for the full 2022-2026 analysis.

raw <- read_csv("data/NNDSS_Weekly_Data_2022-2026_20260703.csv", show_col_types = FALSE) %>%
  filter(`Current MMWR Year` == 2026)

glimpse(raw)

# Filter to just Cyclosporiasis (this file may contain other conditions
# depending on what you exported from data.cdc.gov - always check)
cyclo <- raw %>% filter(Label == "Cyclosporiasis")

nrow(cyclo)            # sanity check
n_distinct(cyclo$`MMWR WEEK`)     # should be 25 weeks in this snapshot
n_distinct(cyclo$`Reporting Area`) # states + regions + territories + totals

# ---- 2. Clean the flagged numeric columns ------------------------------------
# NNDSS convention, confirmed empirically against the full 2022-2026 dataset
# (not just this single year - see 03_seasonal_baseline.R for the check that
# caught this): a "-" flag does NOT reliably mean "value is zero." It shows
# up on blank cells (where it does mean zero) AND on populated cells with
# real values (where it's just a no-issue annotation and the value stands).
# The correct rule is: if a value is present, trust it; only fall back to
# the flag when the cell is blank.

clean_flagged_col <- function(value_col, flag_col) {
  numeric_val <- suppressWarnings(as.numeric(str_remove_all(as.character(value_col), ",")))
  case_when(
    !is.na(numeric_val)             ~ numeric_val,   # a real value was reported - always trust it
    flag_col == "-"                 ~ 0,              # blank + "-" flag = zero
    flag_col %in% c("N", "NC", "U") ~ NA_real_,        # not reportable / not calculated / unavailable
    TRUE ~ NA_real_
  )
}

cyclo <- cyclo %>%
  mutate(
    current_week_clean   = clean_flagged_col(`Current week`, `Current week, flag`),
    prev52_max_clean     = clean_flagged_col(`Previous 52 week Max`, `Previous 52 weeks Max, flag`),
    cum_ytd_current_clean = clean_flagged_col(`Cumulative YTD Current MMWR Year`, `Cumulative YTD Current MMWR Year, flag`),
    cum_ytd_prior_clean   = clean_flagged_col(`Cumulative YTD Previous MMWR Year`, `Cumulative YTD Previous MMWR Year, flag`)
  )

# Note: verified this fix does not change any number reported earlier in this
# project. The "-flag + populated value" pattern that caused the bug only
# shows up elsewhere in the 2022-2025 data, not in this file's 2026 rows -
# but the corrected function is used here regardless, since relying on that
# coincidence would be fragile if this script is ever re-run on fresh data.

# ---- 3. National epidemic curve, 2026 vs. its own recent baseline -----------
# "Total" = grand total including territories and non-U.S. residents
# "U.S. Residents" = national total for U.S. residents only
# We'll use "Total" for the headline national curve.

national <- cyclo %>%
  filter(`Reporting Area` == "Total") %>%
  arrange(`MMWR WEEK`)

national %>% select(`MMWR WEEK`, current_week_clean, cum_ytd_current_clean, cum_ytd_prior_clean)

# CAUTION: "current_week_clean" undercounts recent weeks due to reporting
# lag (see Section 7 below and README "Critical caveat" section). The bars
# for the last 1-2 weeks in this plot will look smaller than they eventually
# turn out to be once backfilled reports arrive. Treat this as "cases known
# as of this snapshot," not a final epidemic curve.
ggplot(national, aes(x = `MMWR WEEK`)) +
  geom_col(aes(y = current_week_clean), fill = "#4C72B0", width = 0.7) +
  labs(
    title = "U.S. Cyclosporiasis Cases by Week, 2026",
    subtitle = "MMWR weeks 1-25 (through late June 2026). Recent weeks will revise upward - see reporting lag note.",
    x = "MMWR Week", y = "Reported cases (current week)",
    caption = "Source: NNDSS Weekly Data, data.cdc.gov. Data are provisional and subject to revision."
  ) +
  theme_minimal(base_size = 12)

ggsave("output/national_weekly_curve_2026-07-03.png", width = 8, height = 5, dpi = 150)

# ---- 4. Year-over-year comparison: is 2026 actually unusual? ----------------
# NNDSS gives us cumulative YTD for both the current AND previous MMWR year
# for free - this is a much sturdier baseline than a single press-quoted
# "typical annual count."

yoy <- national %>%
  select(`MMWR WEEK`, cum_ytd_current_clean, cum_ytd_prior_clean) %>%
  pivot_longer(cols = starts_with("cum_ytd"), names_to = "year_type", values_to = "cumulative_cases") %>%
  mutate(year_type = if_else(year_type == "cum_ytd_current_clean", "2026", "2025"))

ggplot(yoy, aes(x = `MMWR WEEK`, y = cumulative_cases, color = year_type)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Cumulative U.S. Cyclosporiasis Cases: 2026 vs. 2025",
    subtitle = "Same MMWR weeks (1-25) compared year-over-year",
    x = "MMWR Week", y = "Cumulative cases (year-to-date)",
    color = "Year",
    caption = "Source: NNDSS Weekly Data, data.cdc.gov"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/yoy_comparison_2026-07-03.png", width = 8, height = 5, dpi = 150)

# ---- 5. Michigan: current week vs. its own historical max -------------------
# This is the real test of "is Michigan's spike actually unusual" - comparing
# the current week's count against that SAME jurisdiction's previous 52-week
# max, rather than an eyeballed "typically ~50/year" quote.

michigan <- cyclo %>%
  filter(`Reporting Area` == "Michigan") %>%
  arrange(`MMWR WEEK`)

michigan %>% select(`MMWR WEEK`, current_week_clean, prev52_max_clean, cum_ytd_current_clean)

ggplot(michigan, aes(x = `MMWR WEEK`)) +
  geom_col(aes(y = current_week_clean), fill = "#C44E52", width = 0.7) +
  geom_hline(aes(yintercept = max(prev52_max_clean, na.rm = TRUE)),
             linetype = "dashed", color = "black") +
  annotate("text", x = 3, y = max(michigan$prev52_max_clean, na.rm = TRUE) + 2,
           label = "Previous 52-week max", size = 3, hjust = 0) +
  labs(
    title = "Michigan Cyclosporiasis Cases by Week, 2026",
    subtitle = "Dashed line = highest single-week count in the prior 52 weeks",
    x = "MMWR Week", y = "Reported cases (current week)",
    caption = "Source: NNDSS Weekly Data, data.cdc.gov. Data are provisional."
  ) +
  theme_minimal(base_size = 12)

ggsave("output/michigan_weekly_curve_2026-07-03.png", width = 8, height = 5, dpi = 150)

# Quantify it directly
mi_week25 <- michigan %>% filter(`MMWR WEEK` == 25) %>% pull(current_week_clean)
mi_baseline <- max(michigan$prev52_max_clean, na.rm = TRUE)

cat(sprintf(
  "\nMichigan, week 25: %d cases reported, vs. a previous 52-week max of %d.\n",
  mi_week25, mi_baseline
))
cat(sprintf(
  "That's %.1fx the highest single-week count Michigan had seen in the prior year.\n",
  mi_week25 / mi_baseline
))

# ---- 6. Michigan: does reporting lag affect the spike too? ------------------
# Same check as the national Section 7 below, applied to Michigan
# specifically. Since Michigan's spike is the most recent and most acute
# part of this dataset, it's the part most likely to still be understated.

mi_lag_check <- michigan %>%
  arrange(`MMWR WEEK`) %>%
  mutate(
    running_sum_of_current_week = cumsum(replace_na(current_week_clean, 0)),
    reporting_gap = cum_ytd_current_clean - running_sum_of_current_week
  ) %>%
  select(`MMWR WEEK`, current_week_clean, running_sum_of_current_week,
         cum_ytd_current_clean, reporting_gap)

print(mi_lag_check, n = Inf)

mi_final_gap <- mi_lag_check %>% filter(`MMWR WEEK` == max(`MMWR WEEK`)) %>% pull(reporting_gap)
mi_week25_cum <- mi_lag_check %>% filter(`MMWR WEEK` == 25) %>% pull(cum_ytd_current_clean)

cat(sprintf(
  "\nMichigan's reporting-lag gap by week 25: %d cases (%.0f%% of Michigan's reported cumulative total of %d).\n",
  mi_final_gap, 100 * mi_final_gap / mi_week25_cum, mi_week25_cum
))
cat("Interpretation: Michigan's week-25 case count of 46 is likely an UNDERCOUNT.\n")
cat("As more reports arrive from health providers/labs, week 25's true count will probably rise,\n")
cat("the same way weeks 1-24 were each quietly revised upward in the cumulative column over time.\n")
cat("This means the true scale of Michigan's spike may not be fully visible until a few more weeks pass.\n")

# ---- 7. Reporting lag (national): why "Current week" undercounts recent weeks
# If you sum the Current week column across all weeks, you get a much
# smaller number than the reported Cumulative YTD for the final week. This
# is NOT a data error - it reflects real reporting lag (illness -> doctor
# visit -> lab test -> confirmation -> state health dept -> CDC), which
# takes time and isn't fully caught up for the most recent weeks in any
# provisional dataset. See README "Critical caveat" section for the full
# explanation and CDC's own documentation of this behavior.

lag_check <- national %>%
  arrange(`MMWR WEEK`) %>%
  mutate(
    running_sum_of_current_week = cumsum(current_week_clean),
    reporting_gap = cum_ytd_current_clean - running_sum_of_current_week
  ) %>%
  select(`MMWR WEEK`, current_week_clean, running_sum_of_current_week,
         cum_ytd_current_clean, reporting_gap)

print(lag_check, n = Inf)

ggplot(lag_check, aes(x = `MMWR WEEK`)) +
  geom_line(aes(y = running_sum_of_current_week, color = "Running sum of 'Current week'"), linewidth = 1) +
  geom_line(aes(y = cum_ytd_current_clean, color = "Reported Cumulative YTD"), linewidth = 1) +
  labs(
    title = "Reporting Lag: Why 'Current Week' Undercounts Recent Cases",
    subtitle = "The gap between these two lines is cases that arrived at CDC late and were folded\ninto the cumulative total, but never added back into old 'Current week' values",
    x = "MMWR Week", y = "Cases",
    color = NULL,
    caption = "Source: NNDSS Weekly Data, data.cdc.gov"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("output/reporting_lag_gap_2026-07-03.png", width = 8, height = 5, dpi = 150)

final_gap <- lag_check %>% filter(`MMWR WEEK` == max(`MMWR WEEK`)) %>% pull(reporting_gap)
cat(sprintf(
  "\nBy week 25, the reporting-lag gap has grown to %d cases (%.0f%% of the reported cumulative total).\n",
  final_gap, 100 * final_gap / national$cum_ytd_current_clean[national$`MMWR WEEK` == 25]
))
cat("This means: use Cumulative YTD as the more trustworthy measure of true burden.\n")
cat("Treat the most recent 1-2 weeks of 'Current week' as provisional and likely to rise.\n")

# ---- Next steps (see README Section 5) --------------------------------------
# - Re-pull this dataset in a few weeks; NNDSS revises provisional counts
#   retroactively, so re-check earlier weeks too, not just append new ones
# - Consider per-state small-multiples now that we have real counts for all
#   reporting areas, not just the ones news coverage happened to mention
