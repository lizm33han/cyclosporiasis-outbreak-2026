# =============================================================================
# 02_nndss_analysis.R
#
# Purpose: National picture of the 2026 cyclosporiasis season against 2022-2025,
#          including how much the first-published counts get revised and how
#          much "Current week" undercounts.
#
# Data: NNDSS Weekly Data, data.cdc.gov
#       https://data.cdc.gov/NNDSS/NNDSS-Weekly-Data/x9gk-5huc
#       2022-2025 from the 2026-07-03 export, 2026 from the 2026-10-10 export
#       (see 00_load_and_clean.R)
#
# Run from an R project opened at the repo root so relative paths work.
# =============================================================================

source("scripts/00_load_and_clean.R")

national <- cyclo %>% filter(area == "Total")

# ---- 1. The 2026 season, week by week ---------------------------------------
season_2026 <- national %>%
  filter(year == current_year) %>%
  select(week, current_week, weekly_additions, cum_ytd)

print(season_2026, n = Inf)

peak <- season_2026 %>% slice_max(weekly_additions, n = 1)
cat(sprintf("\nPeak week for reports added: MMWR week %d (%s reports added, %s cumulative)\n",
            peak$week, format(peak$weekly_additions, big.mark = ","),
            format(peak$cum_ytd, big.mark = ",")))

# ---- 2. Weekly reports added vs. the 2022-2025 range -------------------------
# Same measure and same vintage (as first published) for every year.
band <- national %>%
  filter(year %in% baseline_years, week <= latest_week) %>%
  group_by(week) %>%
  summarise(lo = min_na(weekly_additions), hi = max_na(weekly_additions),
            avg = mean(weekly_additions), .groups = "drop")

plot_weekly <- band %>%
  left_join(season_2026 %>% select(week, weekly_additions), by = "week")

p_weekly <- ggplot(plot_weekly, aes(x = week)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.7) +
  geom_line(aes(y = weekly_additions), color = accent_color, linewidth = 0.9) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Reports added to the national total each week, 2026 vs. 2022-2025",
    subtitle = "Grey band = min-max range across 2022-2025. Red line = 2026.\nThis is a reporting curve (when reports arrived), not an onset-date epidemic curve.",
    x = "MMWR week", y = "Reports added that week",
    caption = paste0("Source: NNDSS Weekly Data, data.cdc.gov (export ", export_date, "). Provisional data.")
  ) +
  theme_epi()

ggsave(paste0("output/national_weekly_additions_", export_date, ".png"),
       p_weekly, width = 9, height = 5, dpi = 150)

# ---- 3. Cumulative by year ---------------------------------------------------
cum_by_year <- national %>%
  filter(week <= latest_week) %>%
  mutate(year_lab = factor(year))

year_colors <- c("2022" = "grey75", "2023" = "grey60", "2024" = "grey45",
                 "2025" = "grey25", "2026" = accent_color)

p_cum <- ggplot(cum_by_year, aes(week, cum_ytd, color = year_lab, linewidth = year_lab)) +
  geom_line() +
  scale_color_manual(values = year_colors) +
  scale_linewidth_manual(values = c("2022" = 0.6, "2023" = 0.6, "2024" = 0.6,
                                    "2025" = 0.6, "2026" = 1.1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Cumulative cases by MMWR week, 2022-2026",
    subtitle = "Every year shown as first published, so the years are directly comparable",
    x = "MMWR week", y = "Cumulative cases (year to date)", color = NULL,
    caption = paste0("Source: NNDSS Weekly Data, data.cdc.gov (export ", export_date, ")")
  ) +
  guides(linewidth = "none") +
  theme_epi() +
  theme(legend.position = "bottom")

ggsave(paste0("output/national_cumulative_by_year_", export_date, ".png"),
       p_cum, width = 9, height = 5, dpi = 150)

# ---- 4. 2026 vs. baseline at the latest published week ----------------------
at_latest <- national %>%
  filter(week == latest_week) %>%
  select(year, cum_ytd) %>%
  arrange(year)

print(at_latest)

baseline_avg <- at_latest %>% filter(year %in% baseline_years) %>% summarise(m = mean(cum_ytd)) %>% pull(m)
cum_now      <- at_latest %>% filter(year == current_year) %>% pull(cum_ytd)
cat(sprintf("\nThrough week %d: %s cumulative cases in %d, %.1fx the 2022-2025 average (%s).\n",
            latest_week, format(cum_now, big.mark = ","), current_year,
            cum_now / baseline_avg, format(round(baseline_avg), big.mark = ",")))

# ---- 5. First published vs. settled: how much do counts get revised? --------
# Each row in the weekly tables is frozen as of its publication week. The only
# place revisions appear is the "Previous year" cumulative column in the NEXT
# year's rows, which reflects the settled count. Compare 2025 as first published
# (2025 rows) with 2025 as settled (2026 rows, previous-year column).
vintage <- national %>%
  filter(year == 2025, week <= latest_week) %>%
  select(week, first_published = cum_ytd) %>%
  inner_join(
    national %>% filter(year == current_year) %>%
      select(week, settled = cum_prior_year_settled),
    by = "week"
  ) %>%
  mutate(ratio = settled / first_published)

print(vintage %>% filter(week %in% c(25, 30, 35, latest_week)))

vintage_long <- vintage %>%
  pivot_longer(c(first_published, settled), names_to = "version", values_to = "cum") %>%
  mutate(version = recode(version,
                          first_published = "As first published in 2025",
                          settled = "As settled (reported in 2026)"))

p_vintage <- ggplot(vintage_long, aes(week, cum, color = version)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("As first published in 2025" = "grey55",
                                "As settled (reported in 2026)" = "grey15")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "2025 cumulative cases: first published vs. settled",
    subtitle = "Early-season counts are revised upward the most",
    x = "MMWR week", y = "Cumulative cases", color = NULL,
    caption = paste0("Source: NNDSS Weekly Data, data.cdc.gov (export ", export_date, ")")
  ) +
  theme_epi() +
  theme(legend.position = "bottom")

ggsave(paste0("output/vintage_2025_", export_date, ".png"),
       p_vintage, width = 9, height = 5, dpi = 150)

# ---- 6. Reporting lag: "Current week" vs. the cumulative total ---------------
lag_check <- national %>%
  filter(year == current_year) %>%
  arrange(week) %>%
  mutate(running_sum_current_week = cumsum(replace_na(current_week, 0)),
         reporting_gap = cum_ytd - running_sum_current_week)

last_row <- lag_check %>% filter(week == latest_week)
cat(sprintf("\nThrough week %d, the 'Current week' column sums to %s, only %.0f%% of the %s cumulative total.\n",
            latest_week, format(last_row$running_sum_current_week, big.mark = ","),
            100 * last_row$running_sum_current_week / last_row$cum_ytd,
            format(last_row$cum_ytd, big.mark = ",")))

lag_long <- lag_check %>%
  select(week, running_sum_current_week, cum_ytd) %>%
  pivot_longer(-week, names_to = "series", values_to = "cases") %>%
  mutate(series = recode(series,
                         running_sum_current_week = "Running sum of 'Current week'",
                         cum_ytd = "Reported cumulative total"))

p_lag <- ggplot(lag_long, aes(week, cases, color = series)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Running sum of 'Current week'" = "grey55",
                                "Reported cumulative total" = accent_color)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Why 'Current week' undercounts",
    subtitle = "The gap is reports that arrived late and were folded into the cumulative total only",
    x = "MMWR week", y = "Cases", color = NULL,
    caption = paste0("Source: NNDSS Weekly Data, data.cdc.gov (export ", export_date, ")")
  ) +
  theme_epi() +
  theme(legend.position = "bottom")

ggsave(paste0("output/reporting_lag_", export_date, ".png"),
       p_lag, width = 9, height = 5, dpi = 150)
