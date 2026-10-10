# =============================================================================
# 03_seasonal_baseline.R
#
# Purpose: Michigan against its own 2022-2025 history, and Michigan's share of
#          the national total over the 2026 season.
#
# Data: NNDSS Weekly Data, data.cdc.gov (see 00_load_and_clean.R)
# Run from an R project opened at the repo root so relative paths work.
# =============================================================================

source("scripts/00_load_and_clean.R")

michigan <- cyclo %>% filter(area == "Michigan")
national <- cyclo %>% filter(area == "Total")

# ---- 1. Michigan week by week, 2026 -----------------------------------------
mi_2026 <- michigan %>%
  filter(year == current_year) %>%
  select(week, current_week, weekly_additions, cum_ytd)

print(mi_2026 %>% filter(week >= 20), n = Inf)

# ---- 2. Weekly reports added vs. the 2022-2025 range -------------------------
mi_band <- michigan %>%
  filter(year %in% baseline_years, week <= latest_week) %>%
  group_by(week) %>%
  summarise(lo = min_na(weekly_additions), hi = max_na(weekly_additions), .groups = "drop") %>%
  left_join(mi_2026 %>% select(week, weekly_additions), by = "week")

p_mi <- ggplot(mi_band, aes(x = week)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.7) +
  geom_line(aes(y = weekly_additions), color = accent_color, linewidth = 0.9) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Michigan: reports added each week, 2026 vs. 2022-2025",
    subtitle = "Grey band = min-max range across 2022-2025. Red line = 2026.",
    x = "MMWR week", y = "Reports added that week",
    caption = paste0("Source: NNDSS Weekly Data, data.cdc.gov (export ", export_date, "). Provisional data.")
  ) +
  theme_epi()

ggsave(paste0("output/michigan_weekly_additions_", export_date, ".png"),
       p_mi, width = 9, height = 5, dpi = 150)

# ---- 3. The week-25 comparison from the July report, and where it stands now -
wk25 <- michigan %>% filter(week == 25) %>% select(year, current_week, cum_ytd) %>% arrange(year)
print(wk25)

mi_latest <- michigan %>% filter(week == latest_week) %>% select(year, cum_ytd) %>% arrange(year)
print(mi_latest)

# ---- 4. Michigan's share of the national cumulative total -------------------
share <- mi_2026 %>%
  select(week, mi_cum = cum_ytd) %>%
  inner_join(national %>% filter(year == current_year) %>% select(week, nat_cum = cum_ytd),
             by = "week") %>%
  mutate(share = mi_cum / nat_cum)

cat(sprintf("\nMichigan accounts for %.0f%% of the national cumulative total through week %d.\n",
            100 * share$share[share$week == latest_week], latest_week))

p_share <- ggplot(share %>% filter(week >= 20), aes(week, share)) +
  geom_line(color = accent_color, linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Michigan's share of the national cumulative total, 2026",
    x = "MMWR week", y = "Share of national cumulative cases",
    caption = paste0("Source: NNDSS Weekly Data, data.cdc.gov (export ", export_date, ")")
  ) +
  theme_epi()

ggsave(paste0("output/michigan_share_", export_date, ".png"),
       p_share, width = 9, height = 4.5, dpi = 150)

# ---- 5. The latest week ------------------------------------------------------
# Michigan's weekly additions jump again in the last published week. This file
# cannot say whether that is a late batch of reports or a new increase, so
# treat it as something to re-check when later weeks are published.
print(mi_2026 %>% filter(week >= latest_week - 4))
