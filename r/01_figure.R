# ================================
# Trend plot: maternal mortality (increase from 2000 -> 2017)
# ================================

# ---- Packages ----
pkgs <- c("readr","dplyr","ggplot2","stringr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
library(readr); library(dplyr); library(ggplot2); library(stringr)

# ---- 1) Get data frame ----
# If an object named `conflict` already exists, use it; else read from CSV.
if (exists("conflict") && is.data.frame(conflict)) {
  df_raw <- conflict
} else {
  if (!file.exists("data/analytical/finaldataset.csv")) stop("Couldn't find 'finaldataset.csv'.")
  df_raw <- readr::read_csv("data/analytical/finaldataset.csv", show_col_types = FALSE)
}

# ---- 2) Normalize column names and ensure required columns are present ----
# Try to find the needed columns even if names differ (spaces/underscores/case).
pick_col <- function(cands, data) {
  nm <- names(data)
  # direct match
  got <- intersect(cands, nm)
  if (length(got) > 0) return(got[1])
  # loose match: lowercase & replace spaces/underscores
  norm <- function(x) gsub("[ _]+", "", tolower(x))
  nm_norm <- norm(nm)
  idx <- match(norm(cands), nm_norm)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) stop(sprintf("Missing column; tried: %s", paste(cands, collapse=", ")))
  nm[idx[1]]
}

col_iso   <- pick_col(c("iso","ISO","iso3","iso_code"), df_raw)
col_year  <- pick_col(c("year","Year"), df_raw)
col_mm    <- pick_col(c("maternal_mortality","Maternal mortality","Maternal_mortality"), df_raw)

df <- df_raw %>%
  dplyr::select(
    iso   = all_of(col_iso),
    year  = all_of(col_year),
    maternal_mortality = all_of(col_mm)
  )

# ---- 3) Restrict to 2000–2017 and build diff_matmor (relative to 2000) ----
df_0017 <- df %>%
  dplyr::filter(year >= 2000, year < 2018) %>%
  arrange(iso, year)

# Ensure baseline is truly year==2000 per iso (not just the first row)
base_2000 <- df_0017 %>%
  dplyr::filter(year == 2000, !is.na(maternal_mortality)) %>%
  dplyr::select(iso, mm_2000 = maternal_mortality)

df_diff <- df_0017 %>%
  dplyr::left_join(base_2000, by = "iso") %>%
  dplyr::mutate(diff_matmor = maternal_mortality - mm_2000)

# ---- 4) Identify countries with an INCREASE from 2000 to 2017 ----
mm_2017 <- df_0017 %>%
  dplyr::filter(year == 2017) %>%
  dplyr::select(iso, mm_2017 = maternal_mortality)

chg <- base_2000 %>%
  dplyr::inner_join(mm_2017, by = "iso") %>%          # keep only countries with both years present
  dplyr::mutate(delta_17 = mm_2017 - mm_2000) %>%
  dplyr::filter(!is.na(delta_17) & delta_17 > 0)

isos_increase <- chg$iso

df_plot <- df_diff %>%
  dplyr::filter(iso %in% isos_increase)

# ---- 5) Plot: trend lines for countries with an increase (unique color per ISO) ----
p <- ggplot(df_plot, aes(x = year, y = maternal_mortality, group = iso, color = iso)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  labs(
    title = "Maternal Mortality Trends (Countries with Increase from 2000 to 2017)",
    subtitle = "Only countries where maternal mortality in 2017 > 2000 are shown",
    x = "Year",
    y = "Maternal mortality (per 100,000 live births)",
    color = "Country (ISO)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p)

# ---- 6) Save figure ----
ggsave("maternal_mortality_increase_2000_2017.png", p, width = 10, height = 6, dpi = 300)


