##### Histology distal-vs-control analysis
##### Updated June 29, 2026 for AJB minor revisions
##### Reviewer 1 request: report mean ± SE for cell number and cumulative cell wall thickness

library(tidyverse)

# Read data and keep ONLY distal samples for this analysis
df <- read_csv("Histology 2026.csv") %>%
  mutate(
    position = if_else(position == "outside", "distal", position)
  ) %>%
  filter(position == "distal") %>%
  mutate(
    SampleID = row_number()
  )

# Sample region: tissue distal to extending split
df_sample <- df %>%
  transmute(
    SampleID,
    Region = "distal",
    CellNumber = cell.number,
    CellWallThicknessSum = cell.wall,
    Thickness = thickness
  )

# Paired adjacent control region
df_control <- df %>%
  transmute(
    SampleID,
    Region = "control",
    CellNumber = control.cell.number,
    CellWallThicknessSum = control.cell.wall,
    Thickness = control.thickness
  )

# Combine into tidy dataset
df_tidy <- bind_rows(df_sample, df_control) %>%
  mutate(
    Region = factor(Region, levels = c("control", "distal"))
  )

# Function for mean ± SE
mean_se <- function(x) {
  tibble(
    mean = mean(x, na.rm = TRUE),
    se = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))),
    n = sum(!is.na(x))
  )
}

# Cell number paired t-test

paired_cell <- df_tidy %>%
  select(SampleID, Region, CellNumber) %>%
  pivot_wider(names_from = Region, values_from = CellNumber)

# Difference is control - distal
cell_ttest <- t.test(paired_cell$control, paired_cell$distal, paired = TRUE)
cell_ttest

# Cumulative cell wall thickness paired t-test

paired_wall <- df_tidy %>%
  select(SampleID, Region, CellWallThicknessSum) %>%
  pivot_wider(names_from = Region, values_from = CellWallThicknessSum)

# Difference is control - distal
wall_ttest <- t.test(paired_wall$control, paired_wall$distal, paired = TRUE)
wall_ttest

# Blade thickness paired t-test 

paired_thickness <- df_tidy %>%
  select(SampleID, Region, Thickness) %>%
  pivot_wider(names_from = Region, values_from = Thickness)

# Difference is control - distal
thickness_ttest <- t.test(paired_thickness$control, paired_thickness$distal, paired = TRUE)
thickness_ttest

# Summary stats for manuscript 

summary_stats <- bind_rows(
  mean_se(paired_thickness$distal) %>%
    mutate(Metric = "Blade thickness", Region = "distal"),

  mean_se(paired_thickness$control) %>%
    mutate(Metric = "Blade thickness", Region = "control"),

  mean_se(paired_cell$distal) %>%
    mutate(Metric = "Cell number", Region = "distal"),

  mean_se(paired_cell$control) %>%
    mutate(Metric = "Cell number", Region = "control"),

  mean_se(paired_wall$distal) %>%
    mutate(Metric = "Cumulative cell wall thickness", Region = "distal"),

  mean_se(paired_wall$control) %>%
    mutate(Metric = "Cumulative cell wall thickness", Region = "control")
) %>%
  select(Metric, Region, mean, se, n)

summary_stats

# Percent reduction in blade thickness
mean_control_thickness <- mean(paired_thickness$control, na.rm = TRUE)
mean_distal_thickness  <- mean(paired_thickness$distal, na.rm = TRUE)

percent_reduction <- 100 * 
  (mean_control_thickness - mean_distal_thickness) / mean_control_thickness

percent_reduction