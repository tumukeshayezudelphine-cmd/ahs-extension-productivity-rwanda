
# ============================================================
# DATA CLEANING: NISR AHS 2024 - SEASON A
# ============================================================

library(dplyr)

# 1. Open the project folder
setwd("C:/Users/user/Desktop/AHS_Dashboard")

# 2. Read original data
raw_data <- read.csv(
  "seasonA_province.csv",
  stringsAsFactors = FALSE
)

# 3. Keep the original data unchanged
clean_data <- raw_data

# 4. Standardize text columns
text_columns <- c("crop_name")

for (v in text_columns) {
  clean_data[[v]] <- trimws(clean_data[[v]])
}

# 5. Check and convert data types
clean_data$hhid <- as.character(clean_data$hhid)
clean_data$crop_name <- as.character(clean_data$crop_name)

numeric_columns <- c(
  "household_median_kg_ha",
  "plot_season_records",
  "extension_access",
  "district",
  "proportion_organic_fertilizer",
  "proportion_inorganic_fertilizer",
  "proportion_irrigated",
  "clust",
  "weight",
  "province"
)

for (v in numeric_columns) {
  clean_data[[v]] <- suppressWarnings(
    as.numeric(clean_data[[v]])
  )
}

# 6. Check valid extension-access codes
cat("Extension-access values:\n")
print(table(clean_data$extension_access, useNA = "ifany"))

# 7. Check proportions are between 0 and 1
proportion_columns <- c(
  "proportion_organic_fertilizer",
  "proportion_inorganic_fertilizer",
  "proportion_irrigated"
)

for (v in proportion_columns) {
  cat("\nOut-of-range values in", v, ":\n")
  print(
    sum(
      !is.na(clean_data[[v]]) &
        (clean_data[[v]] < 0 | clean_data[[v]] > 1)
    )
  )
}

# 8. Flag unusual productivity values
cat("\nProductivity equal to zero:\n")
print(sum(clean_data$household_median_kg_ha == 0, na.rm = TRUE))

cat("\nProductivity above 100,000 kg/ha:\n")
print(sum(clean_data$household_median_kg_ha > 100000, na.rm = TRUE))

# Show the most extreme observations for investigation
extreme_records <- clean_data %>%
  filter(household_median_kg_ha > 100000) %>%
  select(
    hhid, crop_name, household_median_kg_ha,
    plot_season_records, district, province
  )

print(extreme_records)

# 9. Check survey weights
cat("\nZero or negative weights:\n")
print(sum(clean_data$weight <= 0, na.rm = TRUE))

cat("\nMissing values after standardization:\n")
print(colSums(is.na(clean_data)))

# 10. Save a separate cleaned dataset
write.csv(
  clean_data,
  "seasonA_province_cleaned.csv",
  row.names = FALSE
)

cat("\nCleaning file saved successfully.\n")
cat("Rows in original data:", nrow(raw_data), "\n")
cat("Rows in cleaned data:", nrow(clean_data), "\n")



# Inspect the extreme cassava observation
extreme_records <- clean_data %>%
  filter(hhid == "305771") %>%
  select(
    hhid,
    crop_name,
    household_median_kg_ha,
    plot_season_records,
    extension_access,
    district,
    province,
    weight
  )

print(extreme_records)

# Check productivity distribution by crop
clean_data %>%
  group_by(crop_name) %>%
  summarise(
    observations = n(),
    minimum = min(household_median_kg_ha, na.rm = TRUE),
    median = median(household_median_kg_ha, na.rm = TRUE),
    mean = mean(household_median_kg_ha, na.rm = TRUE),
    maximum = max(household_median_kg_ha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Inspect the nine zero-productivity observations
clean_data %>%
  filter(household_median_kg_ha == 0) %>%
  select(
    hhid, crop_name, household_median_kg_ha,
    extension_access, district, province
  ) %>%
  print()


library(dplyr)

# Cassava results using all observations
cassava_all <- clean_data %>%
  filter(crop_name == "Cassava") %>%
  summarise(
    observations = n(),
    median_productivity = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    mean_productivity = mean(
      household_median_kg_ha,
      na.rm = TRUE
    )
  )

# Sensitivity check excluding the extreme record
cassava_sensitivity <- clean_data %>%
  filter(
    crop_name == "Cassava",
    hhid != "305771"
  ) %>%
  summarise(
    observations = n(),
    median_productivity = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    mean_productivity = mean(
      household_median_kg_ha,
      na.rm = TRUE
    )
  )

cat("Cassava: all observations\n")
print(cassava_all)

cat("\nCassava: excluding household 305771\n")
print(cassava_sensitivity)


# Inspect the extreme cassava observation
clean_data %>%
  filter(hhid == "305771") %>%
  select(
    hhid,
    crop_name,
    household_median_kg_ha,
    plot_season_records,
    extension_access,
    district,
    province,
    weight
  )

library(dplyr)

# Cassava productivity by extension access
cassava_comparison <- clean_data %>%
  filter(crop_name == "Cassava") %>%
  group_by(extension_access) %>%
  summarise(
    observations = n(),
    mean_productivity = mean(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    median_productivity = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(cassava_comparison)


library(dplyr)

# Cassava productivity by extension access
cassava_comparison <- clean_data %>%
  filter(crop_name == "Cassava") %>%
  group_by(extension_access) %>%
  summarise(
    observations = n(),
    mean_productivity = mean(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    median_productivity = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(cassava_comparison)


library(dplyr)

cassava_comparison_without_extreme <- clean_data %>%
  filter(
    crop_name == "Cassava",
    hhid != "305771"
  ) %>%
  group_by(extension_access) %>%
  summarise(
    observations = n(),
    mean_productivity = mean(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    median_productivity = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(cassava_comparison_without_extreme)

