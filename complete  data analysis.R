
############################################################
# NISR BIG DATA HACKATHON
# Project: Agricultural Extension Access and Crop Productivity
# Dataset: AHS 2024, Season A
# Language: R
# Workflow: Import -> Understand -> Clean -> Visualise
#           -> Analyse -> Sensitivity Analysis -> Dashboard
############################################################


############################################################
# 0. SET PROJECT FOLDER AND LOAD PACKAGES
############################################################

# Set the folder containing your CSV file.
setwd("C:/Users/user/Desktop/AHS_Dashboard")

# Load the packages used throughout the project.
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(broom)
library(scales)

# Optional: set a consistent theme for all ggplot charts.
theme_set(theme_minimal(base_size = 12))

# Check that the original data file exists.
if (!file.exists("seasonA_province.csv")) {
  stop("File not found. Check your project folder and filename.")
}


############################################################
# 1. LOAD THE RAW DATA
############################################################

# Preserve an untouched copy of the imported dataset.
raw_data <- read.csv(
  "seasonA_province.csv",
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

# Create a separate working copy for cleaning.
clean_data <- raw_data

cat("\n========== DATA IMPORTED ==========\n")
cat("Number of rows:", nrow(raw_data), "\n")
cat("Number of columns:", ncol(raw_data), "\n")


############################################################
# 2. UNDERSTAND THE DATA
############################################################

# Display the first six rows.
cat("\n========== FIRST SIX ROWS ==========\n")
print(head(raw_data))

# Display column names.
cat("\n========== VARIABLE NAMES ==========\n")
print(names(raw_data))

# Display data types and structure.
cat("\n========== DATA STRUCTURE ==========\n")
str(raw_data)

# Display summary statistics.
cat("\n========== SUMMARY STATISTICS ==========\n")
print(summary(raw_data))

# Display the number of unique households.
cat("\n========== UNIQUE HOUSEHOLDS ==========\n")
if ("hhid" %in% names(raw_data)) {
  cat("Unique households:", length(unique(raw_data$hhid)), "\n")
}

# Check duplicate complete rows.
cat("\n========== DUPLICATE ROWS ==========\n")
cat("Duplicate rows:", sum(duplicated(raw_data)), "\n")

# Count missing values in every variable.
cat("\n========== MISSING VALUES ==========\n")
missing_summary <- data.frame(
  variable = names(raw_data),
  missing_values = sapply(raw_data, function(x) sum(is.na(x)))
)
print(missing_summary)

# Inspect categories of key variables.
if ("crop_name" %in% names(raw_data)) {
  cat("\n========== CROP COUNTS ==========\n")
  print(table(raw_data$crop_name, useNA = "ifany"))
}

if ("extension_access" %in% names(raw_data)) {
  cat("\n========== EXTENSION ACCESS COUNTS ==========\n")
  print(table(raw_data$extension_access, useNA = "ifany"))
}

if ("province" %in% names(raw_data)) {
  cat("\n========== PROVINCE COUNTS ==========\n")
  print(table(raw_data$province, useNA = "ifany"))
}


############################################################
# 3. DATA CLEANING
############################################################

# Confirm that all expected columns are present.
required_columns <- c(
  "hhid",
  "crop_name",
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

missing_columns <- setdiff(required_columns, names(clean_data))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "These required columns are missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Remove accidental spaces from text fields.
clean_data$hhid <- trimws(as.character(clean_data$hhid))
clean_data$crop_name <- trimws(as.character(clean_data$crop_name))

# Convert numeric variables carefully.
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

# Keep a record of values that cannot be converted to numbers.
conversion_report <- data.frame(
  variable = character(),
  newly_missing = integer()
)

for (v in numeric_columns) {
  old_values <- clean_data[[v]]
  new_values <- suppressWarnings(as.numeric(old_values))
  
  newly_missing <- sum(
    !is.na(old_values) &
      trimws(as.character(old_values)) != "" &
      is.na(new_values)
  )
  
  conversion_report <- rbind(
    conversion_report,
    data.frame(
      variable = v,
      newly_missing = newly_missing
    )
  )
  
  clean_data[[v]] <- new_values
}

cat("\n========== NUMERIC CONVERSION REPORT ==========\n")
print(conversion_report)

# Check for missing values after conversion.
cat("\n========== MISSING VALUES AFTER CONVERSION ==========\n")
print(
  data.frame(
    variable = names(clean_data),
    missing_values = sapply(clean_data, function(x) sum(is.na(x)))
  )
)

# Check for duplicate rows.
cat("\nDuplicate rows before removal:",
    sum(duplicated(clean_data)), "\n")

# Do not automatically remove rows.
# Duplicate records should first be investigated.
# No rows are removed in this script.

# Check the validity of extension access codes.
cat("\n========== EXTENSION ACCESS VALUES ==========\n")
print(table(clean_data$extension_access, useNA = "ifany"))

invalid_extension <- !is.na(clean_data$extension_access) &
  !(clean_data$extension_access %in% c(0, 1))

cat("Invalid extension access records:",
    sum(invalid_extension), "\n")

# Check fertilizer and irrigation proportions.
proportion_columns <- c(
  "proportion_organic_fertilizer",
  "proportion_inorganic_fertilizer",
  "proportion_irrigated"
)

proportion_report <- data.frame(
  variable = proportion_columns,
  below_zero = sapply(proportion_columns, function(v) {
    sum(clean_data[[v]] < 0, na.rm = TRUE)
  }),
  above_one = sapply(proportion_columns, function(v) {
    sum(clean_data[[v]] > 1, na.rm = TRUE)
  })
)

cat("\n========== PROPORTION RANGE CHECK ==========\n")
print(proportion_report)

# Check productivity values.
cat("\n========== PRODUCTIVITY QUALITY CHECK ==========\n")
cat(
  "Negative productivity:",
  sum(clean_data$household_median_kg_ha < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Zero productivity:",
  sum(clean_data$household_median_kg_ha == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Productivity above 100,000 kg/ha:",
  sum(clean_data$household_median_kg_ha > 100000, na.rm = TRUE),
  "\n"
)

# Identify extreme values using the 1.5*IQR rule.
# This flags observations for investigation; it does not prove errors.
productivity_values <- clean_data$household_median_kg_ha
productivity_values <- productivity_values[
  is.finite(productivity_values)
]

q1 <- quantile(productivity_values, 0.25, na.rm = TRUE)
q3 <- quantile(productivity_values, 0.75, na.rm = TRUE)
iqr_value <- IQR(productivity_values, na.rm = TRUE)

lower_fence <- q1 - 1.5 * iqr_value
upper_fence <- q3 + 1.5 * iqr_value

outlier_records <- clean_data %>%
  filter(
    !is.na(household_median_kg_ha),
    household_median_kg_ha < lower_fence |
      household_median_kg_ha > upper_fence
  )

cat("\n========== FLAGGED PRODUCTIVITY VALUES ==========\n")
print(outlier_records)

# Specifically inspect the previously identified extreme record.
cat("\n========== HOUSEHOLD 305771 ==========\n")
print(
  clean_data %>%
    filter(hhid == "305771") %>%
    select(all_of(required_columns))
)

# Create an analysis-ready copy.
# Do not impute values or delete extreme observations automatically.
analysis_data <- clean_data %>%
  filter(
    !is.na(crop_name),
    crop_name != "",
    !is.na(extension_access),
    extension_access %in% c(0, 1),
    !is.na(household_median_kg_ha),
    household_median_kg_ha >= 0
  )

# Save the cleaned data for future use.
write.csv(
  clean_data,
  "seasonA_province_cleaned.csv",
  row.names = FALSE
)

cat("\nCleaned dataset saved.\n")
cat("Rows in raw data:", nrow(raw_data), "\n")
cat("Rows in analysis data:", nrow(analysis_data), "\n")


############################################################
# 4. CREATE HELPER VARIABLES FOR ANALYSIS
############################################################

# Label the binary extension access variable.
analysis_data <- analysis_data %>%
  mutate(
    extension_group = factor(
      extension_access,
      levels = c(0, 1),
      labels = c("No access", "Access")
    ),
    province_group = factor(province),
    district_group = factor(district)
  )

# Use log(1 + productivity) for a less skewed outcome.
# This does not replace the original productivity variable.
analysis_data <- analysis_data %>%
  mutate(
    log_productivity = log1p(household_median_kg_ha)
  )


############################################################
# 5. DESCRIPTIVE ANALYSIS
############################################################

cat("\n========== PRODUCTIVITY SUMMARY ==========\n")
productivity_summary <- analysis_data %>%
  group_by(crop_name) %>%
  summarise(
    observations = n(),
    mean_productivity = mean(household_median_kg_ha),
    median_productivity = median(household_median_kg_ha),
    standard_deviation = sd(household_median_kg_ha),
    minimum = min(household_median_kg_ha),
    maximum = max(household_median_kg_ha),
    .groups = "drop"
  )

print(productivity_summary)

cat("\n========== PRODUCTIVITY BY EXTENSION ACCESS ==========\n")
extension_summary <- analysis_data %>%
  group_by(crop_name, extension_group) %>%
  summarise(
    observations = n(),
    mean_productivity = mean(household_median_kg_ha),
    median_productivity = median(household_median_kg_ha),
    standard_deviation = sd(household_median_kg_ha),
    .groups = "drop"
  )

print(extension_summary)

# Export summary tables.
write.csv(productivity_summary, "productivity_summary.csv", row.names = FALSE)
write.csv(extension_summary, "extension_summary.csv", row.names = FALSE)


############################################################
# 6. DATA VISUALISATION
############################################################

# Each plot is printed so it appears in the RStudio Plots pane.
# The plots are also saved as PNG files.

# Plot 1: Number of records by crop.
p1 <- ggplot(analysis_data, aes(x = crop_name, fill = crop_name)) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Number of Crop Records",
    x = "Crop",
    y = "Number of records"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p1)
ggsave("plot_01_crop_counts.png", p1, width = 8, height = 5)

# Plot 2: Extension access distribution.
p2 <- ggplot(analysis_data, aes(x = extension_group, fill = extension_group)) +
  geom_bar(show.legend = FALSE) +
  labs(
    title = "Agricultural Extension Access",
    x = "Extension access",
    y = "Number of records"
  )

print(p2)
ggsave("plot_02_extension_access.png", p2, width = 7, height = 5)

# Plot 3: Productivity distribution.
p3 <- ggplot(
  analysis_data,
  aes(x = household_median_kg_ha)
) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Crop Productivity",
    x = "Productivity (kg/ha)",
    y = "Number of records"
  )

print(p3)
ggsave("plot_03_productivity_histogram.png", p3, width = 8, height = 5)

# Plot 4: Log-transformed productivity distribution.
p4 <- ggplot(
  analysis_data,
  aes(x = log_productivity)
) +
  geom_histogram(bins = 35, fill = "darkseagreen", color = "white") +
  labs(
    title = "Distribution of Log-Transformed Productivity",
    x = "log(1 + productivity)",
    y = "Number of records"
  )

print(p4)
ggsave("plot_04_log_productivity.png", p4, width = 8, height = 5)

# Plot 5: Productivity by crop.
p5 <- ggplot(
  analysis_data,
  aes(x = crop_name, y = household_median_kg_ha)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  labs(
    title = "Productivity Distribution by Crop",
    x = "Crop",
    y = "Productivity (kg/ha)"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p5)
ggsave("plot_05_productivity_by_crop.png", p5, width = 9, height = 5)

# Plot 6: Productivity by extension access.
p6 <- ggplot(
  analysis_data,
  aes(x = extension_group, y = household_median_kg_ha)
) +
  geom_boxplot(outlier.alpha = 0.35) +
  facet_wrap(~crop_name, scales = "free_y") +
  labs(
    title = "Productivity by Extension Access and Crop",
    x = "Extension access",
    y = "Productivity (kg/ha)"
  )

print(p6)
ggsave("plot_06_extension_boxplots.png", p6, width = 10, height = 6)

# Plot 7: Median productivity by crop and extension access.
p7 <- ggplot(
  extension_summary,
  aes(x = crop_name, y = median_productivity, fill = extension_group)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Median Productivity by Crop and Extension Access",
    x = "Crop",
    y = "Median productivity (kg/ha)",
    fill = "Extension access"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p7)
ggsave("plot_07_median_by_access.png", p7, width = 9, height = 5)

# Plot 8: Mean productivity by crop and extension access.
p8 <- ggplot(
  extension_summary,
  aes(x = crop_name, y = mean_productivity, fill = extension_group)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Mean Productivity by Crop and Extension Access",
    x = "Crop",
    y = "Mean productivity (kg/ha)",
    fill = "Extension access"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p8)
ggsave("plot_08_mean_by_access.png", p8, width = 9, height = 5)

# Plot 9: Productivity by province.
p9 <- ggplot(
  analysis_data,
  aes(x = province_group, y = household_median_kg_ha)
) +
  geom_boxplot() +
  labs(
    title = "Productivity by Province Code",
    x = "Province code",
    y = "Productivity (kg/ha)"
  )

print(p9)
ggsave("plot_09_province_boxplot.png", p9, width = 8, height = 5)

# Plot 10: Extension access counts by province.
p10 <- ggplot(
  analysis_data,
  aes(x = province_group, fill = extension_group)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Extension Access Composition by Province",
    x = "Province code",
    y = "Percentage of records",
    fill = "Extension access"
  )

print(p10)
ggsave("plot_10_access_by_province.png", p10, width = 8, height = 5)

# Plot 11: Organic fertilizer proportion and productivity.
p11 <- ggplot(
  analysis_data,
  aes(
    x = proportion_organic_fertilizer,
    y = household_median_kg_ha
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Organic Fertilizer Proportion and Productivity",
    x = "Organic fertilizer proportion",
    y = "Productivity (kg/ha)"
  )

print(p11)
ggsave("plot_11_organic_fertilizer.png", p11, width = 8, height = 5)

# Plot 12: Irrigation proportion and productivity.
p12 <- ggplot(
  analysis_data,
  aes(
    x = proportion_irrigated,
    y = household_median_kg_ha
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Irrigation Proportion and Productivity",
    x = "Irrigated proportion",
    y = "Productivity (kg/ha)"
  )

print(p12)
ggsave("plot_12_irrigation.png", p12, width = 8, height = 5)

# Plot 13: Inorganic fertilizer proportion and productivity.
p13 <- ggplot(
  analysis_data,
  aes(
    x = proportion_inorganic_fertilizer,
    y = household_median_kg_ha
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Inorganic Fertilizer Proportion and Productivity",
    x = "Inorganic fertilizer proportion",
    y = "Productivity (kg/ha)"
  )

print(p13)
ggsave("plot_13_inorganic_fertilizer.png", p13, width = 8, height = 5)


############################################################
# 7. STATISTICAL TESTS
############################################################

# Wilcoxon rank-sum test:
# Compare productivity between extension groups for each crop.
# This is an unweighted comparison and does not adjust for confounders.

cat("\n========== WILCOXON TESTS BY CROP ==========\n")

wilcox_results <- lapply(
  unique(analysis_data$crop_name),
  function(crop) {
    
    d <- analysis_data %>%
      filter(crop_name == crop)
    
    # A test needs observations in both groups.
    if (
      length(unique(d$extension_access)) < 2 ||
      sum(d$extension_access == 0) == 0 ||
      sum(d$extension_access == 1) == 0
    ) {
      return(data.frame(
        crop_name = crop,
        statistic = NA_real_,
        p_value = NA_real_
      ))
    }
    
    test <- wilcox.test(
      household_median_kg_ha ~ extension_access,
      data = d,
      exact = FALSE
    )
    
    data.frame(
      crop_name = crop,
      statistic = unname(test$statistic),
      p_value = test$p.value
    )
  }
)

wilcox_results <- bind_rows(wilcox_results)
print(wilcox_results)

write.csv(wilcox_results, "wilcoxon_results.csv", row.names = FALSE)


############################################################
# 8. REGRESSION MODELS
############################################################

# These are preliminary, unweighted models.
# They examine associations, not causal effects.
# Province is included as a categorical control.
# Interpret log-model coefficients on the log scale.

# Model 1: Productivity in kg/ha.
model_levels <- lm(
  household_median_kg_ha ~
    extension_access +
    proportion_organic_fertilizer +
    proportion_inorganic_fertilizer +
    proportion_irrigated +
    factor(province),
  data = analysis_data
)

cat("\n========== MODEL 1: PRODUCTIVITY LEVELS ==========\n")
print(summary(model_levels))

# Model 2: Log-transformed productivity.
model_log <- lm(
  log1p(household_median_kg_ha) ~
    extension_access +
    proportion_organic_fertilizer +
    proportion_inorganic_fertilizer +
    proportion_irrigated +
    factor(province),
  data = analysis_data
)

cat("\n========== MODEL 2: LOG PRODUCTIVITY ==========\n")
print(summary(model_log))

# Model 3: Crop-specific log-productivity models.
# Separate models help avoid assuming the same relationship for all crops.
fit_crop_model <- function(crop) {
  
  d <- analysis_data %>%
    filter(crop_name == crop)
  
  # Skip if the sample is too small or extension access does not vary.
  if (
    nrow(d) < 20 ||
    length(unique(d$extension_access)) < 2
  ) {
    return(NULL)
  }
  
  lm(
    log1p(household_median_kg_ha) ~
      extension_access +
      proportion_organic_fertilizer +
      proportion_inorganic_fertilizer +
      proportion_irrigated +
      factor(province),
    data = d
  )
}

crop_models <- lapply(
  unique(analysis_data$crop_name),
  fit_crop_model
)

names(crop_models) <- unique(analysis_data$crop_name)

cat("\n========== CROP-SPECIFIC LOG MODELS ==========\n")

for (crop in names(crop_models)) {
  if (!is.null(crop_models[[crop]])) {
    cat("\nCrop:", crop, "\n")
    print(summary(crop_models[[crop]]))
  }
}

# Export regression coefficient tables.
model_levels_table <- tidy(model_levels, conf.int = TRUE)
model_log_table <- tidy(model_log, conf.int = TRUE)

write.csv(model_levels_table, "model_levels_coefficients.csv", row.names = FALSE)
write.csv(model_log_table, "model_log_coefficients.csv", row.names = FALSE)

# Export crop-specific coefficients.
crop_model_tables <- lapply(names(crop_models), function(crop) {
  
  model <- crop_models[[crop]]
  
  if (is.null(model)) {
    return(NULL)
  }
  
  result <- tidy(model, conf.int = TRUE)
  result$crop_name <- crop
  result
})

crop_model_tables <- bind_rows(crop_model_tables)

write.csv(
  crop_model_tables,
  "crop_model_coefficients.csv",
  row.names = FALSE
)


############################################################
# 9. SENSITIVITY ANALYSIS: EXTREME CASSAVA OBSERVATION
############################################################

# Compare cassava results with and without household 305771.
# The observation is excluded only for this sensitivity check.
# It remains in clean_data and the saved cleaned dataset.

cassava_all <- analysis_data %>%
  filter(crop_name == "Cassava") %>%
  summarise(
    observations = n(),
    mean_productivity = mean(household_median_kg_ha),
    median_productivity = median(household_median_kg_ha)
  )

cassava_without_extreme <- analysis_data %>%
  filter(
    crop_name == "Cassava",
    hhid != "305771"
  ) %>%
  summarise(
    observations = n(),
    mean_productivity = mean(household_median_kg_ha),
    median_productivity = median(household_median_kg_ha)
  )

cat("\n========== CASSAVA SENSITIVITY RESULTS ==========\n")
cat("\nIncluding all observations:\n")
print(cassava_all)

cat("\nExcluding household 305771:\n")
print(cassava_without_extreme)

# Compare extension access groups after excluding the extreme observation.
cassava_sensitivity_groups <- analysis_data %>%
  filter(
    crop_name == "Cassava",
    hhid != "305771"
  ) %>%
  group_by(extension_group) %>%
  summarise(
    observations = n(),
    mean_productivity = mean(household_median_kg_ha),
    median_productivity = median(household_median_kg_ha),
    .groups = "drop"
  )

print(cassava_sensitivity_groups)

write.csv(
  cassava_sensitivity_groups,
  "cassava_sensitivity_by_access.csv",
  row.names = FALSE
)


############################################################
# 10. SAVE THE MAIN ANALYSIS DATASET
############################################################

write.csv(
  analysis_data,
  "analysis_ready_data.csv",
  row.names = FALSE
)

cat("\n========== ANALYSIS FINISHED ==========\n")
cat("Output files were saved in:", getwd(), "\n")


############################################################
# 11. INTERACTIVE SHINY DASHBOARD
############################################################

# The dashboard allows users to filter by crop, extension access,
# and province, then inspect KPIs, charts, and the filtered data.

ui <- fluidPage(
  
  titlePanel(
    "Agricultural Extension Access and Crop Productivity"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "crop",
        "Select crop:",
        choices = c("All crops", sort(unique(analysis_data$crop_name))),
        selected = "All crops"
      ),
      
      selectInput(
        "extension",
        "Extension access:",
        choices = c("Both groups", "No access", "Access"),
        selected = "Both groups"
      ),
      
      selectInput(
        "province",
        "Province code:",
        choices = c(
          "All provinces",
          sort(unique(as.character(analysis_data$province)))
        ),
        selected = "All provinces"
      ),
      
      checkboxInput(
        "exclude_extreme",
        "Exclude household 305771 for sensitivity analysis",
        value = FALSE
      ),
      
      helpText(
        "The dashboard is exploratory. Regression outputs above are preliminary."
      )
    ),
    
    mainPanel(
      
      h3("Key indicators"),
      
      fluidRow(
        column(4, wellPanel(
          h4("Records"),
          textOutput("n_records")
        )),
        column(4, wellPanel(
          h4("Mean productivity"),
          textOutput("mean_productivity")
        )),
        column(4, wellPanel(
          h4("Median productivity"),
          textOutput("median_productivity")
        ))
      ),
      
      h3("Productivity distribution"),
      plotOutput("productivity_plot", height = "400px"),
      
      h3("Mean productivity by extension access"),
      plotOutput("access_plot", height = "350px"),
      
      h3("Records by province"),
      plotOutput("province_plot", height = "350px"),
      
      h3("Filtered data"),
      tableOutput("data_table")
    )
  )
)


server <- function(input, output, session) {
  
  # Apply the selected filters to the data.
  filtered_data <- reactive({
    
    d <- analysis_data
    
    if (input$crop != "All crops") {
      d <- d %>% filter(crop_name == input$crop)
    }
    
    if (input$extension == "No access") {
      d <- d %>% filter(extension_access == 0)
    }
    
    if (input$extension == "Access") {
      d <- d %>% filter(extension_access == 1)
    }
    
    if (input$province != "All provinces") {
      d <- d %>%
        filter(as.character(province) == input$province)
    }
    
    if (input$exclude_extreme) {
      d <- d %>% filter(hhid != "305771")
    }
    
    d
  })
  
  # KPI 1: number of records.
  output$n_records <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  # KPI 2: mean productivity.
  output$mean_productivity <- renderText({
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return("No records")
    }
    
    paste0(
      format(
        round(mean(d$household_median_kg_ha, na.rm = TRUE), 2),
        big.mark = ","
      ),
      " kg/ha"
    )
  })
  
  # KPI 3: median productivity.
  output$median_productivity <- renderText({
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return("No records")
    }
    
    paste0(
      format(
        round(median(d$household_median_kg_ha, na.rm = TRUE), 2),
        big.mark = ","
      ),
      " kg/ha"
    )
  })
  
  # Interactive boxplot.
  output$productivity_plot <- renderPlot({
    
    d <- filtered_data()
    
    validate(
      need(nrow(d) > 0, "No records match the selected filters.")
    )
    
    ggplot(
      d,
      aes(
        x = extension_group,
        y = household_median_kg_ha,
        fill = extension_group
      )
    ) +
      geom_boxplot(outlier.alpha = 0.4) +
      facet_wrap(~crop_name, scales = "free_y") +
      labs(
        x = "Extension access",
        y = "Productivity (kg/ha)",
        title = "Productivity distribution by crop and extension access"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Compare group means in the filtered data.
  output$access_plot <- renderPlot({
    
    d <- filtered_data()
    
    validate(
      need(nrow(d) > 0, "No records match the selected filters.")
    )
    
    means <- d %>%
      group_by(crop_name, extension_group) %>%
      summarise(
        mean_productivity = mean(
          household_median_kg_ha,
          na.rm = TRUE
        ),
        .groups = "drop"
      )
    
    ggplot(
      means,
      aes(
        x = crop_name,
        y = mean_productivity,
        fill = extension_group
      )
    ) +
      geom_col(position = "dodge") +
      labs(
        x = "Crop",
        y = "Mean productivity (kg/ha)",
        fill = "Extension access"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  # Show the number of records for each province.
  output$province_plot <- renderPlot({
    
    d <- filtered_data()
    
    validate(
      need(nrow(d) > 0, "No records match the selected filters.")
    )
    
    province_counts <- d %>%
      count(province_group)
    
    ggplot(
      province_counts,
      aes(
        x = province_group,
        y = n
      )
    ) +
      geom_col() +
      labs(
        x = "Province code",
        y = "Number of records",
        title = "Records by province"
      ) +
      theme_minimal()
  })
  
  # Display a limited number of rows to keep the dashboard responsive.
  output$data_table <- renderTable({
    
    filtered_data() %>%
      select(
        hhid,
        crop_name,
        household_median_kg_ha,
        extension_access,
        district,
        province
      ) %>%
      head(100)
    
  }, striped = TRUE, bordered = TRUE)
}


# Launch the Shiny application.
shinyApp(ui = ui, server = server)

