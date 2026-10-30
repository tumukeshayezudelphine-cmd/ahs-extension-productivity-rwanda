# Install these packages once
install.packages(c("haven", "dplyr", "purrr"))

# Load packages
library(haven)
library(dplyr)
library(purrr)


# Select the downloaded Microdata.zip file
zip_path <- file.choose("C:/Users/user/Downloads/Microdata.zip")  # Choose the Microdata.zip file


# Extract the files beside the ZIP file
data_dir <- dirname(zip_path)
unzip(zip_path, exdir = data_dir)


# Find all Stata datasets
files <- list.files(
  file.path(data_dir, "Microdata"),
  pattern = "\\.dta$",
  full.names = TRUE
)


# Display dataset names
basename(files)


# Select the four modules most relevant to our project
key_files <- files[grepl(
  "Section1_|Section3_4_|Section6_|Section8_",
  basename(files)
)]


# Locate the four modules needed for our preliminary project
key_files <- files[grepl(
  "Section1_|Section3_4_|Section6_|Section8_",
  basename(files)
)]


# Check the selected files
basename(key_files)


# Read each selected Stata file into R
survey <- setNames(
  map(key_files, read_dta),
  tools::file_path_sans_ext(basename(key_files))
)

# Confirm that the datasets loaded
length(survey)



# Count rows and columns in each module
map(survey, ~ c(
  observations = nrow(.x),
  variables = ncol(.x)
))



# Inspect variables before deciding how to analyze or merge data
walk2(names(survey), survey, function(nm, dat) {
  cat("\n---", nm, "---\n")
  print(names(dat))
})



# Identify the crop and extension datasets
crop <- survey[[2]]
extension <- survey[[3]]

# Check whether household IDs are unique
cat("Crop records:", nrow(crop), "\n")
cat("Unique crop households:", n_distinct(crop$hhid), "\n")

cat("Extension records:", nrow(extension), "\n")
cat("Unique extension households:", n_distinct(extension$hhid), "\n")

# Count repeated household IDs
crop %>% count(hhid) %>% filter(n > 1) %>% head(10)
extension %>% count(hhid) %>% filter(n > 1) %>% head(10)



# Display question labels for extension services
for (v in names(extension)) {
  label <- attr(extension[[v]], "label")
  if (!is.null(label)) {
    cat(v, ":", label, "\n")
  }
}

# Display labels for the crop module
for (v in names(crop)) {
  label <- attr(crop[[v]], "label")
  if (!is.null(label)) {
    cat(v, ":", label, "\n")
  }
}



# Summarize the available survey weights
summary(crop$weight)
summary(extension$weight)

# Check for missing or non-positive weights
sum(is.na(crop$weight))
sum(!is.na(crop$weight) & crop$weight <= 0)

sum(is.na(extension$weight))
sum(!is.na(extension$weight) & extension$weight <= 0)



# Count records per household in each module
crop %>%
  count(hhid) %>%
  summarise(
    households = n(),
    repeated_households = sum(n > 1),
    maximum_records = max(n)
  )

extension %>%
  count(hhid) %>%
  summarise(
    households = n(),
    repeated_households = sum(n > 1),
    maximum_records = max(n)
  )

# Inspect extension-service response values
table(extension$s6q2, useNA = "ifany")
table(extension$s6q3, useNA = "ifany")

# Check the survey's season categories
table(crop$Season, useNA = "ifany")



# Inspect the first crop entry's harvest variable
attr(crop$s3_q20_1, "label")
attr(crop$s3_q20_1, "labels")
summary(crop$s3_q20_1)

# Inspect the plot-area variable
attr(crop$s2q2, "label")
summary(crop$s2q2)



# Step 1: Check how extension variables are stored
class(extension$s6q1)
class(extension$s6q2)
class(extension$s6q3)

# Step 2: Read the question labels
attr(extension$s6q1, "label")
attr(extension$s6q2, "label")
attr(extension$s6q3, "label")

# Step 3: Inspect extension-service codes
attr(extension$s6q1, "labels")
head(extension[, c("hhid", "s6q1", "s6q2", "s6q3")], 15)

# Step 4: Check all crop harvest quantity variables
harvest_vars <- grep("^s3_q20_[1-7]$", names(crop), value = TRUE)

# Summarize each harvest variable
crop %>%
  summarise(across(all_of(harvest_vars),
                   list(missing = ~sum(is.na(.)),
                        median = ~median(., na.rm = TRUE),
                        maximum = ~max(., na.rm = TRUE))))

# Step 5: Check the season labels
attr(crop$Season, "labels")



library(dplyr)

# Summarise extension services for each household
extension_household <- extension %>%
  group_by(hhid) %>%
  summarise(
    extension_access = as.integer(
      any(!is.na(s6q1) & as.numeric(s6q1) != 26)
    ),
    number_of_service_types = n_distinct(
      as.numeric(s6q1[!is.na(s6q1) & as.numeric(s6q1) != 26])
    ),
    .groups = "drop"
  )

# Count households with and without recorded extension access
table(extension_household$extension_access)

# Check how many households have each number of service types
table(extension_household$number_of_service_types)

# Check whether any service codes are missing
sum(is.na(extension$s6q1))



# Check the labels for crop names and harvest quantities
for (v in c("s3_q4_1", "s3_q20_1",
            "s3_q4_2", "s3_q20_2")) {
  cat("\nVariable:", v, "\n")
  print(attr(crop[[v]], "label"))
}

# Display complete summaries of all harvest variables
harvest_vars <- grep(
  "^s3_q20_[1-7]$",
  names(crop),
  value = TRUE
)

harvest_summary <- crop %>%
  summarise(
    across(
      all_of(harvest_vars),
      list(
        missing = ~sum(is.na(.)),
        median = ~median(., na.rm = TRUE),
        maximum = ~max(., na.rm = TRUE)
      )
    )
  )

print(harvest_summary, width = Inf)



# Inspect the crop names and quantities for the first 15 records
crop %>%
  select(
    hhid, Season, s2q1, s2q2,
    s3_q4_1, s3_q20_1,
    s3_q4_2, s3_q20_2,
    s3_q4_3, s3_q20_3
  ) %>%
  head(15)

# Check whether the same household has records in different seasons
crop %>%
  distinct(hhid, Season) %>%
  count(Season, name = "unique_households")

# Inspect the member codes for seasonal extension services
head(
  extension %>%
    select(hhid, s6q1, s6q2, s6q3),
  20
)

# Check whether households have different extension records
extension %>%
  count(hhid, name = "records_per_household") %>%
  summarise(
    minimum_records = min(records_per_household),
    median_records = median(records_per_household),
    maximum_records = max(records_per_household)
  )


library(dplyr)
library(tidyr)

# Add a unique identifier to every original record
crop_long <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id, hhid, Season, s2q1, s2q2, weight,
    matches("^s3_q4_[1-7]$"),
    matches("^s3_q20_[1-7]$")
  ) %>%
  pivot_longer(
    cols = matches("^s3_q4_[1-7]$"),
    names_to = "entry",
    names_pattern = "s3_q4_([1-7])",
    values_to = "crop_code"
  ) %>%
  left_join(
    crop %>%
      mutate(record_id = row_number()) %>%
      select(record_id, matches("^s3_q20_[1-7]$")) %>%
      pivot_longer(
        cols = matches("^s3_q20_[1-7]$"),
        names_to = "entry",
        names_pattern = "s3_q20_([1-7])",
        values_to = "harvest_kg"
      ),
    by = c("record_id", "entry")
  )

# Keep entries with a recorded crop code
crop_long %>%
  filter(!is.na(crop_code)) %>%
  select(hhid, Season, s2q1, s2q2, crop_code, harvest_kg) %>%
  head(20)


dim(crop_long)
names(crop_long)



library(dplyr)

# Keep only the variables needed for crop analysis
crop_long_clean <- crop_long %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    weight, entry, crop_code, harvest_kg
  ) %>%
  filter(!is.na(crop_code))

# Check the cleaned dataset
dim(crop_long_clean)

# Confirm the first records
crop_long_clean %>%
  select(hhid, Season, s2q1, crop_code, harvest_kg) %>%
  head(15)



# Summarise harvest quantities
crop_long_clean %>%
  summarise(
    crop_entries = n(),
    missing_harvest = sum(is.na(harvest_kg)),
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    negative_harvest = sum(harvest_kg < 0, na.rm = TRUE),
    median_harvest = median(harvest_kg, na.rm = TRUE),
    maximum_harvest = max(harvest_kg, na.rm = TRUE)
  )

# Inspect the 10 largest recorded harvest quantities
crop_long_clean %>%
  filter(!is.na(harvest_kg)) %>%
  arrange(desc(harvest_kg)) %>%
  select(hhid, Season, s2q1, crop_code, s2q2, harvest_kg) %>%
  head(10)



# Select the crop production dataset
crop <- survey[["AHS2024_Section3_4_CROP GROWN, SEEDS AND PRODUCTION & AGRICULTURAL INPUTS AND PRACTICES"]]

# Inspect the important variables
names(crop)[grepl(
  "hhid|clust|province|district|Season|s2q1|s2q2|s3_q4_|s3_q20_|weight",
  names(crop)
)]

# Check the variable labels
library(haven)

sapply(
  crop[c("hhid", "Season", "s2q1", "s2q2",
         "s3_q4_1", "s3_q20_1", "weight")],
  function(x) {
    label <- attr(x, "label")
    if (is.null(label)) "" else label
  }
)

# View the first records
head(crop[, c(
  "hhid", "Season", "s2q1", "s2q2",
  "s3_q4_1", "s3_q20_1", "weight"
)], 10)



# Check the quality of recorded harvest quantities
crop_long_clean %>%
  summarise(
    total_crop_entries = n(),
    missing_harvest = sum(is.na(harvest_kg)),
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    negative_harvest = sum(harvest_kg < 0, na.rm = TRUE),
    median_harvest_kg = median(harvest_kg, na.rm = TRUE),
    maximum_harvest_kg = max(harvest_kg, na.rm = TRUE)
  )

# Inspect the ten largest recorded harvest quantities
crop_long_clean %>%
  filter(!is.na(harvest_kg)) %>%
  arrange(desc(harvest_kg)) %>%
  select(hhid, Season, s2q1, crop_code, s2q2, harvest_kg) %>%
  head(10)



# Examine the original records for the five largest harvests
crop %>%
  filter(hhid %in% c(305771, 305669, 312531, 314651, 313546)) %>%
  select(
    hhid, Season, s2q1, s2q2,
    s3_q4_1, s3_q20_1,
    s3_q4_2, s3_q20_2,
    s3_q4_3, s3_q20_3
  ) %>%
  arrange(desc(s3_q20_1))



# Summarise harvest data by crop code
crop_long_clean %>%
  group_by(crop_code) %>%
  summarise(
    crop_entries = n(),
    missing_harvest = sum(is.na(harvest_kg)),
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    median_harvest_kg = median(harvest_kg, na.rm = TRUE),
    maximum_harvest_kg = max(harvest_kg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(missing_harvest)) %>%
  print(n = 20)



# Identify the ten largest recorded harvests
largest_harvests <- crop_long_clean %>%
  filter(!is.na(harvest_kg)) %>%
  arrange(desc(harvest_kg)) %>%
  select(record_id, hhid, Season, s2q1, entry,
         crop_code, s2q2, harvest_kg) %>%
  slice_head(n = 10)

largest_harvests



# Recover the original survey rows
original_rows <- crop %>%
  mutate(record_id = row_number()) %>%
  semi_join(largest_harvests, by = "record_id")

# Display the original rows for checking
original_rows %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    starts_with("s3_q4_"),
    starts_with("s3_q20_")
  )



# Summarise harvest quantities by crop
crop_summary <- crop_long_clean %>%
  group_by(crop_code) %>%
  summarise(
    crop_entries = n(),
    missing_harvest = sum(is.na(harvest_kg)),
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    median_harvest_kg = if (all(is.na(harvest_kg))) {
      NA_real_
    } else {
      median(harvest_kg, na.rm = TRUE)
    },
    maximum_harvest_kg = if (all(is.na(harvest_kg))) {
      NA_real_
    } else {
      max(harvest_kg, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Display the 20 crops with the most missing harvest values
crop_summary %>%
  arrange(desc(missing_harvest)) %>%
  print(n = 20)



# Calculate provisional yield for each crop entry
yield_check <- crop_long_clean %>%
  filter(
    !is.na(harvest_kg),
    !is.na(s2q2),
    s2q2 > 0,
    harvest_kg >= 0
  ) %>%
  mutate(
    provisional_yield_kg_m2 = harvest_kg / s2q2
  ) %>%
  arrange(desc(provisional_yield_kg_m2))

# Inspect the 15 highest provisional yields
yield_check %>%
  select(
    hhid, Season, s2q1, crop_code,
    s2q2, harvest_kg, provisional_yield_kg_m2
  ) %>%
  slice_head(n = 15)



# Inspect labels for crop details and related variables
variables_to_check <- c(
  "s3_q4_1a", "s3_q4_1b",
  "s2q23_1", "s3_q20_1",
  "s2q2", "weight"
)

sapply(crop[variables_to_check], function(x) {
  label <- attr(x, "label")
  if (is.null(label)) "" else label
})



# Find the 20 smallest positive plot areas
crop %>%
  filter(!is.na(s2q2), s2q2 > 0) %>%
  arrange(s2q2) %>%
  select(
    hhid, Season, s2q1, s2q2,
    s3_q4_1, s3_q4_1a, s3_q4_1b,
    s3_q20_1, s3_q20_2, s3_q20_3
  ) %>%
  slice_head(n = 20)



# Summarise plot area values
crop %>%
  summarise(
    records = n(),
    missing_area = sum(is.na(s2q2)),
    zero_area = sum(s2q2 == 0, na.rm = TRUE),
    negative_area = sum(s2q2 < 0, na.rm = TRUE),
    minimum_area = min(s2q2, na.rm = TRUE),
    q1_area = quantile(s2q2, 0.25, na.rm = TRUE),
    median_area = median(s2q2, na.rm = TRUE),
    q3_area = quantile(s2q2, 0.75, na.rm = TRUE),
    maximum_area = max(s2q2, na.rm = TRUE)
  )




# Count plots with very small recorded areas
crop %>%
  summarise(
    below_1_m2 = sum(s2q2 < 1, na.rm = TRUE),
    below_10_m2 = sum(s2q2 < 10, na.rm = TRUE),
    below_20_m2 = sum(s2q2 < 20, na.rm = TRUE),
    below_50_m2 = sum(s2q2 < 50, na.rm = TRUE),
    below_100_m2 = sum(s2q2 < 100, na.rm = TRUE)
  )

# Inspect all plots with areas below 1 m²
crop %>%
  filter(s2q2 < 1) %>%
  select(
    hhid, Season, s2q1, s2q2,
    s3_q4_1, s3_q4_1a, s3_q4_1b,
    s3_q20_1, s3_q20_2, s3_q20_3
  )



# Check the exact label and units recorded for plot area
attr(crop$s2q2, "label")

# Inspect the distribution of plot area
summary(crop$s2q2)

# Inspect plots with areas below 20 square metres
crop %>%
  filter(s2q2 < 20) %>%
  select(hhid, Season, s2q1, s2q2, s3_q4_1,
         s3_q4_1a, s3_q4_1b, s3_q20_1) %>%
  arrange(s2q2)



library(dplyr)
library(tidyr)

# Reshape the crop names and corresponding harvest quantities
crop_long_check <- crop %>%
  mutate(record_id = row_number()) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(
    entry = as.integer(sub("s3_q4_", "", crop_variable))
  ) %>%
  left_join(
    crop %>%
      mutate(record_id = row_number()) %>%
      select(
        record_id,
        all_of(paste0("s3_q20_", 1:7))
      ) %>%
      pivot_longer(
        cols = all_of(paste0("s3_q20_", 1:7)),
        names_to = "harvest_variable",
        values_to = "harvest_kg"
      ) %>%
      mutate(
        entry = as.integer(sub("s3_q20_", "", harvest_variable))
      ) %>%
      select(record_id, entry, harvest_kg),
    by = c("record_id", "entry")
  ) %>%
  filter(!is.na(crop_code)) %>%
  mutate(
    yield_kg_m2 = if_else(
      s2q2 > 0 & !is.na(harvest_kg),
      harvest_kg / s2q2,
      NA_real_
    )
  )

# Examine the highest provisional yields
crop_long_check %>%
  filter(!is.na(yield_kg_m2), is.finite(yield_kg_m2)) %>%
  arrange(desc(yield_kg_m2)) %>%
  select(
    hhid, Season, s2q1, s2q2,
    crop_code, harvest_kg, yield_kg_m2
  ) %>%
  slice_head(n = 20)



# Inspect every crop recorded on the two unusual plots
crop_long_check %>%
  filter(hhid %in% c(311799, 305771)) %>%
  select(
    hhid, Season, s2q1, s2q2,
    crop_code, harvest_kg,
    yield_kg_m2
  ) %>%
  arrange(hhid, Season, s2q1, desc(harvest_kg))



crop %>%
  filter(
    (hhid == 311799 & Season == 1 & s2q1 == 1) |
      (hhid == 305771 & Season == 1 & s2q1 == 9)
  ) %>%
  select(
    hhid, Season, s2q1, s2q2,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q4_", 1:7, "a")),
    all_of(paste0("s3_q4_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  )



crop_long_check %>%
  filter(hhid %in% c(311799, 305771)) %>%
  select(
    hhid, Season, s2q1, s2q2,
    crop_code, harvest_kg, yield_kg_m2
  ) %>%
  arrange(hhid, Season, s2q1, desc(harvest_kg)) %>%
  print(n = Inf, width = Inf)



crop_long_check %>%
  filter(harvest_kg >= 100000) %>%
  select(
    hhid, Season, s2q1, s2q2,
    crop_code, harvest_kg, yield_kg_m2
  ) %>%
  arrange(desc(harvest_kg))



crop %>%
  filter(hhid == 311799, Season == 1, s2q1 == 1) %>%
  select(
    hhid, Season, s2q1, s2q2,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q4_", 1:7, "a")),
    all_of(paste0("s3_q4_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  ) %>%
  print(width = Inf)



library(dplyr)

# Flag unusual areas and harvest quantities
crop_quality_flags <- crop_long_check %>%
  mutate(
    flag_tiny_area = !is.na(s2q2) & s2q2 < 1,
    flag_small_area = !is.na(s2q2) & s2q2 < 20,
    flag_large_harvest = !is.na(harvest_kg) &
      harvest_kg >= 100000,
    flag_negative_harvest = !is.na(harvest_kg) &
      harvest_kg < 0
  )

# Count the flagged crop records
crop_quality_flags %>%
  summarise(
    tiny_area = sum(flag_tiny_area),
    small_area = sum(flag_small_area),
    large_harvest = sum(flag_large_harvest),
    negative_harvest = sum(flag_negative_harvest)
  )



# Identify distinct plots with very small recorded areas
crop %>%
  filter(s2q2 < 20) %>%
  distinct(hhid, Season, s2q1, s2q2) %>%
  arrange(s2q2) %>%
  print(n = Inf)



# Verify the 700,000 kg cassava record
crop_long_check %>%
  filter(harvest_kg >= 100000) %>%
  select(
    hhid, Season, s2q1, s2q2,
    crop_code, harvest_kg, yield_kg_m2
  )



library(dplyr)

# Check whether crop proportions sum to approximately 100%
proportion_check <- crop %>%
  rowwise() %>%
  mutate(
    number_of_crops = sum(
      !is.na(c_across(all_of(paste0("s3_q4_", 1:7))))
    ),
    proportion_total = sum(
      c_across(all_of(paste0("s3_q4_", 1:7, "a"))),
      na.rm = TRUE
    ),
    density_total = sum(
      c_across(all_of(paste0("s3_q4_", 1:7, "b"))),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

proportion_check %>%
  filter(number_of_crops > 1) %>%
  summarise(
    plots_with_multiple_crops = n(),
    proportion_total_100 = sum(proportion_total == 100),
    proportion_total_below_100 = sum(proportion_total < 100),
    proportion_total_above_100 = sum(proportion_total > 100),
    density_total_100 = sum(density_total == 100),
    median_proportion_total = median(proportion_total)
  )



library(dplyr)
library(tidyr)

# Inspect crop proportion values
crop %>%
  select(all_of(paste0("s3_q4_", 1:7, "a"))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "proportion"
  ) %>%
  filter(!is.na(proportion)) %>%
  summarise(
    records = n(),
    minimum = min(proportion),
    median = median(proportion),
    maximum = max(proportion),
    below_zero = sum(proportion < 0),
    above_100 = sum(proportion > 100)
  )

# Inspect crop density values
crop %>%
  select(all_of(paste0("s3_q4_", 1:7, "b"))) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "density"
  ) %>%
  filter(!is.na(density)) %>%
  summarise(
    records = n(),
    minimum = min(density),
    median = median(density),
    maximum = max(density),
    below_zero = sum(density < 0),
    above_100 = sum(density > 100)
  )


library(haven)
library(dplyr)

# Inspect variable labels
vars_to_check <- c(
  "s2q2",
  paste0("s3_q4_", 1:7, "a"),
  paste0("s3_q4_", 1:7, "b"),
  paste0("s3_q20_", 1:7)
)

variable_labels <- tibble(
  variable = vars_to_check,
  label = sapply(vars_to_check, function(v) {
    as.character(attr(crop[[v]], "label"))
  }),
  type = sapply(vars_to_check, function(v) {
    class(crop[[v]])[1]
  })
)

print(variable_labels, n = Inf)


library(tidyr)

density_check <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q4_", 1:7, "b"))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7, "b")),
    names_to = "density_variable",
    values_to = "density"
  ) %>%
  mutate(entry = as.integer(sub("s3_q4_", "", density_variable))) %>%
  filter(!is.na(density), density > 100) %>%
  select(record_id, hhid, Season, s2q1, s2q2,
         density_variable, density)

print(density_check, n = 20)


library(dplyr)
library(tidyr)

density_check <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q4_", 1:7, "b"))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7, "b")),
    names_to = "density_variable",
    values_to = "density"
  ) %>%
  mutate(
    entry = as.integer(
      sub(".*_([1-7])b$", "\\1", density_variable)
    )
  ) %>%
  filter(!is.na(density), density > 100) %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    density_variable, density, entry
  )

print(density_check, n = 20)


density_by_crop <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q4_", 1:7, "b"))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(
    entry = as.integer(sub(".*_([1-7])$", "\\1", crop_variable))
  ) %>%
  left_join(
    crop %>%
      mutate(record_id = row_number()) %>%
      select(record_id, all_of(paste0("s3_q4_", 1:7, "b"))) %>%
      pivot_longer(
        cols = all_of(paste0("s3_q4_", 1:7, "b")),
        names_to = "density_variable",
        values_to = "density"
      ) %>%
      mutate(
        entry = as.integer(sub(".*_([1-7])b$", "\\1", density_variable))
      ) %>%
      select(record_id, entry, density),
    by = c("record_id", "entry")
  ) %>%
  filter(!is.na(crop_code), !is.na(density)) %>%
  group_by(crop_code) %>%
  summarise(
    crop_entries = n(),
    median_density = median(density),
    maximum_density = max(density),
    values_above_100 = sum(density > 100),
    .groups = "drop"
  ) %>%
  arrange(desc(values_above_100))

print(density_by_crop, n = 20)


library(dplyr)
library(tidyr)

crop_structure <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q4_", 1:7, "a")),
    all_of(paste0("s3_q4_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(
    entry = as.integer(sub(".*_([1-7])$", "\\1", crop_variable))
  ) %>%
  left_join(
    crop %>%
      mutate(record_id = row_number()) %>%
      select(
        record_id,
        all_of(paste0("s3_q4_", 1:7, "a")),
        all_of(paste0("s3_q4_", 1:7, "b")),
        all_of(paste0("s3_q20_", 1:7))
      ) %>%
      pivot_longer(
        cols = -record_id,
        names_to = "variable",
        values_to = "value"
      ) %>%
      mutate(
        entry = as.integer(
          sub(".*_([1-7])[ab]?$", "\\1", variable)
        ),
        measure = case_when(
          grepl("a$", variable) ~ "proportion",
          grepl("b$", variable) ~ "density",
          TRUE ~ "harvest"
        )
      ) %>%
      select(record_id, entry, measure, value) %>%
      pivot_wider(
        names_from = measure,
        values_from = value
      ),
    by = c("record_id", "entry")
  ) %>%
  filter(!is.na(crop_code))

crop_structure %>%
  summarise(
    crop_entries = n(),
    missing_proportion = sum(is.na(proportion)),
    missing_density = sum(is.na(density)),
    missing_harvest = sum(is.na(harvest)),
    density_above_100 = sum(density > 100, na.rm = TRUE),
    proportion_above_100 = sum(proportion > 100, na.rm = TRUE)
  )


library(dplyr)
library(tidyr)

harvest_status <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q7_", 1:7, "a")),
    all_of(paste0("s3_q7_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(
    entry = as.integer(sub(".*_([1-7])$", "\\1", crop_variable))
  ) %>%
  left_join(
    crop %>%
      mutate(record_id = row_number()) %>%
      select(
        record_id,
        all_of(paste0("s3_q7_", 1:7, "a")),
        all_of(paste0("s3_q7_", 1:7, "b")),
        all_of(paste0("s3_q20_", 1:7))
      ) %>%
      pivot_longer(
        cols = -record_id,
        names_to = "variable",
        values_to = "value"
      ) %>%
      mutate(
        entry = as.integer(sub(".*_([1-7])[ab]?$", "\\1", variable)),
        measure = case_when(
          grepl("a$", variable) ~ "planted",
          grepl("b$", variable) ~ "will_harvest",
          TRUE ~ "harvest"
        )
      ) %>%
      select(record_id, entry, measure, value) %>%
      pivot_wider(names_from = measure, values_from = value),
    by = c("record_id", "entry")
  ) %>%
  filter(!is.na(crop_code)) %>%
  group_by(planted, will_harvest) %>%
  summarise(
    crop_entries = n(),
    missing_harvest = sum(is.na(harvest)),
    missing_harvest_percent = round(
      mean(is.na(harvest)) * 100, 2
    ),
    .groups = "drop"
  )

print(harvest_status, n = Inf)



library(haven)
library(dplyr)

variables_to_check <- c(
  "s3_q7_1a", "s3_q7_1b", "s3_q20_1",
  "s3_q7_2a", "s3_q7_2b", "s3_q20_2"
)

question_labels <- tibble(
  variable = variables_to_check,
  label = sapply(variables_to_check, function(v) {
    as.character(attr(crop[[v]], "label"))
  })
)

print(question_labels, n = Inf)

# Inspect the labelled response categories
lapply(variables_to_check, function(v) {
  list(
    variable = v,
    values = head(unique(crop[[v]]), 10),
    labels = attr(crop[[v]], "labels")
  )
})


library(dplyr)
library(tidyr)

harvest_diagnostic <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id,
    all_of(paste0("s3_q7_", 1:7, "a")),
    all_of(paste0("s3_q7_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q20_", 1:7)),
    names_to = "harvest_variable",
    values_to = "harvest_kg"
  ) %>%
  mutate(
    entry = as.integer(
      sub(".*_([1-7])$", "\\1", harvest_variable)
    )
  ) %>%
  left_join(
    crop %>%
      mutate(record_id = row_number()) %>%
      select(
        record_id,
        all_of(paste0("s3_q7_", 1:7, "a")),
        all_of(paste0("s3_q7_", 1:7, "b"))
      ) %>%
      pivot_longer(
        cols = -record_id,
        names_to = "variable",
        values_to = "response"
      ) %>%
      mutate(
        entry = as.integer(
          sub(".*_([1-7])[ab]$", "\\1", variable)
        ),
        question = if_else(
          grepl("a$", variable),
          "planted",
          "will_harvest"
        )
      ) %>%
      select(record_id, entry, question, response) %>%
      pivot_wider(
        names_from = question,
        values_from = response
      ),
    by = c("record_id", "entry")
  ) %>%
  group_by(planted, will_harvest) %>%
  summarise(
    crop_entries = n(),
    missing_harvest = sum(is.na(harvest_kg)),
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    positive_harvest = sum(harvest_kg > 0, na.rm = TRUE),
    negative_harvest = sum(harvest_kg < 0, na.rm = TRUE),
    .groups = "drop"
  )

print(harvest_diagnostic, n = Inf)


library(dplyr)
library(tidyr)

harvest_diagnostic_clean <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q7_", 1:7, "a")),
    all_of(paste0("s3_q7_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(
    entry = as.integer(sub(".*_([1-7])$", "\\1", crop_variable))
  ) %>%
  left_join(
    crop %>%
      mutate(record_id = row_number()) %>%
      select(
        record_id,
        all_of(paste0("s3_q7_", 1:7, "a")),
        all_of(paste0("s3_q7_", 1:7, "b")),
        all_of(paste0("s3_q20_", 1:7))
      ) %>%
      pivot_longer(
        cols = -record_id,
        names_to = "variable",
        values_to = "value"
      ) %>%
      mutate(
        entry = as.integer(sub(".*_([1-7])[ab]?$", "\\1", variable)),
        measure = case_when(
          grepl("a$", variable) ~ "planted",
          grepl("b$", variable) ~ "will_harvest",
          TRUE ~ "harvest"
        )
      ) %>%
      select(record_id, entry, measure, value) %>%
      pivot_wider(names_from = measure, values_from = value),
    by = c("record_id", "entry")
  ) %>%
  filter(!is.na(crop_code)) %>%
  group_by(planted, will_harvest) %>%
  summarise(
    crop_entries = n(),
    missing_harvest = sum(is.na(harvest)),
    zero_harvest = sum(harvest == 0, na.rm = TRUE),
    positive_harvest = sum(harvest > 0, na.rm = TRUE),
    .groups = "drop"
  )

print(harvest_diagnostic_clean, n = Inf)


library(dplyr)
library(tidyr)

unusual_harvest <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q7_", 1:7, "a")),
    all_of(paste0("s3_q7_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(
    entry = as.integer(
      sub(".*_([1-7])$", "\\1", crop_variable)
    ),
    planted_var = paste0("s3_q7_", entry, "a"),
    harvest_status_var = paste0("s3_q7_", entry, "b"),
    harvest_var = paste0("s3_q20_", entry)
  ) %>%
  rowwise() %>%
  mutate(
    planted = as.numeric(
      c_across(all_of(paste0("s3_q7_", 1:7, "a")))[entry]
    ),
    will_harvest = as.numeric(
      c_across(all_of(paste0("s3_q7_", 1:7, "b")))[entry]
    ),
    harvest_kg = as.numeric(
      c_across(all_of(paste0("s3_q20_", 1:7)))[entry]
    )
  ) %>%
  ungroup() %>%
  filter(
    !is.na(crop_code),
    planted == 1,
    will_harvest == 2,
    !is.na(harvest_kg)
  ) %>%
  select(
    record_id, hhid, Season, s2q1, s2q2,
    crop_code, planted, will_harvest, harvest_kg
  )

print(unusual_harvest, n = Inf)


unusual_harvest %>%
  select(
    hhid, Season, s2q1, s2q2,
    crop_code, planted, will_harvest, harvest_kg
  ) %>%
  arrange(desc(harvest_kg))



library(dplyr)
library(tidyr)

# Keep the variables needed for crop production analysis
crop_base <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id, hhid, clust, province, district,
    Season, s2q1, s2q2, weight,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q7_", 1:7, "a")),
    all_of(paste0("s3_q7_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  )

# Reshape the seven crop slots into separate rows
crop_long <- crop_base %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(
    entry = as.integer(sub(".*_", "", crop_variable)),
    planted = as.numeric(
      pick(all_of(paste0("s3_q7_", 1:7, "a")))[
        cbind(seq_len(n()), entry)
      ]
    ),
    will_harvest = as.numeric(
      pick(all_of(paste0("s3_q7_", 1:7, "b")))[
        cbind(seq_len(n()), entry)
      ]
    ),
    harvest_kg = as.numeric(
      pick(all_of(paste0("s3_q20_", 1:7)))[
        cbind(seq_len(n()), entry)
      ]
    )
  ) %>%
  filter(!is.na(crop_code)) %>%
  select(
    hhid, clust, province, district, Season,
    plot = s2q1,
    area_sqm = s2q2,
    weight, crop_code, planted,
    will_harvest, harvest_kg
  )

# Check the result
dim(crop_long)
head(crop_long)


# Number of crop records
nrow(crop_long)

# Number of unique households
n_distinct(crop_long$hhid)

# Missing and zero harvest quantities
crop_long %>%
  summarise(
    crop_records = n(),
    missing_harvest = sum(is.na(harvest_kg)),
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    positive_harvest = sum(harvest_kg > 0, na.rm = TRUE),
    missing_area = sum(is.na(area_sqm)),
    nonpositive_area = sum(area_sqm <= 0, na.rm = TRUE)
  )




library(dplyr)
library(tidyr)

# 1. Give each original row a unique ID
crop_base <- crop %>%
  mutate(record_id = row_number()) %>%
  select(
    record_id, hhid, clust, province, district,
    Season, s2q1, s2q2, weight,
    all_of(paste0("s3_q4_", 1:7)),
    all_of(paste0("s3_q7_", 1:7, "a")),
    all_of(paste0("s3_q7_", 1:7, "b")),
    all_of(paste0("s3_q20_", 1:7))
  )

# 2. Reshape crop names
crop_names <- crop_base %>%
  select(
    record_id, hhid, clust, province, district,
    Season, s2q1, s2q2, weight,
    all_of(paste0("s3_q4_", 1:7))
  ) %>%
  pivot_longer(
    cols = all_of(paste0("s3_q4_", 1:7)),
    names_to = "crop_variable",
    values_to = "crop_code"
  ) %>%
  mutate(entry = as.integer(sub(".*_", "", crop_variable))) %>%
  filter(!is.na(crop_code)) %>%
  select(-crop_variable)

# 3. Reshape planting status
planted_data <- crop_base %>%
  select(record_id, all_of(paste0("s3_q7_", 1:7, "a"))) %>%
  pivot_longer(
    cols = -record_id,
    names_to = "variable",
    values_to = "planted"
  ) %>%
  mutate(entry = as.integer(sub(".*_([1-7])a$", "\\1", variable))) %>%
  select(record_id, entry, planted)

# 4. Reshape expected harvest status
harvest_status <- crop_base %>%
  select(record_id, all_of(paste0("s3_q7_", 1:7, "b"))) %>%
  pivot_longer(
    cols = -record_id,
    names_to = "variable",
    values_to = "will_harvest"
  ) %>%
  mutate(entry = as.integer(sub(".*_([1-7])b$", "\\1", variable))) %>%
  select(record_id, entry, will_harvest)

# 5. Reshape harvest quantities
harvest_data <- crop_base %>%
  select(record_id, all_of(paste0("s3_q20_", 1:7))) %>%
  pivot_longer(
    cols = -record_id,
    names_to = "variable",
    values_to = "harvest_kg"
  ) %>%
  mutate(entry = as.integer(sub(".*_", "", variable))) %>%
  select(record_id, entry, harvest_kg)

# 6. Combine the four parts
crop_long <- crop_names %>%
  left_join(planted_data, by = c("record_id", "entry")) %>%
  left_join(harvest_status, by = c("record_id", "entry")) %>%
  left_join(harvest_data, by = c("record_id", "entry")) %>%
  select(
    hhid, clust, province, district, Season,
    plot = s2q1, area_sqm = s2q2,
    weight, crop_code, planted,
    will_harvest, harvest_kg
  )

# 7. Check the result
dim(crop_long)
head(crop_long)


crop_long %>%
  summarise(
    crop_records = n(),
    unique_households = n_distinct(hhid),
    missing_harvest = sum(is.na(harvest_kg)),
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    positive_harvest = sum(harvest_kg > 0, na.rm = TRUE),
    missing_area = sum(is.na(area_sqm)),
    nonpositive_area = sum(area_sqm <= 0, na.rm = TRUE)
  )






# summary of harvest data by crop type
crop_summary <- crop_long %>%
  group_by(crop_code) %>%
  summarise(
    crop_records = n(),
    
    missing_harvest = sum(is.na(harvest_kg)),
    
    zero_harvest = sum(harvest_kg == 0, na.rm = TRUE),
    
    positive_harvest = sum(harvest_kg > 0, na.rm = TRUE),
    
    median_harvest_kg = if (all(is.na(harvest_kg))) {
      NA_real_
    } else {
      median(harvest_kg, na.rm = TRUE)
    },
    
    maximum_harvest_kg = if (all(is.na(harvest_kg))) {
      NA_real_
    } else {
      max(harvest_kg, na.rm = TRUE)
    },
    
    .groups = "drop"
  ) %>%
  arrange(desc(crop_records))

# Display the 15 most frequently recorded crop types
print(crop_summary, n = 15)

# Check whether the summary contains -Inf values
sum(is.infinite(crop_summary$maximum_harvest_kg))




# Inspect crop proportion and crop density variables
names(crop)[grepl("s3_q4_[1-7][ab]$", names(crop))]

# Examine their distributions
summary(crop %>%
          select(all_of(paste0("s3_q4_", 1:7, "a"))))

summary(crop %>%
          select(all_of(paste0("s3_q4_", 1:7, "b"))))



library(dplyr)

# Count crop entries recorded in each plot-season record
plot_crop_check <- crop_long %>%
  group_by(hhid, Season, plot) %>%
  summarise(
    number_of_crops = n_distinct(crop_code),
    total_harvest_kg = if (all(is.na(harvest_kg))) {
      NA_real_
    } else {
      sum(harvest_kg, na.rm = TRUE)
    },
    plot_area_sqm = first(area_sqm),
    .groups = "drop"
  )

# How many plot-season records have one or multiple crops?
plot_crop_check %>%
  count(number_of_crops, name = "plot_season_records") %>%
  arrange(number_of_crops) %>%
  print(n = 20)

# Inspect the plots that have only one crop
single_crop_plots <- plot_crop_check %>%
  filter(number_of_crops == 1)

summary(single_crop_plots$plot_area_sqm)




# Join the crop name and harvest data to single-crop plots
single_crop_productivity <- crop_long %>%
  group_by(hhid, Season, plot) %>%
  filter(n_distinct(crop_code) == 1) %>%
  ungroup() %>%
  mutate(
    productivity_kg_ha = if_else(
      !is.na(harvest_kg) &
        !is.na(area_sqm) &
        harvest_kg >= 0 &
        area_sqm > 0,
      harvest_kg / area_sqm * 10000,
      NA_real_
    )
  )

# Summarize the provisional productivity
single_crop_productivity %>%
  summarise(
    records = n(),
    missing_harvest = sum(is.na(harvest_kg)),
    valid_productivity = sum(!is.na(productivity_kg_ha)),
    median_kg_ha = median(
      productivity_kg_ha, na.rm = TRUE
    ),
    maximum_kg_ha = max(
      productivity_kg_ha, na.rm = TRUE
    )
  )



# Inspect the 10 highest provisional productivity values
single_crop_productivity %>%
  filter(!is.na(productivity_kg_ha)) %>%
  arrange(desc(productivity_kg_ha)) %>%
  select(
    hhid, Season, plot, crop_code,
    area_sqm, harvest_kg, productivity_kg_ha
  ) %>%
  print(n = 10)

# Examine the distribution more robustly
quantile(
  single_crop_productivity$productivity_kg_ha,
  probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1),
  na.rm = TRUE
)



# Inspect the original survey row for the extreme cassava value
crop %>%
  filter(
    hhid == 305771,
    as.numeric(Season) == 1,
    s2q1 == 9
  ) %>%
  select(
    hhid, Season, s2q1, s2q2,
    starts_with("s3_q4_"),
    starts_with("s3_q20_")
  ) %>%
  print(width = Inf)



# Calculate the 99th-percentile threshold
p99 <- quantile(
  single_crop_productivity$productivity_kg_ha,
  probs = 0.99,
  na.rm = TRUE
)

# Count and inspect flagged observations
high_productivity <- single_crop_productivity %>%
  filter(productivity_kg_ha > p99) %>%
  arrange(desc(productivity_kg_ha))

cat("99th-percentile threshold:", p99, "kg/ha\n")
cat("Records above threshold:", nrow(high_productivity), "\n")

high_productivity %>%
  select(
    hhid, Season, plot, crop_code,
    area_sqm, harvest_kg, productivity_kg_ha
  ) %>%
  print(n = 10)



# Summarize the 104 flagged records by crop
high_productivity_summary <- high_productivity %>%
  group_by(crop_code) %>%
  summarise(
    flagged_records = n(),
    median_productivity = median(
      productivity_kg_ha, na.rm = TRUE
    ),
    maximum_productivity = max(
      productivity_kg_ha, na.rm = TRUE
    ),
    median_area_sqm = median(
      area_sqm, na.rm = TRUE
    ),
    maximum_harvest_kg = max(
      harvest_kg, na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(flagged_records))

print(high_productivity_summary, n = 30)



high_productivity %>%
  summarise(
    flagged_records = n(),
    records_area_below_100sqm = sum(area_sqm < 100, na.rm = TRUE),
    records_harvest_above_10000kg = sum(harvest_kg > 10000, na.rm = TRUE),
    median_area_sqm = median(area_sqm, na.rm = TRUE),
    median_harvest_kg = median(harvest_kg, na.rm = TRUE)
  )



library(dplyr)

# Keep three important food crops
selected_crop_productivity <- single_crop_productivity %>%
  filter(
    as.numeric(crop_code) %in% c(101, 305, 111),
    !is.na(productivity_kg_ha)
  ) %>%
  mutate(
    crop_name = case_when(
      as.numeric(crop_code) == 101 ~ "Maize",
      as.numeric(crop_code) == 305 ~ "Cassava",
      as.numeric(crop_code) == 111 ~ "Sweet potato"
    )
  )

# Summarize productivity separately for each crop
selected_crop_productivity %>%
  group_by(crop_name) %>%
  summarise(
    records = n(),
    median_kg_ha = median(productivity_kg_ha),
    q75_kg_ha = quantile(productivity_kg_ha, 0.75),
    q95_kg_ha = quantile(productivity_kg_ha, 0.95),
    maximum_kg_ha = max(productivity_kg_ha),
    .groups = "drop"
  ) %>%
  print()




library(dplyr)

# Step 1: Summarize productivity at household and crop level
household_crop_productivity <- selected_crop_productivity %>%
  group_by(hhid, crop_name) %>%
  summarise(
    household_median_kg_ha = median(
      productivity_kg_ha,
      na.rm = TRUE
    ),
    plot_season_records = n(),
    .groups = "drop"
  )

# Step 2: Check that extension data has one row per household
extension_household %>%
  summarise(
    total_rows = n(),
    unique_households = n_distinct(hhid),
    missing_access = sum(is.na(extension_access))
  )

# Step 3: Join extension access to household productivity
analysis_data <- household_crop_productivity %>%
  left_join(
    extension_household %>%
      select(hhid, extension_access),
    by = "hhid"
  )

# Step 4: Check whether the join matched the households
analysis_data %>%
  summarise(
    households = n_distinct(hhid),
    records = n(),
    missing_extension_access = sum(is.na(extension_access))
  )

# Step 5: Compare productivity by crop and extension access
productivity_comparison <- analysis_data %>%
  filter(!is.na(extension_access)) %>%
  group_by(crop_name, extension_access) %>%
  summarise(
    households = n_distinct(hhid),
    median_productivity_kg_ha = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    mean_productivity_kg_ha = mean(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(productivity_comparison)



# Inspect the 10 highest household-level productivity values
analysis_data %>%
  group_by(crop_name) %>%
  slice_max(
    order_by = household_median_kg_ha,
    n = 10,
    with_ties = FALSE
  ) %>%
  select(
    hhid,
    crop_name,
    extension_access,
    household_median_kg_ha,
    plot_season_records
  ) %>%
  arrange(crop_name, desc(household_median_kg_ha)) %>%
  print(n = 30)

# Calculate the percentage difference in medians
productivity_comparison %>%
  select(
    crop_name,
    extension_access,
    households,
    median_productivity_kg_ha
  ) %>%
  tidyr::pivot_wider(
    names_from = extension_access,
    values_from = median_productivity_kg_ha,
    names_prefix = "access_"
  ) %>%
  mutate(
    difference_kg_ha = access_1 - access_0,
    percentage_difference =
      100 * (access_1 - access_0) / access_0
  )



library(dplyr)
library(tidyr)

percentage_comparison <- productivity_comparison %>%
  select(
    crop_name,
    extension_access,
    median_productivity_kg_ha
  ) %>%
  pivot_wider(
    names_from = extension_access,
    values_from = median_productivity_kg_ha,
    names_prefix = "access_"
  ) %>%
  mutate(
    difference_kg_ha = access_1 - access_0,
    percentage_difference =
      100 * difference_kg_ha / access_0
  )

print(percentage_comparison)



crop_long %>%
  filter(
    hhid == 305771,
    as.numeric(crop_code) == 305
  ) %>%
  select(
    hhid, Season, plot, area_sqm,
    crop_code, harvest_kg, weight
  ) %>%
  print(n = Inf)




# Inspect the selected single-crop records for this household
single_crop_productivity %>%
  filter(
    hhid == 305771,
    as.numeric(crop_code) == 305
  ) %>%
  mutate(
    productivity_kg_ha = harvest_kg / area_sqm * 10000,
    flag_extreme = hhid == 305771 &
      Season == 1 & plot == 9
  ) %>%
  select(
    hhid, Season, plot, area_sqm,
    harvest_kg, productivity_kg_ha, flag_extreme
  ) %>%
  print(n = Inf)



# Sensitivity check: retain all records in the main data,
# but temporarily exclude the one flagged plot for comparison

selected_without_flag <- selected_crop_productivity %>%
  filter(
    !(hhid == 305771 &
        as.numeric(crop_code) == 305 &
        Season == 1 &
        plot == 9)
  )

# Recalculate household-level medians
household_productivity_sensitivity <- selected_without_flag %>%
  group_by(hhid, crop_name) %>%
  summarise(
    household_median_kg_ha = median(productivity_kg_ha),
    .groups = "drop"
  ) %>%
  left_join(
    extension_household %>%
      select(hhid, extension_access),
    by = "hhid"
  )

# Compare groups after this one-record sensitivity check
household_productivity_sensitivity %>%
  group_by(crop_name, extension_access) %>%
  summarise(
    households = n_distinct(hhid),
    median_productivity_kg_ha =
      median(household_median_kg_ha),
    .groups = "drop"
  ) %>%
  print()



# Exploratory, unweighted statistical tests
wilcoxon_results <- household_productivity_sensitivity %>%
  filter(!is.na(extension_access)) %>%
  group_by(crop_name) %>%
  summarise(
    households_without_access =
      sum(extension_access == 0),
    households_with_access =
      sum(extension_access == 1),
    p_value = wilcox.test(
      household_median_kg_ha[extension_access == 1],
      household_median_kg_ha[extension_access == 0],
      exact = FALSE
    )$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    interpretation = case_when(
      p_value < 0.05 ~
        "Evidence of a distributional difference",
      TRUE ~
        "Insufficient evidence of a distributional difference"
    )
  )

print(wilcoxon_results)



wilcoxon_results <- wilcoxon_results %>%
  mutate(
    p_value_holm = p.adjust(p_value, method = "holm"),
    significant_after_adjustment = p_value_holm < 0.05
  )

print(wilcoxon_results)




library(dplyr)

# Inspect the variables used for possible adjustment
crop %>%
  select(
    hhid, district, Season, weight,
    s4_q1, s4_q4, s4_q22
  ) %>%
  summarise(
    rows = n(),
    missing_district = sum(is.na(district)),
    missing_season = sum(is.na(Season)),
    missing_weight = sum(is.na(weight)),
    missing_organic_fertilizer = sum(is.na(s4_q1)),
    missing_inorganic_fertilizer = sum(is.na(s4_q4)),
    missing_irrigation = sum(is.na(s4_q22))
  ) %>%
  print()

# Inspect the recorded categories and labels
table(crop$Season, useNA = "ifany")
table(crop$s4_q1, useNA = "ifany")
table(crop$s4_q4, useNA = "ifany")
table(crop$s4_q22, useNA = "ifany")



library(haven)

# Display variable descriptions and value labels
variables_to_check <- c("s4_q1", "s4_q4", "s4_q22")

for (v in variables_to_check) {
  cat("\n-----------------------------\n")
  cat("Variable:", v, "\n")
  cat("Description:", attr(crop[[v]], "label"), "\n")
  cat("Value labels:\n")
  print(attr(crop[[v]], "labels"))
  cat("Counts with labels:\n")
  print(table(as_factor(crop[[v]]), useNA = "ifany"))
}




library(dplyr)

# Check whether plot-season keys are unique in the original crop data
plot_covariates <- crop %>%
  transmute(
    hhid,
    Season,
    plot = s2q1,
    organic_fertilizer = case_when(
      as.numeric(s4_q1) == 1 ~ 1,
      as.numeric(s4_q1) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    inorganic_fertilizer = case_when(
      as.numeric(s4_q4) == 1 ~ 1,
      as.numeric(s4_q4) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    irrigation_status = factor(
      as.numeric(s4_q22),
      levels = c(1, 2, 3, 4),
      labels = c(
        "Yes",
        "No",
        "No need",
        "Cannot afford or no water"
      )
    )
  )

# Check for duplicate keys before joining
duplicate_keys <- plot_covariates %>%
  count(hhid, Season, plot) %>%
  filter(n > 1)

cat("Duplicate plot-season keys:", nrow(duplicate_keys), "\n")

# Rebuild crop_analysis with no duplicate district column
crop_analysis <- selected_crop_productivity %>%
  select(-any_of(c(
    "organic_fertilizer",
    "inorganic_fertilizer",
    "irrigation_status"
  ))) %>%
  left_join(
    plot_covariates,
    by = c("hhid", "Season", "plot"),
    relationship = "many-to-one"
  )

# Check the result
crop_analysis %>%
  summarise(
    records = n(),
    missing_district = sum(is.na(district)),
    missing_organic = sum(is.na(organic_fertilizer)),
    missing_inorganic = sum(is.na(inorganic_fertilizer)),
    missing_irrigation = sum(is.na(irrigation_status))
  ) %>%
  print()

# Confirm that the join did not increase the number of records
cat("Selected records:", nrow(selected_crop_productivity), "\n")
cat("Joined records:", nrow(crop_analysis), "\n")




household_covariates <- crop_analysis %>%
  group_by(hhid, crop_name) %>%
  summarise(
    district = first(district),
    proportion_organic_fertilizer =
      if (all(is.na(organic_fertilizer))) NA_real_
    else mean(organic_fertilizer, na.rm = TRUE),
    proportion_inorganic_fertilizer =
      if (all(is.na(inorganic_fertilizer))) NA_real_
    else mean(inorganic_fertilizer, na.rm = TRUE),
    proportion_irrigated =
      if (all(is.na(irrigation_status))) NA_real_
    else mean(irrigation_status == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

print(head(household_covariates))



# Inspect the original irrigation value labels
dput(attr(crop$s4_q22, "labels"))

# Display the original response counts
table(crop$s4_q22, useNA = "ifany")

# Display labelled categories, if supported by haven
haven::as_factor(crop$s4_q22) %>%
  table(useNA = "ifany")



library(dplyr)

plot_covariates <- crop %>%
  transmute(
    hhid,
    Season,
    plot = s2q1,
    
    organic_fertilizer = case_when(
      as.numeric(s4_q1) == 1 ~ 1,
      as.numeric(s4_q1) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    inorganic_fertilizer = case_when(
      as.numeric(s4_q4) == 1 ~ 1,
      as.numeric(s4_q4) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    irrigation_status = factor(
      as.numeric(s4_q22),
      levels = c(1, 2, 3, 4),
      labels = c(
        "Yes",
        "No need for irrigation",
        "Cannot afford irrigation",
        "No water available"
      )
    )
  )

crop_analysis <- selected_crop_productivity %>%
  select(-any_of(c(
    "organic_fertilizer",
    "inorganic_fertilizer",
    "irrigation_status"
  ))) %>%
  left_join(
    plot_covariates,
    by = c("hhid", "Season", "plot"),
    relationship = "many-to-one"
  )

# Verify the corrected categories
table(crop_analysis$irrigation_status, useNA = "ifany")




household_covariates <- crop_analysis %>%
  group_by(hhid, crop_name) %>%
  summarise(
    district = first(district),
    
    proportion_organic_fertilizer =
      mean(organic_fertilizer, na.rm = TRUE),
    
    proportion_inorganic_fertilizer =
      mean(inorganic_fertilizer, na.rm = TRUE),
    
    proportion_irrigated =
      mean(irrigation_status == "Yes", na.rm = TRUE),
    
    .groups = "drop"
  )

print(head(household_covariates))




library(dplyr)

# Join the household-level productivity data to the covariates
model_data <- household_productivity_sensitivity %>%
  left_join(
    household_covariates,
    by = c("hhid", "crop_name")
  ) %>%
  mutate(
    district = factor(district),
    extension_access = factor(
      extension_access,
      levels = c(0, 1),
      labels = c("No access", "Access")
    )
  )

# Check the dataset before modelling
model_data %>%
  group_by(crop_name) %>%
  summarise(
    households = n(),
    missing_productivity = sum(is.na(household_median_kg_ha)),
    missing_extension = sum(is.na(extension_access)),
    missing_district = sum(is.na(district)),
    missing_organic = sum(is.na(proportion_organic_fertilizer)),
    missing_inorganic = sum(is.na(proportion_inorganic_fertilizer)),
    missing_irrigation = sum(is.na(proportion_irrigated)),
    .groups = "drop"
  ) %>%
  print()




# Fit a separate exploratory model for each crop
crop_models <- lapply(
  split(model_data, model_data$crop_name),
  function(dat) {
    
    lm(
      log1p(household_median_kg_ha) ~
        extension_access +
        district +
        proportion_organic_fertilizer +
        proportion_inorganic_fertilizer +
        proportion_irrigated,
      data = dat
    )
  }
)

# Display the results for each crop
for (crop in names(crop_models)) {
  cat("\n\n==========", crop, "==========\n")
  print(summary(crop_models[[crop]]))
}




# Check whether irrigation varies within each crop
model_data %>%
  group_by(crop_name) %>%
  summarise(
    households = n(),
    distinct_irrigation_values =
      n_distinct(proportion_irrigated),
    minimum_irrigation = min(proportion_irrigated, na.rm = TRUE),
    maximum_irrigation = max(proportion_irrigated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Calculate confidence intervals for the extension-access coefficients
for (crop in names(crop_models)) {
  cat("\n\n==========", crop, "==========\n")
  
  print(
    confint(crop_models[[crop]], "extension_accessAccess")
  )
}



# Refit cassava model without irrigation
cassava_model_corrected <- lm(
  log1p(household_median_kg_ha) ~
    extension_access +
    district +
    proportion_organic_fertilizer +
    proportion_inorganic_fertilizer,
  data = filter(model_data, crop_name == "Cassava")
)

summary(cassava_model_corrected)

# Extension-access confidence interval
confint(
  cassava_model_corrected,
  "extension_accessAccess"
)



# Use the corrected cassava model
final_models <- crop_models
final_models[["Cassava"]] <- cassava_model_corrected

# Extract the key statistics
results_table <- bind_rows(
  lapply(names(final_models), function(crop_name) {
    
    model <- final_models[[crop_name]]
    coefficient <- coef(model)["extension_accessAccess"]
    interval <- confint(model, "extension_accessAccess")
    model_summary <- summary(model)
    
    data.frame(
      crop = crop_name,
      households = nobs(model),
      coefficient = unname(coefficient),
      percentage_difference =
        100 * (exp(unname(coefficient)) - 1),
      CI_lower = interval[1, 1],
      CI_upper = interval[1, 2],
      p_value = coef(model_summary)[
        "extension_accessAccess", "Pr(>|t|)"
      ],
      adjusted_R_squared = model_summary$adj.r.squared
    )
  })
)

print(results_table, digits = 4)




library(dplyr)

final_models <- crop_models
final_models[["Cassava"]] <- cassava_model_corrected

results_table <- bind_rows(
  lapply(names(final_models), function(crop_name) {
    
    model <- final_models[[crop_name]]
    estimate <- coef(model)["extension_accessAccess"]
    ci <- confint(model, "extension_accessAccess")
    model_summary <- summary(model)
    
    data.frame(
      Crop = crop_name,
      Households = nobs(model),
      Estimate_log_scale = unname(estimate),
      Approx_difference_percent =
        100 * (exp(unname(estimate)) - 1),
      CI_lower_log = ci[1, 1],
      CI_upper_log = ci[1, 2],
      P_value = coef(model_summary)[
        "extension_accessAccess", "Pr(>|t|)"
      ],
      Adjusted_R_squared = model_summary$adj.r.squared
    )
  })
)

print(results_table, digits = 4)



percentage_results <- bind_rows(
  lapply(names(final_models), function(crop_name) {
    
    model <- final_models[[crop_name]]
    
    estimate <- coef(model)["extension_accessAccess"]
    ci <- confint(model, "extension_accessAccess")
    
    data.frame(
      Crop = crop_name,
      Estimated_difference_percent =
        100 * (exp(unname(estimate)) - 1),
      Lower_95_percent =
        100 * (exp(ci[1, 1]) - 1),
      Upper_95_percent =
        100 * (exp(ci[1, 2]) - 1),
      P_value = coef(summary(model))[
        "extension_accessAccess", "Pr(>|t|)"
      ]
    )
  })
)

print(percentage_results, digits = 4)




class(crop)
class(crop_long)
class(extension)
class(extension_household)

# Check the structure of the survey list
class(survey)
names(survey)



crop <- survey[[
  "AHS2024_Section3_4_CROP GROWN, SEEDS AND PRODUCTION & AGRICULTURAL INPUTS AND PRACTICES"
]]

# Confirm that crop is a data frame
class(crop)
dim(crop)
names(crop)[1:10]



library(dplyr)

crop_households <- crop %>%
  distinct(hhid)

extension_households <- extension_household %>%
  distinct(hhid)

linkage_check <- crop_households %>%
  mutate(
    found_in_extension =
      hhid %in% extension_households$hhid
  ) %>%
  count(
    found_in_extension,
    name = "number_of_households"
  )

print(linkage_check)

unmatched_households <- crop_households %>%
  filter(!hhid %in% extension_households$hhid)

cat(
  "Crop households without a matching extension record:",
  nrow(unmatched_households),
  "\n"
)



# Display the household ID that did not match
print(unmatched_households)

# Check whether this household appears in the original crop data
unmatched_crop_records <- crop %>%
  semi_join(unmatched_households, by = "hhid")

print(
  unmatched_crop_records %>%
    select(hhid, clust, province, district, Season) %>%
    distinct()
)



# Check missing household IDs
cat("Missing IDs in crop data:",
    sum(is.na(crop$hhid)), "\n")

cat("Missing IDs in extension data:",
    sum(is.na(extension_household$hhid)), "\n")

# Inspect the unmatched ID's data type and value
str(unmatched_households$hhid)
dput(unmatched_households$hhid)




# Check household 303852 in the original extension dataset
extension_check <- extension %>%
  filter(hhid == 303852)

cat("Rows found in extension data:",
    nrow(extension_check), "\n")

print(extension_check)

# Check whether the household appears in the household-members dataset
household_members <- survey[[
  "AHS2024_Section1_HOUSEHOLD MEMBERS CHARACTERISTICS"
]]

members_check <- household_members %>%
  filter(hhid == 303852)

cat("Rows found in household-members data:",
    nrow(members_check), "\n")



# Compare unique household counts
cat(
  "Unique households in crop data:",
  n_distinct(crop$hhid), "\n"
)

cat(
  "Unique households in extension data:",
  n_distinct(extension$hhid), "\n"
)

# Check whether extension household 303852 is present
cat(
  "Household 303852 present in extension data:",
  any(extension$hhid == 303852, na.rm = TRUE), "\n"
)

# Check service codes for households with extension records
extension %>%
  count(s6q1, sort = TRUE) %>%
  print(n = Inf)



# Identify extension households absent from crop data
extension_only <- extension_households %>%
  filter(!hhid %in% crop_households$hhid)

cat("Extension households absent from crop data:",
    nrow(extension_only), "\n")

# Check their presence in the household-members module
extension_only_check <- extension_only %>%
  mutate(
    in_household_members =
      hhid %in% household_members$hhid
  )

print(
  extension_only_check %>%
    count(in_household_members, name = "households")
)

# Show a few IDs for inspection
print(head(extension_only, 10))



extension_code_check <- extension %>%
  mutate(service_code = as.numeric(s6q1)) %>%
  group_by(hhid) %>%
  summarise(
    has_none = any(service_code == 26, na.rm = TRUE),
    has_service = any(
      !is.na(service_code) &
        service_code != 26
    ),
    number_of_rows = n(),
    .groups = "drop"
  )

# Households with potentially conflicting codes
conflicting_households <- extension_code_check %>%
  filter(has_none & has_service)

cat("Households with both None and service codes:",
    nrow(conflicting_households), "\n")

# Counts of household classifications
extension_code_check %>%
  summarise(
    total_households = n(),
    none_only = sum(has_none & !has_service),
    service_only = sum(has_service & !has_none),
    both_none_and_service = sum(has_none & has_service),
    neither_code = sum(!has_none & !has_service)
  ) %>%
  print()




extension_code_check <- extension %>%
  mutate(service_code = as.numeric(s6q1)) %>%
  group_by(hhid) %>%
  summarise(
    has_none = any(service_code == 26, na.rm = TRUE),
    has_service = any(
      !is.na(service_code) & service_code != 26
    ),
    number_of_rows = n(),
    .groups = "drop"
  )

# Recalculate household classifications
extension_code_check %>%
  summarise(
    total_households = n(),
    none_only = sum(has_none & !has_service),
    service_only = sum(has_service & !has_none),
    both_none_and_service = sum(has_none & has_service),
    neither_code = sum(!has_none & !has_service)
  ) %>%
  print()

# Inspect households with both None and service codes
conflicting_households <- extension_code_check %>%
  filter(has_none & has_service)

print(conflicting_households)

# Display all service codes recorded for the conflicting household
if (nrow(conflicting_households) > 0) {
  extension %>%
    filter(hhid %in% conflicting_households$hhid) %>%
    select(hhid, s6q1, s6q2, s6q3) %>%
    arrange(hhid) %>%
    print(n = Inf)
}




extension_household <- extension %>%
  mutate(service_code = as.numeric(s6q1)) %>%
  group_by(hhid) %>%
  summarise(
    has_none = any(service_code == 26, na.rm = TRUE),
    has_service = any(
      !is.na(service_code) & service_code != 26
    ),
    extension_access = as.integer(has_service),
    conflicting_codes = has_none & has_service,
    number_of_service_types = n_distinct(
      service_code[
        !is.na(service_code) & service_code != 26
      ]
    ),
    .groups = "drop"
  )

# Verify the updated classifications
extension_household %>%
  count(extension_access, conflicting_codes) %>%
  print()

# Check household 304368
extension_household %>%
  filter(hhid == 304368) %>%
  print()




# Compare old and new extension classifications
access_comparison <- extension_household %>%
  select(hhid, new_access = extension_access) %>%
  left_join(
    # Reconstruct the previous rule
    extension %>%
      mutate(service_code = as.numeric(s6q1)) %>%
      group_by(hhid) %>%
      summarise(
        old_access = as.integer(
          any(
            !is.na(service_code) & service_code != 26
          )
        ),
        .groups = "drop"
      ),
    by = "hhid"
  )

access_comparison %>%
  summarise(
    households_checked = n(),
    changed_access_status = sum(
      new_access != old_access,
      na.rm = TRUE
    )
  ) %>%
  print()



# 1. Inspect variables that may describe the sampling design
variable_info <- data.frame(
  variable = names(crop),
  label = sapply(names(crop), function(v) {
    label <- attr(crop[[v]], "label")
    if (is.null(label)) "" else as.character(label)[1]
  }),
  stringsAsFactors = FALSE
)

sampling_variables <- variable_info[
  grepl(
    "weight|strat|cluster|PSU|primary sampling|sample design|enumeration area|EA",
    paste(variable_info$variable, variable_info$label),
    ignore.case = TRUE
  ),
]

print(sampling_variables, row.names = FALSE)

# 2. Inspect the weight variable's label and summary
cat("Weight label:\n")
print(attr(crop$weight, "label"))

summary(crop$weight)

# 3. Check weights and clusters by season
crop %>%
  group_by(Season) %>%
  summarise(
    records = n(),
    households = n_distinct(hhid),
    clusters = n_distinct(clust),
    missing_weights = sum(is.na(weight)),
    nonpositive_weights = sum(
      !is.na(weight) & weight <= 0
    ),
    minimum_weight = min(weight, na.rm = TRUE),
    median_weight = median(weight, na.rm = TRUE),
    maximum_weight = max(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()



# Check the weight and cluster variables in each survey module
for (module_name in names(survey)) {
  
  dat <- survey[[module_name]]
  
  cat("\nMODULE:", module_name, "\n")
  
  for (v in intersect(
    c("hhid", "clust", "Cluster_ID", "weight",
      "province", "district"),
    names(dat)
  )) {
    cat(
      v, "| label:",
      if (is.null(attr(dat[[v]], "label"))) {
        "No label"
      } else {
        attr(dat[[v]], "label")
      },
      "| distinct values:",
      dplyr::n_distinct(dat[[v]], na.rm = TRUE),
      "\n"
    )
  }
}

# Check whether Cluster_ID and clust agree in the crop module
if ("Cluster_ID" %in% names(crop)) {
  crop %>%
    summarise(
      records = n(),
      same_cluster = sum(
        !is.na(clust) & !is.na(Cluster_ID) &
          clust == Cluster_ID
      ),
      different_cluster = sum(
        !is.na(clust) & !is.na(Cluster_ID) &
          clust != Cluster_ID
      ),
      missing_clust = sum(is.na(clust)),
      missing_Cluster_ID = sum(is.na(Cluster_ID))
    ) %>%
    print()
}


library(dplyr)

# Check whether the cluster variables have a consistent relationship
cluster_check <- crop %>%
  distinct(clust, Cluster_ID) %>%
  count(clust, name = "number_of_Cluster_IDs")

print(cluster_check)

# Check how many distinct clust values each Cluster_ID has
reverse_cluster_check <- crop %>%
  distinct(clust, Cluster_ID) %>%
  count(Cluster_ID, name = "number_of_clust_values")

print(reverse_cluster_check)

# Check whether each household has one cluster and one weight
household_check <- crop %>%
  group_by(hhid) %>%
  summarise(
    number_of_clusters = n_distinct(clust),
    number_of_cluster_IDs = n_distinct(Cluster_ID),
    number_of_weights = n_distinct(weight),
    number_of_seasons = n_distinct(Season),
    .groups = "drop"
  )

print(
  household_check %>%
    summarise(
      households = n(),
      households_multiple_clusters =
        sum(number_of_clusters > 1),
      households_multiple_Cluster_IDs =
        sum(number_of_cluster_IDs > 1),
      households_multiple_weights =
        sum(number_of_weights > 1),
      households_multiple_seasons =
        sum(number_of_seasons > 1)
    )
)

# Check whether the weight is constant within each cluster
cluster_weight_check <- crop %>%
  distinct(clust, weight) %>%
  count(clust, name = "number_of_weights")

print(
  cluster_weight_check %>%
    summarise(
      clusters = n(),
      clusters_with_multiple_weights =
        sum(number_of_weights > 1)
    )
)



# Check the relationship between the two cluster identifiers
crop %>%
  distinct(clust, Cluster_ID) %>%
  summarise(
    number_of_pairs = n(),
    pairs_where_difference_is_8000 =
      sum(clust - Cluster_ID == 8000)
  ) %>%
  print()

# Check weight variation within each household and season
weight_by_household_season <- crop %>%
  group_by(hhid, Season) %>%
  summarise(
    number_of_weights = n_distinct(weight),
    minimum_weight = min(weight, na.rm = TRUE),
    maximum_weight = max(weight, na.rm = TRUE),
    .groups = "drop"
  )

weight_by_household_season %>%
  summarise(
    household_season_groups = n(),
    groups_with_multiple_weights =
      sum(number_of_weights > 1),
    groups_with_one_weight =
      sum(number_of_weights == 1)
  ) %>%
  print()

# Compare the number of distinct weights across seasons
crop %>%
  group_by(Season) %>%
  summarise(
    records = n(),
    households = n_distinct(hhid),
    distinct_weights = n_distinct(weight),
    minimum_weight = min(weight, na.rm = TRUE),
    median_weight = median(weight, na.rm = TRUE),
    maximum_weight = max(weight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Display household-season groups with multiple weights, if any
weight_by_household_season %>%
  filter(number_of_weights > 1) %>%
  print(n = 20)

# Display clusters that have multiple weights
crop %>%
  distinct(clust, weight) %>%
  count(clust, name = "number_of_weights") %>%
  filter(number_of_weights > 1) %>%
  arrange(desc(number_of_weights)) %>%
  print(n = 20)


# Compare household weights across seasons
household_season_weights <- crop %>%
  distinct(hhid, Season, weight) %>%
  group_by(hhid) %>%
  summarise(
    seasons_observed = n_distinct(Season),
    weights_observed = n_distinct(weight),
    .groups = "drop"
  )

household_season_weights %>%
  summarise(
    households = n(),
    households_in_multiple_seasons =
      sum(seasons_observed > 1),
    households_with_different_weights =
      sum(weights_observed > 1)
  ) %>%
  print()

# Examine the households with different weights
crop %>%
  distinct(hhid, Season, weight) %>%
  semi_join(
    household_season_weights %>%
      filter(weights_observed > 1),
    by = "hhid"
  ) %>%
  arrange(hhid, Season) %>%
  print(n = 30)


library(dplyr)

# Check the number of distinct weights within each cluster and season
cluster_season_weights <- crop %>%
  distinct(clust, Season, weight) %>%
  group_by(clust, Season) %>%
  summarise(
    number_of_weights = n_distinct(weight),
    minimum_weight = min(weight, na.rm = TRUE),
    maximum_weight = max(weight, na.rm = TRUE),
    .groups = "drop"
  )

cluster_season_weights %>%
  summarise(
    cluster_season_groups = n(),
    groups_with_multiple_weights =
      sum(number_of_weights > 1),
    groups_with_one_weight =
      sum(number_of_weights == 1)
  ) %>%
  print()

# Compare the same clusters across seasons
cluster_season_weights %>%
  select(clust, Season, minimum_weight) %>%
  tidyr::pivot_wider(
    names_from = Season,
    values_from = minimum_weight,
    names_prefix = "season_"
  ) %>%
  print(n = 20)


library(dplyr)

# Keep only Season A crop productivity records
seasonA_productivity <- selected_crop_productivity %>%
  filter(as.numeric(Season) == 1)

# Keep Season A crop records for the adjustment variables
seasonA_covariates <- crop_analysis %>%
  filter(as.numeric(Season) == 1) %>%
  group_by(hhid, crop_name) %>%
  summarise(
    district = first(district),
    proportion_organic_fertilizer =
      mean(organic_fertilizer, na.rm = TRUE),
    proportion_inorganic_fertilizer =
      mean(inorganic_fertilizer, na.rm = TRUE),
    proportion_irrigated =
      mean(irrigation_status == "Yes", na.rm = TRUE),
    .groups = "drop"
  )

# Summarise productivity per household and crop
seasonA_household_yield <- seasonA_productivity %>%
  group_by(hhid, crop_name) %>%
  summarise(
    household_median_kg_ha =
      median(productivity_kg_ha, na.rm = TRUE),
    plot_season_records = n(),
    .groups = "drop"
  )

# Attach extension access and Season A survey information
seasonA_analysis <- seasonA_household_yield %>%
  left_join(
    extension_household %>%
      select(hhid, extension_access),
    by = "hhid"
  ) %>%
  left_join(
    seasonA_covariates,
    by = c("hhid", "crop_name")
  ) %>%
  left_join(
    crop %>%
      filter(as.numeric(Season) == 1) %>%
      distinct(hhid, clust, weight),
    by = "hhid"
  )

# Check the final dataset
seasonA_analysis %>%
  summarise(
    rows = n(),
    households = n_distinct(hhid),
    missing_extension = sum(is.na(extension_access)),
    missing_cluster = sum(is.na(clust)),
    missing_weight = sum(is.na(weight)),
    nonpositive_weights = sum(weight <= 0, na.rm = TRUE)
  ) %>%
  print()

# Check whether a household appears more than once per crop
seasonA_analysis %>%
  count(hhid, crop_name) %>%
  filter(n > 1) %>%
  print()


library(dplyr)

# Check the number of households per crop and access group
seasonA_analysis %>%
  group_by(crop_name, extension_access) %>%
  summarise(
    households = n_distinct(hhid),
    median_yield = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  print()

# Check missing or undefined model variables
seasonA_analysis %>%
  summarise(
    missing_yield = sum(is.na(household_median_kg_ha)),
    missing_district = sum(is.na(district)),
    missing_organic = sum(
      is.na(proportion_organic_fertilizer) |
        !is.finite(proportion_organic_fertilizer)
    ),
    missing_inorganic = sum(
      is.na(proportion_inorganic_fertilizer) |
        !is.finite(proportion_inorganic_fertilizer)
    ),
    missing_irrigation = sum(
      is.na(proportion_irrigated) |
        !is.finite(proportion_irrigated)
    ),
    missing_extension = sum(is.na(extension_access))
  ) %>%
  print()

# Check how many observations are available for each crop
seasonA_analysis %>%
  count(crop_name, name = "household_crop_records") %>%
  print()




library(dplyr)
install.packages("survey")
library(survey)

# Prepare variables for modeling
seasonA_analysis <- seasonA_analysis %>%
  mutate(
    district = factor(district),
    extension_access = as.numeric(extension_access)
  )

# Fit one weighted, cluster-adjusted model per crop
crop_names <- unique(seasonA_analysis$crop_name)

model_results <- list()

for (crop in crop_names) {
  
  dat <- seasonA_analysis %>%
    filter(crop_name == crop) %>%
    droplevels()
  
  # Start with the main explanatory variable and adjustments
  predictors <- c(
    "extension_access",
    "district",
    "proportion_organic_fertilizer",
    "proportion_inorganic_fertilizer"
  )
  
  # Include irrigation only if it varies in this crop's data
  if (dplyr::n_distinct(dat$proportion_irrigated) > 1) {
    predictors <- c(predictors, "proportion_irrigated")
  }
  
  # Build the model formula
  model_formula <- as.formula(
    paste(
      "log1p(household_median_kg_ha) ~",
      paste(predictors, collapse = " + ")
    )
  )
  
  # Survey design: Season A weights and cluster identifier
  design <- svydesign(
    ids = ~clust,
    weights = ~weight,
    data = dat,
    nest = TRUE
  )
  
  # Fit model
  fit <- svyglm(
    model_formula,
    design = design,
    family = gaussian()
  )
  
  model_results[[crop]] <- fit
  
  cat("\n-----------------------------\n")
  cat("CROP:", crop, "\n")
  cat("-----------------------------\n")
  print(summary(fit))
}


# Inspect the cassava model's district distribution
cassava_data <- seasonA_analysis %>%
  filter(crop_name == "Cassava")

cassava_data %>%
  group_by(district) %>%
  summarise(
    households = n_distinct(hhid),
    clusters = n_distinct(clust),
    extension_groups = n_distinct(extension_access),
    access_yes = sum(extension_access == 1),
    access_no = sum(extension_access == 0),
    .groups = "drop"
  ) %>%
  arrange(households) %>%
  print(n = Inf)

# Check the number of clusters in each crop model
seasonA_analysis %>%
  group_by(crop_name) %>%
  summarise(
    households = n_distinct(hhid),
    clusters = n_distinct(clust),
    .groups = "drop"
  ) %>%
  print()

# Obtain confidence intervals for extension access
for (crop in names(model_results)) {
  cat("\nCROP:", crop, "\n")
  print(confint(model_results[[crop]]))
}


class(crop)
class(survey)
names(survey)


library(dplyr)
library(survey)

# Retrieve the actual crop dataset from the survey list
crop_data <- survey[[
  "AHS2024_Section3_4_CROP GROWN, SEEDS AND PRODUCTION & AGRICULTURAL INPUTS AND PRACTICES"
]]

# Check that crop_data is a data frame
class(crop_data)

# Get each household's province from Season A
province_lookup <- crop_data %>%
  filter(as.numeric(Season) == 1) %>%
  distinct(hhid, province)

# Check the result
print(head(province_lookup))
nrow(province_lookup)


seasonA_province <- seasonA_analysis %>%
  select(-any_of("province")) %>%
  left_join(province_lookup, by = "hhid") %>%
  mutate(
    province = factor(province),
    extension_access = as.numeric(extension_access)
  )

# Check the dataset
dim(seasonA_province)
table(seasonA_province$province, useNA = "ifany")



province_models <- list()

for (crop_name_i in unique(seasonA_province$crop_name)) {
  
  dat <- seasonA_province %>%
    filter(crop_name == crop_name_i) %>%
    droplevels()
  
  predictors <- c(
    "extension_access",
    "province",
    "proportion_organic_fertilizer",
    "proportion_inorganic_fertilizer"
  )
  
  if (dplyr::n_distinct(dat$proportion_irrigated) > 1) {
    predictors <- c(predictors, "proportion_irrigated")
  }
  
  model_formula <- as.formula(
    paste(
      "log1p(household_median_kg_ha) ~",
      paste(predictors, collapse = " + ")
    )
  )
  
  design <- svydesign(
    ids = ~clust,
    weights = ~weight,
    data = dat,
    nest = TRUE
  )
  
  fit <- svyglm(
    model_formula,
    design = design,
    family = gaussian()
  )
  
  province_models[[crop_name_i]] <- fit
  
  cat("\nCROP:", crop_name_i, "\n")
  
  # Extension-access estimate, standard error and p-value
  print(
    coef(summary(fit))[
      "extension_access", ,
      drop = FALSE
    ]
  )
  
  # 95% confidence interval
  print(confint(fit)["extension_access", ])
}


### Preliminary Results

#Using Season A data from the Rwanda Agricultural Household Survey 2024, we examined the association between access to agricultural extension services and the productivity of maize, cassava and sweet potato. In province-adjusted, weighted regression models, extension access was positively associated with the transformed productivity measure for all three crops. The estimated differences were approximately 9.6% for maize, 11.9% for cassava and 5.1% for sweet potato on the model's transformed scale. However, none of these associations was statistically significant at the 5% level. These findings therefore do not provide sufficient statistical evidence of an adjusted association between extension access and productivity for the selected crops. The results should be interpreted cautiously because the analysis is observational and the survey-design specification still requires verification. No causal conclusions can be drawn from these models.







library(dplyr)

# 1. Check the dataset dimensions and variables
dim(seasonA_province)
names(seasonA_province)

# 2. Check the number of observations and households
seasonA_province %>%
  summarise(
    observations = n(),
    households = n_distinct(hhid),
    clusters = n_distinct(clust),
    missing_extension = sum(is.na(extension_access)),
    missing_yield = sum(is.na(household_median_kg_ha)),
    missing_province = sum(is.na(province)),
    missing_weight = sum(is.na(weight)),
    nonpositive_weights = sum(weight <= 0, na.rm = TRUE)
  )

# 3. Check observations for each crop and access group
seasonA_province %>%
  group_by(crop_name, extension_access) %>%
  summarise(
    observations = n(),
    households = n_distinct(hhid),
    median_yield = median(
      household_median_kg_ha,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# 4. Check whether households have duplicate crop records
seasonA_province %>%
  count(hhid, crop_name) %>%
  filter(n > 1)



# Confirm the original crop dataset
crop_data <- survey[[
  "AHS2024_Section3_4_CROP GROWN, SEEDS AND PRODUCTION & AGRICULTURAL INPUTS AND PRACTICES"
]]

# 1. Check the units recorded in the questionnaire labels
labels_to_check <- c(
  "s2q2",
  "s3_q20_1",
  "s3_q20_2",
  "s3_q20_3",
  "s3_q20_4",
  "s3_q20_5",
  "s3_q20_6",
  "s3_q20_7"
)

data.frame(
  variable = labels_to_check,
  label = sapply(labels_to_check, function(v) {
    lab <- attr(crop_data[[v]], "label")
    if (is.null(lab)) "" else as.character(lab)
  })
)

# 2. Check weights and plot areas
summary(seasonA_province$weight)

summary(crop_data$s2q2[
  as.numeric(crop_data$Season) == 1
])

# 3. Inspect the largest selected-crop yields
selected_crop_productivity %>%
  filter(as.numeric(Season) == 1) %>%
  arrange(desc(productivity_kg_ha)) %>%
  select(
    hhid, Season, plot, area_sqm,
    crop_name, harvest_kg, productivity_kg_ha
  ) %>%
  head(15)

# 4. Verify nonpositive weights
sum(
  is.na(seasonA_province$weight) |
    seasonA_province$weight <= 0
)




# Inspect the 15 highest yields in Season A
selected_crop_productivity %>%
  filter(as.numeric(Season) == 1) %>%
  arrange(desc(productivity_kg_ha)) %>%
  select(
    hhid,
    Season,
    plot,
    area_sqm,
    crop_name,
    harvest_kg,
    productivity_kg_ha
  ) %>%
  head(15)

# Check for missing or nonpositive weights
sum(
  is.na(seasonA_province$weight) |
    seasonA_province$weight <= 0
)

# Count very small plot areas in Season A
sum(
  as.numeric(crop_data$Season) == 1 &
    !is.na(crop_data$s2q2) &
    crop_data$s2q2 < 20
)

# Count zero and negative areas in Season A
sum(
  as.numeric(crop_data$Season) == 1 &
    !is.na(crop_data$s2q2) &
    crop_data$s2q2 <= 0
)



# Inspect the original household and plot record
crop_data %>%
  filter(
    hhid == 305771,
    as.numeric(Season) == 1,
    s2q1 == 9
  ) %>%
  select(
    hhid, Season, s2q1, s2q2,
    matches("^s3_q4_[1-7]$"),
    matches("^s3_q20_[1-7]$")
  ) %>%
  print(width = Inf)

# Inspect all Season A plots with area below 20 m²
crop_data %>%
  filter(
    as.numeric(Season) == 1,
    !is.na(s2q2),
    s2q2 < 20
  ) %>%
  select(
    hhid, Season, s2q1, s2q2,
    matches("^s3_q4_[1-7]$"),
    matches("^s3_q20_[1-7]$")
  ) %>%
  print(n = Inf, width = Inf)



library(dplyr)
library(survey)

# Sensitivity dataset: exclude the flagged cassava observation
# only for this diagnostic comparison
sensitivity_data <- seasonA_province %>%
  filter(!(crop_name == "Cassava" & hhid == 305771))

sensitivity_models <- list()

for (crop_name_i in unique(sensitivity_data$crop_name)) {
  
  dat <- sensitivity_data %>%
    filter(crop_name == crop_name_i) %>%
    droplevels()
  
  predictors <- c(
    "extension_access",
    "province",
    "proportion_organic_fertilizer",
    "proportion_inorganic_fertilizer"
  )
  
  if (dplyr::n_distinct(dat$proportion_irrigated) > 1) {
    predictors <- c(predictors, "proportion_irrigated")
  }
  
  model_formula <- as.formula(
    paste(
      "log1p(household_median_kg_ha) ~",
      paste(predictors, collapse = " + ")
    )
  )
  
  design <- svydesign(
    ids = ~clust,
    weights = ~weight,
    data = dat,
    nest = TRUE
  )
  
  fit <- svyglm(
    model_formula,
    design = design,
    family = gaussian()
  )
  
  sensitivity_models[[crop_name_i]] <- fit
  
  cat("\nCROP:", crop_name_i, "\n")
  print(
    coef(summary(fit))[
      "extension_access", ,
      drop = FALSE
    ]
  )
  print(confint(fit)["extension_access", ])
}



# Check whether each household has one weight in Season A
weight_check <- crop_data %>%
  filter(as.numeric(Season) == 1) %>%
  group_by(hhid) %>%
  summarise(
    number_of_weights = n_distinct(weight[!is.na(weight)]),
    .groups = "drop"
  )

table(weight_check$number_of_weights)

# Check how many clusters are represented in each province
seasonA_province %>%
  group_by(province) %>%
  summarise(
    households = n_distinct(hhid),
    clusters = n_distinct(clust),
    .groups = "drop"
  )

# Check the survey-weight range in each crop model
seasonA_province %>%
  group_by(crop_name) %>%
  summarise(
    observations = n(),
    clusters = n_distinct(clust),
    minimum_weight = min(weight),
    median_weight = median(weight),
    maximum_weight = max(weight),
    .groups = "drop"
  )

# Inspect the labels of the cluster and weight variables
attr(crop_data$clust, "label")
attr(crop_data$weight, "label")



# Check the survey documentation available in your files
names(survey)

# Check labels and metadata for key design variables
design_variables <- c(
  "hhid",
  "clust",
  "Cluster_ID",
  "weight",
  "province",
  "district"
)

design_metadata <- lapply(
  design_variables,
  function(v) {
    x <- crop_data[[v]]
    
    list(
      variable = v,
      label = attr(x, "label"),
      class = class(x),
      missing = sum(is.na(x)),
      distinct_values = dplyr::n_distinct(x, na.rm = TRUE)
    )
  }
)

design_metadata



library(dplyr)
library(survey)

# Use the province-adjusted Season A dataset.
# Exclude the flagged cassava observation for this
# sensitivity check, as in your previous analysis.
design_sensitivity_data <- seasonA_province %>%
  filter(!(crop_name == "Cassava" & hhid == 305771)) %>%
  mutate(
    district = factor(district),
    province = factor(province)
  )

compare_designs <- list()

for (crop_i in unique(design_sensitivity_data$crop_name)) {
  
  dat <- design_sensitivity_data %>%
    filter(crop_name == crop_i) %>%
    droplevels()
  
  predictors <- c(
    "extension_access",
    "province",
    "proportion_organic_fertilizer",
    "proportion_inorganic_fertilizer"
  )
  
  if (n_distinct(dat$proportion_irrigated) > 1) {
    predictors <- c(predictors, "proportion_irrigated")
  }
  
  f <- as.formula(
    paste(
      "log1p(household_median_kg_ha) ~",
      paste(predictors, collapse = " + ")
    )
  )
  
  # Specification 1: clusters and weights
  design1 <- svydesign(
    ids = ~clust,
    weights = ~weight,
    data = dat,
    nest = TRUE
  )
  
  fit1 <- svyglm(f, design = design1, family = gaussian())
  
  # Specification 2: clusters, weights and district strata
  design2 <- svydesign(
    ids = ~clust,
    strata = ~district,
    weights = ~weight,
    data = dat,
    nest = TRUE
  )
  
  fit2 <- svyglm(f, design = design2, family = gaussian())
  
  compare_designs[[crop_i]] <- data.frame(
    crop = crop_i,
    model = c("Cluster + weight", "District strata"),
    estimate = c(
      coef(fit1)["extension_access"],
      coef(fit2)["extension_access"]
    ),
    standard_error = c(
      sqrt(vcov(fit1)["extension_access",
                      "extension_access"]),
      sqrt(vcov(fit2)["extension_access",
                      "extension_access"])
    ),
    p_value = c(
      coef(summary(fit1))["extension_access", "Pr(>|t|)"],
      coef(summary(fit2))["extension_access", "Pr(>|t|)"]
    )
  )
}

do.call(rbind, compare_designs)


library(ggplot2)
library(dplyr)
install.packages("scales")
library(scales)

# Prepare descriptive data
access_plot_data <- seasonA_province %>%
  group_by(crop_name) %>%
  summarise(
    access_percentage = mean(extension_access == 1) * 100,
    .groups = "drop"
  )


 

# Create the coefficient plot
p3 <- ggplot(
  results_plot,
  aes(x = estimate, y = crop_name)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    orientation = "y",
    width = 0.2
  ) +
  geom_point(size = 3) +
  labs(
    title = "Extension Access and Crop Productivity",
    subtitle = "Adjusted estimates with 95% confidence intervals",
    x = "Coefficient on log(1 + productivity)",
    y = "Crop"
  ) +
  theme_minimal()

# Display the chart
print(p3)

# Save the chart
ggsave(
  "figure3_regression_results.png",
  plot = p3,
  width = 9,
  height = 5,
  dpi = 300
)



library(dplyr)
library(survey)
library(ggplot2)

# Prepare the data
plot_data <- seasonA_province %>%
  mutate(
    extension_access = as.numeric(extension_access),
    crop_name = factor(
      crop_name,
      levels = c("Maize", "Cassava", "Sweet potato")
    )
  )

# Calculate weighted access percentages
weighted_access <- plot_data %>%
  group_by(crop_name) %>%
  group_modify(~ {
    design <- svydesign(
      ids = ~clust,
      weights = ~weight,
      data = .x,
      nest = TRUE
    )
    
    result <- svymean(~extension_access, design, na.rm = TRUE)
    
    data.frame(
      percentage = as.numeric(coef(result)[1]) * 100,
      standard_error = as.numeric(SE(result)[1]) * 100
    )
  }) %>%
  ungroup()

# Draw the chart
p1_weighted <- ggplot(
  weighted_access,
  aes(x = crop_name, y = percentage)
) +
  geom_col() +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    vjust = -0.5
  ) +
  labs(
    title = "Extension-Service Access by Crop",
    subtitle = "AHS 2024, Season A; weighted descriptive estimates",
    x = "Crop",
    y = "Estimated households with access (%)"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  theme_minimal(base_size = 12)

print(p1_weighted)

ggsave(
  "figure1_weighted_extension_access.png",
  p1_weighted,
  width = 8,
  height = 5,
  dpi = 300
)



p2_professional <- ggplot(
  plot_data,
  aes(
    x = factor(
      extension_access,
      levels = c(0, 1),
      labels = c("No access", "Has access")
    ),
    y = log1p(household_median_kg_ha)
  )
) +
  geom_boxplot() +
  facet_wrap(~crop_name, scales = "free_y") +
  labs(
    title = "Crop Productivity and Extension Access",
    subtitle = "AHS 2024, Season A; descriptive comparison",
    x = "Extension-service access",
    y = "Log(1 + household median yield in kg/ha)"
  ) +
  theme_minimal(base_size = 12)

print(p2_professional)

ggsave(
  "figure2_professional_productivity.png",
  p2_professional,
  width = 10,
  height = 6,
  dpi = 300
)

write.csv(
  seasonA_province,
  "seasonA_province.csv",
  row.names = FALSE
)


dir.create(
  "C:/Users/user/Desktop/AHS_Dashboard",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  seasonA_province,
  "C:/Users/user/Desktop/AHS_Dashboard/seasonA_province.csv",
  row.names = FALSE
)




library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

dat <- read.csv(
  "C:/Users/user/Desktop/AHS_Dashboard/seasonA_province.csv"
)

# Convert extension access into a consistent label
dat <- dat %>%
  mutate(
    extension_label = case_when(
      tolower(trimws(as.character(extension_access))) %in%
        c("1", "yes", "true") ~ "Yes",
      tolower(trimws(as.character(extension_access))) %in%
        c("0", "no", "false") ~ "No",
      TRUE ~ NA_character_
    )
  )

ui <- fluidPage(
  titlePanel(
    "Agricultural Extension and Crop Productivity in Rwanda"
  ),
  
  p(
    "NISR Agricultural Household Survey 2024 | Season A",
    style = "color: #555;"
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Dashboard filters"),
      
      selectInput(
        "crop",
        "Select crop",
        choices = c("All crops", sort(unique(dat$crop_name))),
        selected = "All crops"
      ),
      
      p("Use the dropdown to explore individual crops.")
    ),
    
    mainPanel(
      h3("Key indicators"),
      
      fluidRow(
        column(
          4,
          wellPanel(
            h5("Observations"),
            textOutput("n_obs")
          )
        ),
        column(
          4,
          wellPanel(
            h5("Unique households"),
            textOutput("n_households")
          )
        ),
        column(
          4,
          wellPanel(
            h5("Extension access"),
            textOutput("access_rate")
          )
        )
      ),
      
      h3("Crop productivity by extension access"),
      p(
        "The boxplot compares the distribution of reported ",
        "household crop yields for records with and without ",
        "extension access."
      ),
      
      plotOutput("yield_plot", height = "400px"),
      
      h3("Explore the data"),
      DTOutput("data_table"),
      
      hr(),
      
      h4("Interpretation note"),
      p(
        "These are descriptive comparisons. Differences in yield ",
        "do not establish that extension services caused a change ",
        "in productivity. Other household and farm characteristics ",
        "may contribute to observed differences."
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    if (input$crop == "All crops") {
      dat
    } else {
      dat %>% filter(crop_name == input$crop)
    }
  })
  
  output$n_obs <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$n_households <- renderText({
    format(
      dplyr::n_distinct(filtered_data()$hhid, na.rm = TRUE),
      big.mark = ","
    )
  })
  
  output$access_rate <- renderText({
    x <- filtered_data()$extension_label
    x <- x[!is.na(x)]
    
    if (length(x) == 0) {
      return("Not available")
    }
    
    paste0(round(100 * mean(x == "Yes"), 1), "%")
  })
  
  output$yield_plot <- renderPlot({
    plot_data <- filtered_data() %>%
      filter(
        !is.na(extension_label),
        !is.na(household_median_kg_ha),
        is.finite(household_median_kg_ha),
        household_median_kg_ha >= 0
      )
    
    validate(
      need(nrow(plot_data) > 0,
           "No usable yield records for this selection.")
    )
    
    ggplot(
      plot_data,
      aes(
        x = extension_label,
        y = household_median_kg_ha
      )
    ) +
      geom_boxplot(
        fill = "steelblue",
        alpha = 0.75,
        outlier.alpha = 0.3
      ) +
      labs(
        x = "Access to agricultural extension",
        y = "Household median yield (kg/ha)",
        title = "Yield distribution by extension access"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)
shiny::runApp("C:/Users/user/Desktop/AHS_Dashboard")

plotOutput("yield_plot", height = "400px")

h3("Crop productivity across provinces")
plotOutput("province_plot", height = "450px")


output$province_plot <- renderPlot({
  
  province_data <- filtered_data() %>%
    filter(
      !is.na(province),
      !is.na(household_median_kg_ha),
      is.finite(household_median_kg_ha),
      household_median_kg_ha >= 0
    ) %>%
    group_by(province) %>%
    summarise(
      median_yield = median(
        household_median_kg_ha,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  validate(
    need(
      nrow(province_data) > 0,
      "No usable province data for this selection."
    )
  )
  
  ggplot(
    province_data,
    aes(
      x = reorder(province, median_yield),
      y = median_yield
    )
  ) +
    geom_col(fill = "darkseagreen4") +
    coord_flip() +
    labs(
      title = "Median crop yield by province",
      x = "Province",
      y = "Median yield (kg/ha)"
    ) +
    theme_minimal(base_size = 13)
})

plotOutput("province_plot", height = "450px")


h3("Adjusted regression results")

p(
  "These provisional estimates examine the association between ",
  "extension access and log crop productivity, adjusting for ",
  "other variables in the fitted models."
)

tableOutput("regression_table")

p(
  "Interpretation: p-values above 0.05 indicate that the analysis ",
  "did not detect a statistically significant association at the ",
  "5% level. This does not prove that no association exists."
)


output$regression_table <- renderTable({
  
  data.frame(
    Crop = c("Maize", "Cassava", "Sweet potato"),
    Estimate = c(0.09202814, 0.06050673, 0.04946144),
    P_value = c(0.4189734, 0.7509757, 0.5769217),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Estimate = round(Estimate, 3),
      P_value = round(P_value, 3)
    )
})
names(seasonA_province)
summary(seasonA_province$household_median_kg_ha)

