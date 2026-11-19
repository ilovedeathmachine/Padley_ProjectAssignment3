library(tidyverse)
library(janitor)


# Import data
bats_raw <- read_csv(
  "Raw Data/WNS_BATS_RAW_7NOV2026.csv",
  show_col_types = FALSE
)

# View dataset
View(bats_raw)

# Inspect structure
glimpse(bats_raw)

# Count rows and columns
dim(bats_raw)

# Number of missing values in each column
colSums(is.na(bats_raw))

# Percentage missing per column
round(
  colMeans(is.na(bats_raw)) * 100,
  2
)

# CHeck for duplicated records
sum(duplicated(bats_raw))

# Check repeated site-year combinations
bats_raw %>%
  count(site, wyear) %>%
  filter(n > 1)

# Summarize numerical variables
summary(select(bats_raw, where(is.numeric)))

# Check negative bat counts
sum(bats_raw$count < 0, na.rm = TRUE)

# Create a separate dataset
bats_clean <- bats_raw %>%
  clean_names()

# Inspect standardized column names
names(bats_clean)

# Standardize county names
bats_clean <- bats_clean %>%
  mutate(
    state = str_to_upper(str_trim(state)),
    county = str_to_title(str_trim(county)),
    epi_phase = str_trim(epi_phase),
    site_status = str_trim(site_status)
  )

bats_clean <- bats_clean %>%
  mutate(
    temp_missing = is.na(
      mean_early_temp_global_celsius
    ),
    vpd_missing = is.na(
      log_early_vpd_global_kilopascals
    ),
    lambda_missing = is.na(lambda)
  )

bats_clean %>%
  group_by(epi_phase) %>%
  summarise(
    observations = n(),
    temp_missing_pct = mean(temp_missing) * 100,
                            vpd_missing_pct = mean(vpd_missing) * 100,
                            .groups = "drop"
  )


# Reusable outlier detection function
flag_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  
  !is.na(x) & (x < lower | x > upper)
}

# Add outlier flags without removing records
bats_clean <- bats_clean %>%
  mutate(
    count_outlier = flag_iqr(count),
    lambda_outlier = flag_iqr(lambda)
  )

# Count flagged observations
bats_clean %>%
  summarise(
    count_outliers = sum(count_outlier),
    lambda_outliers = sum(lambda_outlier)
  )


# Check for invalid values
bats_clean %>%
  summarise(
    negative_counts = sum(count < 0, na.rm = TRUE),
    negative_colony_sizes = sum(
      pre_wns_colony_size < 0, na.rm = TRUE
    ),
    invalid_lambda = sum(
      lambda < 0, na.rm = TRUE
    ),
    missing_site_ids = sum(is.na(site))
  )


# Compare raw and cleaned record counts
nrow(bats_raw)
nrow(bats_clean)

# Confirm all original site-year records remain
stopifnot(
  nrow(bats_raw) == nrow(bats_clean)
)

stopifnot(
  identical(bats_raw$site, bats_clean$site),
  identical(bats_raw$wyear, bats_clean$wyear)
)


write_csv(
  bats_clean,
  "WNS_BATS_CLEANED_18NOV2026.csv",
  na = ""
)


bats_exported <- read_csv(
  "WNS_BATS_CLEANED_18NOV2026.csv",
  show_col_types = FALSE
)

dim(bats_exported)
