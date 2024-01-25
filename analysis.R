library(tidyverse)

# load relevant agencies
penalties <- read_csv("penalties_2017.csv")

# Vector with the paths to the CSV files
file_paths <- list.files(pattern = "budgets\\USG_spending_.*\\.csv", full.names = TRUE)

# Read all CSV files into a list of tibbles
list_of_tibbles <- lapply(file_paths, read_csv)

# Combine all tibbles into one big tibble row-wise
data <- bind_rows(list_of_tibbles) %>%
    mutate(year = substr(submission_period, 3, 6))

# Create a new column initialized with NAs
data$matched_agency <- NA_character_

# Loop through each unique agency name
for (agency in unique(penalties$federal_agency)) {
  # Find the rows in data where federal_account_name contains the agency name
  matches <- str_detect(data$federal_account_name, fixed(agency))
  # Update the matched_agency column with the agency name where a match is found
  data$matched_agency[matches] <- agency
}

data$matched_agency[is.na(data$matched_agency)] <- data$owning_agency_name[is.na(data$matched_agency)]

# Sum the 'total_budgetary_resources' for each 'federal_account_name' grouped by 'submission_period'
summarized_budget <- data %>%
  group_by(year, matched_agency) %>%
  summarize(budgetary_resources = sum(total_budgetary_resources, na.rm = TRUE),
    obligations = sum(obligations_incurred, na.rm = TRUE))

# For each 'federal agency' in 'penalties', sum the 'total_budget' from 'summarized_budget'
penalties_with_budget <- penalties %>%
  left_join(summarized_budget, by = c("federal_agency" = "matched_agency")) %>%
  pivot_wider(names_from = year, values_from = c(budgetary_resources, obligations)) %>%
  select(-c(budgetary_resources_NA, obligations_NA))

write.csv(penalties_with_budget, "outputs/penalties_with_budget.csv", row.names = FALSE)


### Employment data

# Base path of the folders
base_path <- "C:/Users/User/Downloads/fedscope"

# Function to process files for a single year
process_year_files <- function(year) {
  year_folder <- file.path(base_path, as.character(year))

  # Construct the file paths
  codes_file_path <- file.path(year_folder, "DTagy.txt")
  data_file_path <- file.path(year_folder, paste0("FACTDATA_SEP", year, ".txt"))

  # Read and process files
  codes <- read_csv(codes_file_path, show_col_types = FALSE) %>%
    distinct(AGYSUB, .keep_all = TRUE)

  data <- read_csv(data_file_path, show_col_types = FALSE) %>%
    group_by(AGYSUB, OCC) %>%
    summarize(staff = n(), .groups = 'drop') %>%
    left_join(codes, by = "AGYSUB") %>%
    select(AGYT, AGYSUBT, OCC, staff) %>%
    mutate(year = year)

  return(data)
}

# Apply the function to each year and combine results
all_data <- map_dfr(2000:2021, process_year_files)
write.csv(all_data, "outputs/fedscope_full_data.csv", row.names = FALSE)

penalties_2000 <- read_csv("penalties_2000.csv", show_col_types = FALSE)

# Create a new column initialized with NAs
all_data$matched_agency <- NA_character_

# Loop through each unique agency name
for (agency in unique(penalties_2000$agency)) {
  # Find the rows in data where federal_account_name contains the agency name
  matches <- str_detect(all_data$AGYSUBT, fixed(agency, ignore_case = TRUE))
  # Update the matched_agency column with the agency name where a match is found
  all_data$matched_agency[matches] <- agency
}

all_data$matched_agency[is.na(all_data$matched_agency)] <- all_data$AGYT[is.na(all_data$matched_agency)]

# enforcement employment codes
enforcement_codes <- read_csv("enforcement.csv", show_col_types = FALSE) %>%
  filter(!str_detect(descriptor, fixed("dubious"))) %>%
  pull(code)

# Sum the 'total_budgetary_resources' for each 'federal_account_name' grouped by 'submission_period'
employees <- all_data %>%
  group_by(year, matched_agency) %>%
  summarize(total_employees = sum(staff, na.rm = TRUE),
    enforcement_employees = sum(if_else(OCC %in% enforcement_codes, staff, 0), na.rm = TRUE))

# For each 'federal agency' in 'penalties', sum the 'total_budget' from 'summarized_budget'
penalties_with_employees <- penalties_2000 %>%
  left_join(employees, by = c("agency" = "matched_agency"))

penalties_with_employees_summary <- penalties_with_employees %>%
  group_by(agency) %>%
  summarize(across(c(total_employees, enforcement_employees, penalties_dollars, penalties_number), ~ mean(., na.rm = TRUE)))

write.csv(penalties_with_employees, "outputs/penalties_with_employees.csv", row.names = FALSE)
write.csv(penalties_with_employees_summary, "outputs/penalties_with_employees_summary.csv", row.names = FALSE)

epa <- all_data %>%
    filter(matched_agency == "Environmental Protection Agency", year == 2021) %>%
    arrange(desc(staff)) %>%
    select(matched_agency, year, OCC, staff) %>%
    mutate(enforecement = ifelse(OCC %in% enforcement_codes, TRUE, FALSE))
write.csv(epa, "outputs/epa.csv", row.names = FALSE)
