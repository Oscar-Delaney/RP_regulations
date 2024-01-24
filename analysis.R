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
