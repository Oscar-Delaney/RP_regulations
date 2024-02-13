library(tidyverse)
library(quantmod)

# load relevant agencies
penalties <- read_csv("penalties_2017.csv")

# Vector with the paths to the CSV files
file_paths <- list.files(path = "budgets", pattern = "^USG_spending_.*\\.csv", full.names = TRUE)

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


### Penalties data

# Vector with the paths to the CSV files
file_paths <- list.files(path = "penalties_data", pattern = "^export.*\\.csv$", full.names = TRUE)

# Read all CSV files into a list of tibbles
list_of_tibbles <- lapply(file_paths, function(x) {
    read_csv(x, show_col_types = FALSE) %>%
    select(Company, Parent = `Current Parent Company`, Penalty = `Penalty Amount`, 
           Year = `Penalty Year`, Date = `Penalty Date`, Agency)
})


# Combine all tibbles into one big tibble row-wise
data <- bind_rows(list_of_tibbles)
data$Penalty <- as.numeric(gsub("\\$", "", gsub(",", "", data$Penalty)))
data$Date <- as.Date(as.character(data$Date), format = "%Y%m%d")
write.csv(data, "outputs/all_penalties_data.csv", row.names = FALSE)
data <- na.omit(data)

# load relevant companies (data as of 24th January 2024)
sp_data <- read_csv("S&P.csv", show_col_types = FALSE)

# Function to match company names progressively
progressive_match <- function(data_name, sp_name) {
  data_name_lower <- tolower(data_name)
  sp_name_lower <- tolower(sp_name)
  
  # Split names into words
  data_words <- str_split(data_name_lower, "\\s+")
  sp_words <- str_split(sp_name_lower, "\\s+")
  
  # Find matches based on progressive words
  for (i in 1:min(lengths(data_words), lengths(sp_words))) {
    match <- data_name_lower[startsWith(data_name_lower, sp_words[[1]][1:i])]
    if (length(match) > 0) {
      return(match[1]) # Return the first match
    }
  }
  
  return(NA) # No match found
}

# Apply the progressive matching function
sp_data$Matching_Parent <- sapply(sp_data$Name, function(name) {
  progressive_match(data$Parent, name)
})

# Check results, and edit manually, I couldn't think of a better way :(
sp_data
sp_data$Matching_Parent[sp_data$Name == "JOHNSON + JOHNSON"] <- "Johnson & Johnson"
sp_data$Matching_Parent[sp_data$Name == "MERCK + CO. INC."] <- "Merck"
sp_data$Matching_Parent[sp_data$Name == "BANK OF AMERICA CORP"] <- "Bank Of America"
sp_data$Matching_Parent[sp_data$Name == "US DOLLAR"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "TEXAS INSTRUMENTS INC"] <- "Texa Instruments"
sp_data$Matching_Parent[sp_data$Name == "APPLIED MATERIALS INC"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "UNITED PARCEL SERVICE CL B"] <- "United Parcel Service"
sp_data$Matching_Parent[sp_data$Name == "AMERICAN EXPRESS CO"] <- "American Express"
sp_data$Matching_Parent[sp_data$Name == "AMERICAN TOWER CORP"] <- "American Tower"
sp_data$Matching_Parent[sp_data$Name == "THE CIGNA GROUP"] <- "Cigna"
sp_data$Matching_Parent[sp_data$Name == "BOSTON SCIENTIFIC CORP"] <- "Boston Scientific"
sp_data$Matching_Parent[sp_data$Name == "T MOBILE US INC"] <- "T-Mobile US"
sp_data$Matching_Parent[sp_data$Name == "CADENCE DESIGN SYS INC"] <- "Cadence Design Systems"
sp_data$Matching_Parent[sp_data$Name == "SOUTHERN CO/THE"] <- "Southern Company"
sp_data$Matching_Parent[sp_data$Name == "US BANCORP"] <- "U.S. Bancorp"
sp_data$Matching_Parent[sp_data$Name == "O REILLY AUTOMOTIVE INC"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "PARKER HANNIFIN CORP"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "GENERAL DYNAMICS CORP"] <- "General Dynamics"
sp_data$Matching_Parent[sp_data$Name == "AIR PRODUCTS + CHEMICALS INC"] <- "Air Products & Chemicals"
sp_data$Matching_Parent[sp_data$Name == "ARTHUR J GALLAGHER + CO"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "CAPITAL ONE FINANCIAL CORP"] <- "Capital One Financial"
sp_data$Matching_Parent[sp_data$Name == "GENERAL MOTORS CO"] <- "General Motors"
sp_data$Matching_Parent[sp_data$Name == "CROWN CASTLE INC"] <- "Crown Castle"
sp_data$Matching_Parent[sp_data$Name == "PUBLIC STORAGE"] <- "Public Storage"
sp_data$Matching_Parent[sp_data$Name == "TE CONNECTIVITY LTD"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "BANK OF NEW YORK MELLON CORP"] <- "Bank of New York Mellon"
sp_data$Matching_Parent[sp_data$Name == "DR HORTON INC"] <- "Dr Horton"
sp_data$Matching_Parent[sp_data$Name == "AMERICAN ELECTRIC POWER"] <- "American Electric Power"
sp_data$Matching_Parent[sp_data$Name == "UNITED RENTALS INC"] <- "United Rentals"
sp_data$Matching_Parent[sp_data$Name == "WW GRAINGER INC"] <- "WW Grainger"
sp_data$Matching_Parent[sp_data$Name == "GENERAL MILLS INC"] <- "General Mills"
sp_data$Matching_Parent[sp_data$Name == "CONSTELLATION ENERGY"] <- "Constellation Energy"
sp_data$Matching_Parent[sp_data$Name == "FIDELITY NATIONAL INFO SERV"] <- "Fidelity National Information Services"
sp_data$Matching_Parent[sp_data$Name == "OLD DOMINION FREIGHT LINE"] <- "Old DOminion Freight Line"
sp_data$Matching_Parent[sp_data$Name == "P G + E CORP"] <- "PG&E Corp."
sp_data$Matching_Parent[sp_data$Name == "GLOBAL PAYMENTS INC"] <- "Global Payments"
sp_data$Matching_Parent[sp_data$Name == "ON SEMICONDUCTOR"] <- "On Semiconductor"
sp_data$Matching_Parent[sp_data$Name == "FAIR ISAAC CORP"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "ARCH CAPITAL GROUP LTD"] <- "Arch Capital"
sp_data$Matching_Parent[sp_data$Name == "ROYAL CARIBBEAN CRUISES LTD"] <- "Royal Caribbean Cruises"
sp_data$Matching_Parent[sp_data$Name == "GE HEALTHCARE TECHNOLOGY"] <- "GE HealthCare Technologies"
sp_data$Matching_Parent[sp_data$Name == "DOLLAR TREE INC"] <- "Dollar Tree"
sp_data$Matching_Parent[sp_data$Name == "TAKE TWO INTERACTIVE SOFTWRE"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "HARTFORD FINANCIAL SVCS GRP"] <- "Hartford Financial Services"
sp_data$Matching_Parent[sp_data$Name == "WEST PHARMACEUTICAL SERVICES"] <- "West Pharmaceutical Services"
sp_data$Matching_Parent[sp_data$Name == "T ROWE PRICE GROUP INC"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "CHURCH + DWIGHT CO INC"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "M + T BANK CORP"] <- "M&T Bank"
sp_data$Matching_Parent[sp_data$Name == "AMERICAN WATER WORKS CO INC"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "COOPER COS INC/THE"] <- "Cooper Companies"
sp_data$Matching_Parent[sp_data$Name == "HUNTINGTON BANCSHARES INC"] <- "Huntington Bancshares"
sp_data$Matching_Parent[sp_data$Name == "WESTERN DIGITAL CORP"] <- "Western Digital"
sp_data$Matching_Parent[sp_data$Name == "SOUTHWEST AIRLINES CO"] <- "Southwest Airlines"
sp_data$Matching_Parent[sp_data$Name == "BROWN + BROWN INC"] <- "Brown & Brown"
sp_data$Matching_Parent[sp_data$Name == "HUNT (JB) TRANSPRT SVCS INC"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "FIRST SOLAR INC"] <- "First Solar"
sp_data$Matching_Parent[sp_data$Name == "WR BERKLEY CORP"] <- "W.R. Berkley"
sp_data$Matching_Parent[sp_data$Name == "MID AMERICA APARTMENT COMM"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "STANLEY BLACK + DECKER INC"] <- "Stanley Black & Decker"
sp_data$Matching_Parent[sp_data$Name == "UNITED AIRLINES HOLDINGS INC"] <- "United Airlines Holdings"
sp_data$Matching_Parent[sp_data$Name == "GEN DIGITAL INC"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "JACK HENRY + ASSOCIATES INC"] <- "Jack Henry & Associates"
sp_data$Matching_Parent[sp_data$Name == "BIO TECHNE CORP"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "SCHWAB (CHARLES) CORP"] <- "Charles Schwab Corp."
sp_data$Matching_Parent[sp_data$Name == "CHARLES RIVER LABORATORIES"] <- "Charles River Laboratories"
sp_data$Matching_Parent[sp_data$Name == "REGENCY CENTERS CORP"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "BOSTON PROPERTIES INC"] <- "Boston Properties"
sp_data$Matching_Parent[sp_data$Name == "BROWN FORMAN CORP CLASS B"] <- "Brown-Forman"
sp_data$Matching_Parent[sp_data$Name == "SMITH (A.O.) CORP"] <- "A.O. Smith Corp."
sp_data$Matching_Parent[sp_data$Name == "AMERICAN AIRLINES GROUP INC"] <- "American Airlines"
sp_data$Matching_Parent[sp_data$Name == "WYNN RESORTS LTD"] <- "Wynn Resorts"
sp_data$Matching_Parent[sp_data$Name == "ROBERT HALF INC"] <- "Robert Half"
sp_data$Matching_Parent[sp_data$Name == "FOX CORP   CLASS A"] <- "Fox Corporation"
sp_data$Matching_Parent[sp_data$Name == "FEDERAL REALTY INVS TRUST"] <- NA_character_
sp_data$Matching_Parent[sp_data$Name == "BIO RAD LABORATORIES A"] <- "Bio-Rad Laboratories"
sp_data$Matching_Parent[sp_data$Name == "FOX CORP   CLASS B"] <- "Fox Corporation"

# Microsoft weight in S&P500 on Jan 25, 2024 was 7.32%
# MIcrosoft capitalisation was then $3.009T
sp_data$Market_cap <- sp_data$Weight / 7.32 * 3.009e12
sp_data <- sp_data %>%
  group_by(Matching_Parent) %>%
  summarize(Market_cap = sum(Market_cap, na.rm = TRUE), Ticker = head(Ticker, n = 1))

sp_data <- na.omit(sp_data)

sp_data$price <- NA
for (i in seq_len(nrow(sp_data))) {
  # Use tryCatch to handle errors gracefully
  tryCatch({
    # Fetch the symbol data
    getSymbols(sp_data$Ticker[i], src = "yahoo",
               from = as.Date("2024-01-24"),
               to = as.Date("2024-01-25"))
    
    # If successful, extract the closing price
    sp_data$price[i] <- as.numeric(tail(Cl(get(sp_data$Ticker[i])), n = 1))
  }, error = function(e) {
    # Handle errors, for example, by printing a message
    message("Error with ticker ", sp_data$Ticker[i], ": ", e$message)
  })
  
  # Print progress every 10 iterations
  if (i %% 10 == 0) {
    print(i)
  }
}

write.csv(sp_data, "outputs/S_and_P_processed.csv", row.names = FALSE)

sp_data$Matching_Parent <- tolower(sp_data$Matching_Parent)
data$Parent <- tolower(data$Parent)
data <- data %>%
  left_join(sp_data, by = c("Parent" = "Matching_Parent")) %>%
  drop_na(Ticker)

data$price_at_time <- NA
for (i in seq_len(nrow(data))) {
  # Use tryCatch to handle errors gracefully
  tryCatch({
    # Fetch the symbol data
    getSymbols(data$Ticker[i], src = "yahoo",
               from = data$Date[i] - 4,
               to = data$Date[i])
    
    # If successful, extract the closing price
    data$price_at_time[i] <- as.numeric(tail(Cl(get(data$Ticker[i])), n = 1))
  }, error = function(e) {
    # Handle errors, for example, by printing a message
    message("Error with ticker ", data$Ticker[i], ": ", e$message)
  })
  
  # Print progress every 10 iterations
  if (i %% 50 == 0) {
    print(i)
  }
}

data$Market_cap_at_time <- data$Market_cap * data$price_at_time / data$price
data$Penalty_scaled <- data$Penalty / data$Market_cap_at_time
data <- data %>%
  arrange(desc(Penalty_scaled))

write.csv(data, "outputs/penalties_per_market_cap.csv", row.names = FALSE)

summary_stats <- data %>%
  filter(Penalty_scaled > 1e-3) %>%
  group_by(Agency) %>%
  summarize(
    Fines_Above_Threshold = n(), # Number of fines above 0.1% of market cap
    Sum_Scaled_Penalty = sum(Penalty_scaled, na.rm = TRUE), # Sum of scaled penalties
    Sqrt_Sum_Sq_Scaled_Penalty = sqrt(sum(Penalty_scaled^2, na.rm = TRUE)), # Sqrt of sum of squares of scaled penalties
    Max_Scaled_Penalty = max(Penalty_scaled, na.rm = TRUE) # Maximum scaled penalty
  ) %>%
  arrange(desc(Sum_Scaled_Penalty))

summary_stats
write.csv(summary_stats, "outputs/penalties_per_market_cap_summary.csv", row.names = FALSE)
