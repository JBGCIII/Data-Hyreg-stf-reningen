##########################################################################################################
#                                         DATA SET CREATION FULLL
##########################################################################################################

# Load or install required packages
required_packages <- c("readxl", "dplyr", "purrr", "stringr")

installed <- required_packages %in% installed.packages()
if (any(!installed)) {
  install.packages(required_packages[!installed])
}
invisible(lapply(required_packages, library, character.only = TRUE))

# Create necessary directories
dir.create("Data/Avfall", recursive = TRUE, showWarnings = FALSE)
dir.create("Data/El", recursive = TRUE, showWarnings = FALSE)
dir.create("Data/Fjärrvärme", recursive = TRUE, showWarnings = FALSE)
dir.create("Data/Vatten och Avlopp", recursive = TRUE, showWarnings = FALSE)
dir.create("Data/Totalt", recursive = TRUE, showWarnings = FALSE)



##########################################################################################################
#                                         AVFALL
##########################################################################################################

files <- list.files("Raw_Data/Nils-holgersson/Avfall", full.names = TRUE)


data_list <- lapply(files, function(f) {
  df <- read_excel(f)

  # Keep only Län, Kommun, and Av/Avfall columns
  df <- df %>%
    select(matches("Län|Kommun|^Av"))
  
  # Standardize column names (remove spaces)
  names(df) <- gsub(" ", "_", names(df))
  
  # Normalize Län names
  df <- df %>%
    mutate(
      Län = str_squish(Län),
      Län = str_replace(Län, "s län$", ""),   # remove trailing "s län"
      Län = str_replace(Län, " län$", ""),    # remove trailing "län"
      Län = str_replace(Län, "s$", "")        # remove possessive 's'
    )
  
  # Normalize Kommun names (handle known cases)
  df <- df %>%
    mutate(
      Kommun = str_squish(Kommun),
      Kommun = str_replace(Kommun, "^Malung$", "Malung-Sälen"),
      Kommun = str_replace(Kommun, "^Gotland$", "Region Gotland")
      # Add more replacements here if needed
    )
  
  # Identify the newest year column in the file
  years <- str_extract(names(df), "\\d{4}")
  newest_year <- max(as.numeric(years), na.rm = TRUE)
  
  # Keep only the latest year's Avfall data
  year_col <- names(df)[grepl(newest_year, names(df))]
  df <- df %>%
    select(Län, Kommun, all_of(year_col))
  
  # Rename the Avfall column cleanly
  names(df)[3] <- paste0("Avfall_", newest_year)
  
  return(df)
})

# Merge all by Län and Kommun
merged <- reduce(data_list, full_join, by = c("Län", "Kommun"))

# Sort columns so years are in order
merged <- merged %>%
  select(Län, Kommun, sort(names(.)[!names(.) %in% c("Län", "Kommun")]))

# Save as CSV
write.csv(merged, "Data/Avfall/merged_avfall_2015_2024.csv", row.names = FALSE)



##########################################################################################################
#                                         VATTEN OCH AVLOPP
##########################################################################################################


files <- list.files("Raw_Data/Nils-holgersson/Vatten och Avlopp", full.names = TRUE)


data_list <- lapply(files, function(f) {
  df <- read_excel(f)

  # Keep only Län, Kommun, and Av/Avfall columns
  df <- df %>%
    select(matches("Län|Kommun|^Av"))
  
  # Standardize column names (remove spaces)
  names(df) <- gsub(" ", "_", names(df))
  
  # Normalize Län names
  df <- df %>%
    mutate(
      Län = str_squish(Län),
      Län = str_replace(Län, "s län$", ""),   # remove trailing "s län"
      Län = str_replace(Län, " län$", ""),    # remove trailing "län"
      Län = str_replace(Län, "s$", "")        # remove possessive 's'
    )
  
  # Normalize Kommun names (handle known cases)
  df <- df %>%
    mutate(
      Kommun = str_squish(Kommun),
      Kommun = str_replace(Kommun, "^Malung$", "Malung-Sälen"),
      Kommun = str_replace(Kommun, "^Gotland$", "Region Gotland")
      # Add more replacements here if needed
    )
  
  # Identify the newest year column in the file
  years <- str_extract(names(df), "\\d{4}")
  newest_year <- max(as.numeric(years), na.rm = TRUE)
  
  # Keep only the latest year's Avfall data
  year_col <- names(df)[grepl(newest_year, names(df))]
  df <- df %>%
    select(Län, Kommun, all_of(year_col))
  
  # Rename the Avfall column cleanly
  names(df)[3] <- paste0("VA", newest_year)
  
  return(df)
})

# Merge all by Län and Kommun
merged <- reduce(data_list, full_join, by = c("Län", "Kommun"))

# Sort columns so years are in order
merged <- merged %>%
  select(Län, Kommun, sort(names(.)[!names(.) %in% c("Län", "Kommun")]))

# Save as CSV
write.csv(merged, "Data/Vatten och Avlopp/merged_VA_2015_2024.csv", row.names = FALSE)





##########################################################################################################
#                                         EL
##########################################################################################################

files <- list.files("Raw_Data/Nils-holgersson/Vatten och Avlopp", full.names = TRUE)

# Loop through and print headers
for (f in files) {
  cat("\n----", basename(f), "----\n")
  print(names(read_excel(f, n_max = 0)))  # read only header row
}


library(readxl)
library(dplyr)
library(purrr)
library(stringr)

# 1️⃣ List all Excel files
files <- list.files("Raw_Data/Nils-holgersson/Vatten och Avlopp", full.names = TRUE)


va_list <- lapply(files, function(f) {
  df <- read_excel(f)
  
  # Ensure unique column names
  names(df) <- make.unique(names(df))
  
  # Standardize column names
  names(df) <- str_squish(names(df))
  names(df) <- gsub(" ", "_", names(df))
  names(df) <- gsub("Kommuner", "Kommun", names(df))
  
  # Detect year from filename
  file_year <- as.numeric(str_extract(basename(f), "\\d{4}"))
  
  # Find relevant columns
  va_col <- names(df)[grepl(paste0("VA[_ ]?", file_year), names(df))]
  price_col <- names(df)[grepl("kr/lgh", names(df), ignore.case = TRUE)]
  rank_col  <- names(df)[grepl("Rang", names(df), ignore.case = TRUE)]
  
  # Keep only relevant columns
  df <- df %>%
    select(Län, Kommun, all_of(va_col), all_of(price_col), all_of(rank_col))
  
  # Rename
  names(df)[3] <- paste0("VA_", file_year)
  if (length(price_col) > 0) names(df)[4] <- paste0("Pris_", file_year)
  if (length(rank_col) > 0) names(df)[5] <- paste0("Rang_", file_year)
  
  # Clean Län and Kommun names
  df <- df %>%
    mutate(
      Län = str_squish(Län),
      Län = str_replace(Län, "s län$", ""),
      Län = str_replace(Län, " län$", ""),
      Län = str_replace(Län, "s$", ""),
      Kommun = str_squish(Kommun),
      Kommun = str_replace(Kommun, "^Malung$", "Malung-Sälen"),
      Kommun = str_replace(Kommun, "^Gotland$", "Region Gotland")
    )
  
  return(df)
})

# Merge all
va_merged <- reduce(va_list, full_join, by = c("Län", "Kommun"))

# Order columns
va_merged <- va_merged %>%
  select(Län, Kommun, sort(names(.)[!names(.) %in% c("Län", "Kommun")]))


# 5️⃣ Save as CSV
write.csv(va_merged, "Data/VA/merged_VA_2016_2024.csv", row.names = FALSE)


# --- 💾 Save as CSV ---
write.csv(merged_va, "Data/Vatten och Avlopp/merged_VA_2015_2024.csv", row.names = FALSE)
##########################################################################################################
#                                         AVFALL
##########################################################################################################







##########################################################################################################
#                                         AVFALL
##########################################################################################################
