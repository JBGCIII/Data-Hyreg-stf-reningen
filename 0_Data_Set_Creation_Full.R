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