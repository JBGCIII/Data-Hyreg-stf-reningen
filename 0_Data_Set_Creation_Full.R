##########################################################################################################
#                                         DATA SET CREATION FULLL
##########################################################################################################

# Load or install required packages
required_packages <- c("readxl", "dplyr", "lubridate", "zoo","xts" )

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