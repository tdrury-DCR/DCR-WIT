###############################  HEADER  ######################################
#  TITLE: ImportResEXOCalibrations.R
#  DESCRIPTION: This script will process/import reservoir EXO calibration data
#  AUTHOR(S): Travis Drury, Tayelor Gosselin
#  DATE LAST UPDATED: 2026-09-24
#  Last Update: Created file
############################################################################## .

# COMMENT OUT BELOW WHEN RUNNING FUNCTION IN SHINY
#
# library(tidyverse)
# library(stringr)
# library(odbc)
# library(DBI)
# library(lubridate)
# library(magrittr)
# library(readxl)
# library(DescTools)
# library(glue)

# # COMMENT OUT ABOVE CODE WHEN RUNNING IN SHINY!

######################################################################## .
###                          Process Data                           ####
######################################################################## .

PROCESS_DATA <- function(file, rawdatafolder, filename.db, probe = NULL, ImportTable, ImportFlagTable = NULL) { # Start the function - takes 1 input (File)

  # Eliminate Scientific notation in numerical fields
  options(scipen = 999)

  # Get the full path to the file
  path <- paste0(rawdatafolder, "/", file)

  # Read in the raw data export, and leave intact for debugging purposes
  sheet_names <- excel_sheets(path)
  last_sheetname <- tail(sheet_names, 1)
  secondlast_sheetname <- sheet_names[length(sheet_names) - 1]
  last_sheet <- ifelse(last_sheetname != "ESRI_MAPINFO_SHEET", last_sheetname, secondlast_sheetname)

  df_raw <- read_excel(path,
    sheet = last_sheet
  )

  # Create a working copy of raw data
  df <- df_raw

  # Replace all micro symbols with u
  colnames(df) <- colnames(df) %>% gsub("\U00B5", "u", .)
  df_cols <- colnames(df)
  df <- as.data.frame(lapply(df, function(y) gsub("\U00B5", "u", y)))
  colnames(df) <- df_cols
  # Call out specific parameters into their own df
  ## Dissolved Oxygen ####
  DO <- subset(df[1:43, 1:2])
  DO <- DO %>%
    filter(DO != "")
  DO$parameter <- c("Dissolved Oxygen (DO)")
  DO$datetime_start <- c(DO[2, 2])
  DO$datetime_end <- c(DO[3, 2])

  DO$datetime_start <- as.numeric(DO$datetime_start)
  DO$datetime_start <- convert_date(DO$datetime_start, type = "excel", fraction = TRUE)
  DO$datetime_end <- as.numeric(DO$datetime_end)
  DO$datetime_end <- convert_date(DO$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  DO$cal_point <- with(DO, ifelse(DO == "[Cal Point 1]", "Point 1", ""))

  DO$standard <- c(DO[20, 2])
  DO <- DO %>% dplyr::mutate(standard_unit = str_extract_all(standard, "\\%\\s[:upper:][:lower:][:lower:]"))
  DO$standard_unit <- as.character(DO$standard_unit)
  DO <- DO %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  DO$standard <- as.numeric(DO$standard)

  DO$precal_value <- c(DO[21, 2])
  DO <- DO %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "\\%\\s[:upper:][:lower:][:lower:]"))
  DO$precal_unit <- as.character(DO$precal_unit)
  DO <- DO %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  DO$precal_value <- as.numeric(DO$precal_value)

  DO$postcal_value <- c(DO[22, 2])
  DO <- DO %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "\\%\\s[:upper:][:lower:][:lower:]"))
  DO$postcal_unit <- as.character(DO$postcal_unit)
  DO <- DO %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  DO$postcal_value <- as.numeric(DO$postcal_value)

  DO$raw_value <- c(DO[23, 2])
  DO <- DO %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "\\%\\s[:upper:][:lower:][:lower:]"))
  DO$raw_value_unit <- as.character(DO$raw_value_unit)
  DO <- DO %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  DO$raw_value <- as.numeric(DO$raw_value)

  DO$temp <- c(DO[24, 2])
  DO <- DO %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  DO$temp_unit <- as.character(DO$temp_unit)
  DO <- DO %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  DO$temp <- as.numeric(DO$temp)

  DO$barometer <- c(DO[26, 2]) # handheld cals do NOT record this, gives out NA - is this an issue? *****
  DO <- DO %>% dplyr::mutate(barometer_unit = str_extract_all(barometer, "[:lower:][:lower:][:upper:][:lower:]"))
  DO$barometer_unit <- as.character(DO$barometer_unit)
  DO <- DO %>% dplyr::mutate(barometer = str_extract_all(barometer, "[:digit:]+\\.[:digit:]+"))
  DO$barometer <- as.numeric(DO$barometer)

  DO$stability <- as.character(c(DO[25, 2]))
  DO$qc_score <- as.character(c(DO[5, 2]))
  DO$staff <- as.character(c(DO[14, 2]))
  DO$sondeID <- as.character(ifelse(DO[8, 2] == "20F160656", "Exo 10",
    ifelse(DO[8, 2] == "20F160657", "Exo 11", "")
  ))

  ## select pertinent columns
  DO <- DO[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "barometer",
    "barometer_unit", "stability", "qc_score", "staff"
  )]

  ## remove duplicate rows
  DO <- unique(DO)
  DO <- DO %>%
    filter(cal_point != "")

  # Filter out blank rows
  DO <- DO %>% filter(!is.na(postcal_value))


  ## Specific Conductivity ####
  cond <- subset(df[1:43, 3:4])
  cond <- cond %>%
    filter(conductivity != "")
  cond$parameter <- c("Specific Conductivity (uS/cm)")
  cond$datetime_start <- c(cond[2, 2])
  cond$datetime_end <- c(cond[3, 2])

  cond$datetime_start <- as.numeric(cond$datetime_start)
  cond$datetime_start <- convert_date(cond$datetime_start, type = "excel", fraction = TRUE)
  cond$datetime_end <- as.numeric(cond$datetime_end)
  cond$datetime_end <- convert_date(cond$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  cond$cal_point <- with(cond, ifelse(conductivity == "[Cal Point 1]", "Point 1", ""))

  cond$standard <- c(cond[17, 2])
  cond <- cond %>% dplyr::mutate(standard_unit = str_extract_all(standard, "[:space:].[:upper:]\\/[:lower:][:lower:]"))
  cond$standard_unit <- as.character(cond$standard_unit)
  cond <- cond %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  cond$standard <- as.numeric(cond$standard)

  cond$precal_value <- c(cond[18, 2])
  cond <- cond %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "[:space:].[:upper:]\\/[:lower:][:lower:]"))
  cond$precal_unit <- as.character(cond$precal_unit)
  cond <- cond %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  cond$precal_value <- as.numeric(cond$precal_value)

  cond$postcal_value <- c(cond[19, 2])
  cond <- cond %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "[:space:].[:upper:]\\/[:lower:][:lower:]"))
  cond$postcal_unit <- as.character(cond$postcal_unit)
  cond <- cond %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  cond$postcal_value <- as.numeric(cond$postcal_value)

  cond$raw_value <- c(cond[20, 2])
  cond <- cond %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "[:space:].[:upper:]\\/[:lower:][:lower:]"))
  cond$raw_value_unit <- as.character(cond$raw_value_unit)
  cond <- cond %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  cond$raw_value <- as.numeric(cond$raw_value)

  cond$temp <- c(cond[21, 2])
  cond <- cond %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  cond$temp_unit <- as.character(cond$temp_unit)
  cond <- cond %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  cond$temp <- as.numeric(cond$temp)

  cond$stability <- as.character(c(cond[22, 2]))
  cond$qc_score <- as.character(c(cond[5, 2]))
  cond$staff <- as.character(c(cond[14, 2]))
  cond$sondeID <- as.character(ifelse(cond[8, 2] == "20F160656", "Exo 10",
    ifelse(cond[8, 2] == "20F160657", "Exo 11", "")
  ))

  ## select pertinent columns
  cond <- cond[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "stability", "qc_score",
    "staff"
  )]

  ## remove duplicate rows
  cond <- unique(cond)
  cond <- cond %>%
    filter(cal_point != "")

  # Filter out blank rows
  cond <- cond %>% filter(!is.na(postcal_value))


  ## Chlorophyll-A (ug/L) ####
  chlA <- subset(df[1:43, 5:6])
  chlA <- chlA %>%
    filter(`chlA(ug/L)` != "")
  chlA$parameter <- c("Chlorophyll-A (ug/L)")
  chlA$datetime_start <- c(chlA[2, 2])
  chlA$datetime_end <- c(chlA[3, 2])

  chlA$datetime_start <- as.numeric(chlA$datetime_start)
  chlA$datetime_start <- convert_date(chlA$datetime_start, type = "excel", fraction = TRUE)
  chlA$datetime_end <- as.numeric(chlA$datetime_end)
  chlA$datetime_end <- convert_date(chlA$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  # starts getting messy with multiple cal points
  chlA$cal_point <- with(chlA, ifelse(`chlA(ug/L)` == "[Cal Point 1]", "Point 1",
    ifelse(`chlA(ug/L)` == "[Cal Point 2]", "Point 2", "")
  ))


  chlA$standard <- with(chlA, ifelse(chlA$cal_point == "Point 1", chlA[16, 2],
    ifelse(chlA$cal_point == "Point 2", chlA[23, 2], "")
  ))
  chlA <- chlA %>% dplyr::mutate(standard_unit = str_extract_all(standard, "[:space:].[:lower:]\\/[:upper:]"))
  chlA$standard_unit <- as.character(chlA$standard_unit)
  chlA <- chlA %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  chlA$standard <- as.numeric(chlA$standard)


  chlA$precal_value <- with(chlA, ifelse(chlA$cal_point == "Point 1", chlA[17, 2],
    ifelse(chlA$cal_point == "Point 2", chlA[24, 2], "")
  ))
  chlA <- chlA %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "[:space:].[:lower:]\\/[:upper:]"))
  chlA$precal_unit <- as.character(chlA$precal_unit)
  chlA <- chlA %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  chlA$precal_value <- as.numeric(chlA$precal_value)


  chlA$postcal_value <- with(chlA, ifelse(chlA$cal_point == "Point 1", chlA[18, 2],
    ifelse(chlA$cal_point == "Point 2", chlA[25, 2], "")
  ))
  chlA <- chlA %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "[:space:].[:lower:]\\/[:upper:]"))
  chlA$postcal_unit <- as.character(chlA$postcal_unit)
  chlA <- chlA %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  chlA$postcal_value <- as.numeric(chlA$postcal_value)


  chlA$raw_value <- with(chlA, ifelse(chlA$cal_point == "Point 1", chlA[19, 2],
    ifelse(chlA$cal_point == "Point 2", chlA[26, 2], "")
  ))
  chlA <- chlA %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "[:space:].[:lower:]\\/[:upper:]"))
  chlA$raw_value_unit <- as.character(chlA$raw_value_unit)
  chlA <- chlA %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  chlA$raw_value <- as.numeric(chlA$raw_value)


  chlA$temp <- with(chlA, ifelse(chlA$cal_point == "Point 1", chlA[20, 2],
    ifelse(chlA$cal_point == "Point 2", chlA[27, 2], "")
  ))
  chlA <- chlA %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  chlA$temp_unit <- as.character(chlA$temp_unit)
  chlA <- chlA %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  chlA$temp <- as.numeric(chlA$temp)


  chlA$stability <- as.character(with(chlA, ifelse(chlA$cal_point == "Point 1", chlA[21, 2],
    ifelse(chlA$cal_point == "Point 2", chlA[28, 2], "")
  )))
  chlA$qc_score <- as.character(c(chlA[5, 2]))
  chlA$staff <- as.character(c(chlA[14, 2]))
  chlA$sondeID <- as.character(ifelse(chlA[8, 2] == "20F160656", "Exo 10",
    ifelse(chlA[8, 2] == "20F160657", "Exo 11", "")
  ))

  ## select pertinent columns
  chlA <- chlA[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "stability", "qc_score",
    "staff"
  )]

  ## remove duplicate rows
  chlA <- unique(chlA)
  chlA <- chlA %>%
    filter(cal_point != "")

  # Filter out blank rows
  chlA <- chlA %>% filter(!is.na(postcal_value))


  ## Phycocyanin (ug/L) ####
  phyco <- subset(df[1:43, 7:8])
  phyco <- phyco %>%
    filter(`phyco(ug/L)` != "")
  phyco$parameter <- c("Phycocyanin (ug/L)")
  phyco$datetime_start <- c(phyco[2, 2])
  phyco$datetime_end <- c(phyco[3, 2])

  phyco$datetime_start <- as.numeric(phyco$datetime_start)
  phyco$datetime_start <- convert_date(phyco$datetime_start, type = "excel", fraction = TRUE)
  phyco$datetime_end <- as.numeric(phyco$datetime_end)
  phyco$datetime_end <- convert_date(phyco$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  # starts getting messy with multiple cal points
  phyco$cal_point <- with(phyco, ifelse(`phyco(ug/L)` == "[Cal Point 1]", "Point 1",
    ifelse(`phyco(ug/L)` == "[Cal Point 2]", "Point 2", "")
  ))


  phyco$standard <- with(phyco, ifelse(phyco$cal_point == "Point 1", phyco[16, 2],
    ifelse(phyco$cal_point == "Point 2", phyco[23, 2], "")
  ))
  phyco <- phyco %>% dplyr::mutate(standard_unit = str_extract_all(standard, "[:space:].[:lower:]\\/[:upper:]"))
  phyco$standard_unit <- as.character(phyco$standard_unit)
  phyco <- phyco %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  phyco$standard <- as.numeric(phyco$standard)


  phyco$precal_value <- with(phyco, ifelse(phyco$cal_point == "Point 1", phyco[17, 2],
    ifelse(phyco$cal_point == "Point 2", phyco[24, 2], "")
  ))
  phyco <- phyco %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "[:space:].[:lower:]\\/[:upper:]"))
  phyco$precal_unit <- as.character(phyco$precal_unit)
  phyco <- phyco %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  phyco$precal_value <- as.numeric(phyco$precal_value)


  phyco$postcal_value <- with(phyco, ifelse(phyco$cal_point == "Point 1", phyco[18, 2],
    ifelse(phyco$cal_point == "Point 2", phyco[25, 2], "")
  ))
  phyco <- phyco %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "[:space:].[:lower:]\\/[:upper:]"))
  phyco$postcal_unit <- as.character(phyco$postcal_unit)
  phyco <- phyco %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  phyco$postcal_value <- as.numeric(phyco$postcal_value)


  phyco$raw_value <- with(phyco, ifelse(phyco$cal_point == "Point 1", phyco[19, 2],
    ifelse(phyco$cal_point == "Point 2", phyco[26, 2], "")
  ))
  phyco <- phyco %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "[:space:].[:lower:]\\/[:upper:]"))
  phyco$raw_value_unit <- as.character(phyco$raw_value_unit)
  phyco <- phyco %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  phyco$raw_value <- as.numeric(phyco$raw_value)


  phyco$temp <- with(phyco, ifelse(phyco$cal_point == "Point 1", phyco[20, 2],
    ifelse(phyco$cal_point == "Point 2", phyco[27, 2], "")
  ))
  phyco <- phyco %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  phyco$temp_unit <- as.character(phyco$temp_unit)
  phyco <- phyco %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  phyco$temp <- as.numeric(phyco$temp)


  phyco$stability <- as.character(with(phyco, ifelse(phyco$cal_point == "Point 1", phyco[21, 2],
    ifelse(phyco$cal_point == "Point 2", phyco[28, 2], "")
  )))
  phyco$qc_score <- as.character(c(phyco[5, 2]))
  phyco$staff <- as.character(c(phyco[14, 2]))
  phyco$sondeID <- as.character(ifelse(phyco[8, 2] == "20F160656", "Exo 10",
    ifelse(phyco[8, 2] == "20F160657", "Exo 11", "")
  ))
  ## select pertinent columns
  phyco <- phyco[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "stability", "qc_score",
    "staff"
  )]

  ## remove duplicate rows
  phyco <- unique(phyco)
  phyco <- phyco %>%
    filter(cal_point != "")

  # Filter out blank rows
  phyco <- phyco %>% filter(!is.na(postcal_value))


  ## Phycocyanin (RFU) ####
  phyco_RFU <- subset(df[1:43, 9:10])
  phyco_RFU <- phyco_RFU %>%
    filter(`phyco(RFU)` != "")
  phyco_RFU$parameter <- c("Phycocyanin (RFU)")
  phyco_RFU$datetime_start <- c(phyco_RFU[2, 2])
  phyco_RFU$datetime_end <- c(phyco_RFU[3, 2])

  phyco_RFU$datetime_start <- as.numeric(phyco_RFU$datetime_start)
  phyco_RFU$datetime_start <- convert_date(phyco_RFU$datetime_start, type = "excel", fraction = TRUE)
  phyco_RFU$datetime_end <- as.numeric(phyco_RFU$datetime_end)
  phyco_RFU$datetime_end <- convert_date(phyco_RFU$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  # starts getting messy with multiple cal points
  phyco_RFU$cal_point <- with(phyco_RFU, ifelse(`phyco(RFU)` == "[Cal Point 1]", "Point 1",
    ifelse(`phyco(RFU)` == "[Cal Point 2]", "Point 2", "")
  ))


  phyco_RFU$standard <- with(phyco_RFU, ifelse(phyco_RFU$cal_point == "Point 1", phyco_RFU[16, 2],
    ifelse(phyco_RFU$cal_point == "Point 2", phyco_RFU[23, 2], "")
  ))
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(standard_unit = str_extract_all(standard, "[:upper:][:upper:][:upper:]"))
  phyco_RFU$standard_unit <- as.character(phyco_RFU$standard_unit)
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  phyco_RFU$standard <- as.numeric(phyco_RFU$standard)


  phyco_RFU$precal_value <- with(phyco_RFU, ifelse(phyco_RFU$cal_point == "Point 1", phyco_RFU[17, 2],
    ifelse(phyco_RFU$cal_point == "Point 2", phyco_RFU[24, 2], "")
  ))
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "[:upper:][:upper:][:upper:]"))
  phyco_RFU$precal_unit <- as.character(phyco_RFU$precal_unit)
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  phyco_RFU$precal_value <- as.numeric(phyco_RFU$precal_value)


  phyco_RFU$postcal_value <- with(phyco_RFU, ifelse(phyco_RFU$cal_point == "Point 1", phyco_RFU[18, 2],
    ifelse(phyco_RFU$cal_point == "Point 2", phyco_RFU[25, 2], "")
  ))
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "[:upper:][:upper:][:upper:]"))
  phyco_RFU$postcal_unit <- as.character(phyco_RFU$postcal_unit)
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  phyco_RFU$postcal_value <- as.numeric(phyco_RFU$postcal_value)


  phyco_RFU$raw_value <- with(phyco_RFU, ifelse(phyco_RFU$cal_point == "Point 1", phyco_RFU[19, 2],
    ifelse(phyco_RFU$cal_point == "Point 2", phyco_RFU[26, 2], "")
  ))
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "[:upper:][:upper:][:upper:]"))
  phyco_RFU$raw_value_unit <- as.character(phyco_RFU$raw_value_unit)
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  phyco_RFU$raw_value <- as.numeric(phyco_RFU$raw_value)


  phyco_RFU$temp <- with(phyco_RFU, ifelse(phyco_RFU$cal_point == "Point 1", phyco_RFU[20, 2],
    ifelse(phyco_RFU$cal_point == "Point 2", phyco_RFU[27, 2], "")
  ))
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  phyco_RFU$temp_unit <- as.character(phyco_RFU$temp_unit)
  phyco_RFU <- phyco_RFU %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  phyco_RFU$temp <- as.numeric(phyco_RFU$temp)


  phyco_RFU$stability <- as.character(with(phyco_RFU, ifelse(phyco_RFU$cal_point == "Point 1", phyco_RFU[21, 2],
    ifelse(phyco_RFU$cal_point == "Point 2", phyco_RFU[28, 2], "")
  )))
  phyco_RFU$qc_score <- as.character(c(phyco_RFU[5, 2]))
  phyco_RFU$staff <- as.character(c(phyco_RFU[14, 2]))
  phyco_RFU$sondeID <- as.character(ifelse(phyco_RFU[8, 2] == "20F160656", "Exo 10",
    ifelse(phyco_RFU[8, 2] == "20F160657", "Exo 11", "")
  ))

  ## select pertinent columns
  phyco_RFU <- phyco_RFU[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "stability", "qc_score",
    "staff"
  )]

  ## remove duplicate rows
  phyco_RFU <- unique(phyco_RFU)
  phyco_RFU <- phyco_RFU %>%
    filter(cal_point != "")

  # Filter out blank rows
  phyco_RFU <- phyco_RFU %>% filter(!is.na(postcal_value))


  ## Chlorophyll-A (RFU) ####
  chlA_RFU <- subset(df[1:43, 11:12])
  chlA_RFU <- chlA_RFU %>%
    filter(`chlA(RFU)` != "")
  chlA_RFU$parameter <- c("Chlorophyll-A (RFU)")
  chlA_RFU$datetime_start <- c(chlA_RFU[2, 2])
  chlA_RFU$datetime_end <- c(chlA_RFU[3, 2])

  chlA_RFU$datetime_start <- as.numeric(chlA_RFU$datetime_start)
  chlA_RFU$datetime_start <- convert_date(chlA_RFU$datetime_start, type = "excel", fraction = TRUE)
  chlA_RFU$datetime_end <- as.numeric(chlA_RFU$datetime_end)
  chlA_RFU$datetime_end <- convert_date(chlA_RFU$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  # starts getting messy with multiple cal points
  chlA_RFU$cal_point <- with(chlA_RFU, ifelse(`chlA(RFU)` == "[Cal Point 1]", "Point 1",
    ifelse(`chlA(RFU)` == "[Cal Point 2]", "Point 2", "")
  ))


  chlA_RFU$standard <- with(chlA_RFU, ifelse(chlA_RFU$cal_point == "Point 1", chlA_RFU[16, 2],
    ifelse(chlA_RFU$cal_point == "Point 2", chlA_RFU[23, 2], "")
  ))
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(standard_unit = str_extract_all(standard, "[:upper:][:upper:][:upper:]"))
  chlA_RFU$standard_unit <- as.character(chlA_RFU$standard_unit)
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  chlA_RFU$standard <- as.numeric(chlA_RFU$standard)


  chlA_RFU$precal_value <- with(chlA_RFU, ifelse(chlA_RFU$cal_point == "Point 1", chlA_RFU[17, 2],
    ifelse(chlA_RFU$cal_point == "Point 2", chlA_RFU[24, 2], "")
  ))
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "[:upper:][:upper:][:upper:]"))
  chlA_RFU$precal_unit <- as.character(chlA_RFU$precal_unit)
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  chlA_RFU$precal_value <- as.numeric(chlA_RFU$precal_value)


  chlA_RFU$postcal_value <- with(chlA_RFU, ifelse(chlA_RFU$cal_point == "Point 1", chlA_RFU[18, 2],
    ifelse(chlA_RFU$cal_point == "Point 2", chlA_RFU[25, 2], "")
  ))
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "[:upper:][:upper:][:upper:]"))
  chlA_RFU$postcal_unit <- as.character(chlA_RFU$postcal_unit)
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  chlA_RFU$postcal_value <- as.numeric(chlA_RFU$postcal_value)


  chlA_RFU$raw_value <- with(chlA_RFU, ifelse(chlA_RFU$cal_point == "Point 1", chlA_RFU[19, 2],
    ifelse(chlA_RFU$cal_point == "Point 2", chlA_RFU[26, 2], "")
  ))
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "[:upper:][:upper:][:upper:]"))
  chlA_RFU$raw_value_unit <- as.character(chlA_RFU$raw_value_unit)
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  chlA_RFU$raw_value <- as.numeric(chlA_RFU$raw_value)


  chlA_RFU$temp <- with(chlA_RFU, ifelse(chlA_RFU$cal_point == "Point 1", chlA_RFU[20, 2],
    ifelse(chlA_RFU$cal_point == "Point 2", chlA_RFU[27, 2], "")
  ))
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  chlA_RFU$temp_unit <- as.character(chlA_RFU$temp_unit)
  chlA_RFU <- chlA_RFU %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  chlA_RFU$temp <- as.numeric(chlA_RFU$temp)


  chlA_RFU$stability <- as.character(with(chlA_RFU, ifelse(chlA_RFU$cal_point == "Point 1", chlA_RFU[21, 2],
    ifelse(chlA_RFU$cal_point == "Point 2", chlA_RFU[28, 2], "")
  )))
  chlA_RFU$qc_score <- as.character(c(chlA_RFU[5, 2]))
  chlA_RFU$staff <- as.character(c(chlA_RFU[14, 2]))
  chlA_RFU$sondeID <- as.character(ifelse(chlA_RFU[8, 2] == "20F160656", "Exo 10",
    ifelse(chlA_RFU[8, 2] == "20F160657", "Exo 11", "")
  ))

  ## select pertinent columns
  chlA_RFU <- chlA_RFU[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "stability", "qc_score",
    "staff"
  )]

  ## remove duplicate rows
  chlA_RFU <- unique(chlA_RFU)
  chlA_RFU <- chlA_RFU %>%
    filter(cal_point != "")

  # Filter out blank rows
  chlA_RFU <- chlA_RFU %>% filter(!is.na(postcal_value))

  ## Turbidity (NTU/FNU) ####
  turb <- subset(df[1:43, 13:14])
  turb <- turb %>%
    filter(turbidity != "")
  turb$parameter <- c("Turbidity")
  turb$datetime_start <- c(turb[2, 2])
  turb$datetime_end <- c(turb[3, 2])

  turb$datetime_start <- as.numeric(turb$datetime_start)
  turb$datetime_start <- convert_date(turb$datetime_start, type = "excel", fraction = TRUE)
  turb$datetime_end <- as.numeric(turb$datetime_end)
  turb$datetime_end <- convert_date(turb$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  # starts getting messy with multiple cal points
  turb$cal_point <- with(turb, ifelse(turbidity == "[Cal Point 1]", "Point 1",
    ifelse(turbidity == "[Cal Point 2]", "Point 2", "")
  ))


  turb$standard <- with(turb, ifelse(turb$cal_point == "Point 1", turb[16, 2],
    ifelse(turb$cal_point == "Point 2", turb[23, 2], "")
  ))
  turb <- turb %>% dplyr::mutate(standard_unit = str_extract_all(standard, "[:upper:][:upper:][:upper:]"))
  turb$standard_unit <- as.character(turb$standard_unit)
  turb <- turb %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  turb$standard <- as.numeric(turb$standard)


  turb$precal_value <- with(turb, ifelse(turb$cal_point == "Point 1", turb[17, 2],
    ifelse(turb$cal_point == "Point 2", turb[24, 2], "")
  ))
  turb <- turb %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "[:upper:][:upper:][:upper:]"))
  turb$precal_unit <- as.character(turb$precal_unit)
  turb <- turb %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  turb$precal_value <- as.numeric(turb$precal_value)


  turb$postcal_value <- with(turb, ifelse(turb$cal_point == "Point 1", turb[18, 2],
    ifelse(turb$cal_point == "Point 2", turb[25, 2], "")
  ))
  turb <- turb %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "[:upper:][:upper:][:upper:]"))
  turb$postcal_unit <- as.character(turb$postcal_unit)
  turb <- turb %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  turb$postcal_value <- as.numeric(turb$postcal_value)


  turb$raw_value <- with(turb, ifelse(turb$cal_point == "Point 1", turb[19, 2],
    ifelse(turb$cal_point == "Point 2", turb[26, 2], "")
  ))
  turb <- turb %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "[:upper:][:upper:][:upper:]"))
  turb$raw_value_unit <- as.character(turb$raw_value_unit)
  turb <- turb %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  turb$raw_value <- as.numeric(turb$raw_value)


  turb$temp <- with(turb, ifelse(turb$cal_point == "Point 1", turb[20, 2],
    ifelse(turb$cal_point == "Point 2", turb[27, 2], "")
  ))
  turb <- turb %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  turb$temp_unit <- as.character(turb$temp_unit)
  turb <- turb %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  turb$temp <- as.numeric(turb$temp)


  turb$stability <- as.character(with(turb, ifelse(turb$cal_point == "Point 1", turb[21, 2],
    ifelse(turb$cal_point == "Point 2", turb[28, 2], "")
  )))
  turb$qc_score <- as.character(c(turb[5, 2]))
  turb$staff <- as.character(c(turb[14, 2]))
  turb$sondeID <- as.character(ifelse(turb[8, 2] == "20F160656", "Exo 10",
    ifelse(turb[8, 2] == "20F160657", "Exo 11", "")
  ))

  ## select pertinent columns
  turb <- turb[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "stability", "qc_score",
    "staff"
  )]

  ## remove duplicate rows
  turb <- unique(turb)
  turb <- turb %>%
    filter(cal_point != "")

  # Filter out blank rows
  turb <- turb %>% filter(!is.na(postcal_value))

  ## pH ####
  pH_df <- subset(df[1:43, 15:16])
  pH_df <- pH_df %>%
    filter(pH != "")
  pH_df$parameter <- c("pH")
  pH_df$datetime_start <- c(pH_df[2, 2])
  pH_df$datetime_end <- c(pH_df[3, 2])

  pH_df$datetime_start <- as.numeric(pH_df$datetime_start)
  pH_df$datetime_start <- convert_date(pH_df$datetime_start, type = "excel", fraction = TRUE)
  pH_df$datetime_end <- as.numeric(pH_df$datetime_end)
  pH_df$datetime_end <- convert_date(pH_df$datetime_end, type = "excel", fraction = TRUE)


  ## add in other columns - mutate to separate values from units, then transform values to numeric class
  # starts getting messy with multiple cal points
  pH_df$cal_point <- with(pH_df, ifelse(pH == "[Cal Point 1]", "Point 1",
    ifelse(pH == "[Cal Point 2]", "Point 2",
      ifelse(pH == "[Cal Point 3]", "Point 3", "")
    )
  ))


  pH_df$standard <- with(pH_df, ifelse(pH_df$cal_point == "Point 1", pH_df[21, 2],
    ifelse(pH_df$cal_point == "Point 2", pH_df[28, 2],
      ifelse(pH_df$cal_point == "Point 3", pH_df[35, 2], "")
    )
  ))
  pH_df <- pH_df %>% dplyr::mutate(standard_unit = str_extract_all(standard, "[:lower:][:upper:]"))
  pH_df$standard_unit <- as.character(pH_df$standard_unit)
  pH_df <- pH_df %>% dplyr::mutate(standard = str_extract_all(standard, "[:digit:]+\\.[:digit:]+"))
  pH_df$standard <- as.numeric(pH_df$standard)


  pH_df$precal_value <- with(pH_df, ifelse(pH_df$cal_point == "Point 1", pH_df[22, 2],
    ifelse(pH_df$cal_point == "Point 2", pH_df[29, 2],
      ifelse(pH_df$cal_point == "Point 3", pH_df[36, 2], "")
    )
  ))
  pH_df <- pH_df %>% dplyr::mutate(precal_unit = str_extract_all(precal_value, "[:lower:][:upper:]"))
  pH_df$precal_unit <- as.character(pH_df$precal_unit)
  pH_df <- pH_df %>% dplyr::mutate(precal_value = str_extract_all(precal_value, "[:digit:]+\\.[:digit:]+"))
  pH_df$precal_value <- as.numeric(pH_df$precal_value)


  pH_df$postcal_value <- with(pH_df, ifelse(pH_df$cal_point == "Point 1", pH_df[23, 2],
    ifelse(pH_df$cal_point == "Point 2", pH_df[30, 2],
      ifelse(pH_df$cal_point == "Point 3", pH_df[37, 2], "")
    )
  ))
  pH_df <- pH_df %>% dplyr::mutate(postcal_unit = str_extract_all(postcal_value, "[:lower:][:upper:]"))
  pH_df$postcal_unit <- as.character(pH_df$postcal_unit)
  pH_df <- pH_df %>% dplyr::mutate(postcal_value = str_extract_all(postcal_value, "[:digit:]+\\.[:digit:]+"))
  pH_df$postcal_value <- as.numeric(pH_df$postcal_value)


  pH_df$raw_value <- with(pH_df, ifelse(pH_df$cal_point == "Point 1", pH_df[24, 2],
    ifelse(pH_df$cal_point == "Point 2", pH_df[31, 2],
      ifelse(pH_df$cal_point == "Point 3", pH_df[38, 2], "")
    )
  ))
  pH_df <- pH_df %>% dplyr::mutate(raw_value_unit = str_extract_all(raw_value, "[:lower:][:upper:]"))
  pH_df$raw_value_unit <- as.character(pH_df$raw_value_unit)
  pH_df <- pH_df %>% dplyr::mutate(raw_value = str_extract_all(raw_value, "[:digit:]+\\.[:digit:]+"))
  pH_df$raw_value <- as.numeric(pH_df$raw_value)


  pH_df$temp <- with(pH_df, ifelse(pH_df$cal_point == "Point 1", pH_df[25, 2],
    ifelse(pH_df$cal_point == "Point 2", pH_df[32, 2],
      ifelse(pH_df$cal_point == "Point 3", pH_df[39, 2], "")
    )
  ))
  pH_df <- pH_df %>% dplyr::mutate(temp_unit = str_extract_all(temp, "[:upper:]"))
  pH_df$temp_unit <- as.character(pH_df$temp_unit)
  pH_df <- pH_df %>% dplyr::mutate(temp = str_extract_all(temp, "[:digit:]+\\.[:digit:]+"))
  pH_df$temp <- as.numeric(pH_df$temp)


  pH_df$stability <- as.character(with(pH_df, ifelse(pH_df$cal_point == "Point 1", pH_df[26, 2],
    ifelse(pH_df$cal_point == "Point 2", pH_df[33, 2],
      ifelse(pH_df$cal_point == "Point 3", pH_df[40, 2], "")
    )
  )))
  pH_df$qc_score <- as.character(c(pH_df[5, 2]))
  pH_df$staff <- as.character(c(pH_df[14, 2]))
  pH_df$sondeID <- as.character(ifelse(pH_df[8, 2] == "20F160656", "Exo 10",
    ifelse(pH_df[8, 2] == "20F160657", "Exo 11", "")
  ))

  ## select pertinent columns
  pH_df <- pH_df[c(
    "sondeID", "datetime_start", "datetime_end", "parameter", "cal_point", "standard", "standard_unit", "precal_value",
    "precal_unit", "postcal_value", "postcal_unit", "raw_value", "raw_value_unit", "temp", "temp_unit", "stability", "qc_score",
    "staff"
  )]

  ## remove duplicate rows
  pH_df <- unique(pH_df)
  pH_df <- pH_df %>%
    filter(cal_point != "")

  # Filter out blank rows
  pH_df <- pH_df %>% filter(!is.na(postcal_value))

  ######## COMBINE DATAFRAMES ########
  dfs <- list(DO, cond, chlA, phyco, phyco_RFU, chlA_RFU, turb, pH_df)

  merged_df <- reduce(dfs, full_join)

  df.wq <- merged_df

  # UniqueID
  df.wq <- df.wq %>% mutate(
    datetime_start = lubridate::round_date(datetime_start, unit = "minute"),
    datetime_end = lubridate::round_date(datetime_end, unit = "minute")
  )
  df.wq$UniqueID <- ""
  df.wq$UniqueID <- paste(df.wq$sondeID, format(df.wq$datetime_start, format = "%Y-%m-%d %H:%M"), df.wq$parameter, df.wq$cal_point, sep = "_")

  ## Make sure it is unique within the data file - if not then exit function and send warning
  dupecheck <- which(duplicated(df.wq$UniqueID))
  dupes <- df.wq$UniqueID[dupecheck] # These are the dupes

  if (length(dupes) > 0) {
    # Exit function and send a warning to userlength(dupes) # number of dupes
    stop(paste(
      "This data file contains", length(dupes),
      "records that appear to be duplicates. Eliminate all duplicates before proceeding.",
      "The duplicate records include:", paste(head(dupes, 15), collapse = ", ")
    ), call. = FALSE)
  }

  ### Create a database pool connection using App credentials ----
  dsn <- "DCR_DWSP_App_R"
  schema <- "Quabbin"
  tz <- "America/New_York"
  tz_out <- "America/New_York"
  pool <- dbPool(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz, timezone_out = tz_out)
  # Insert database queries here

  dbtbl1 <- "tblResEXOCalibrations"
  exo_cal_tbl <- tbl(pool, DBI::Id(schema, dbtbl1))
  exo_cal_db <- exo_cal_tbl %>%
    collect()

  ## Compare data with existing to make sure none is being duplicated
  exo_cal_db$UniqueID <- paste(exo_cal_db$SondeID, format(exo_cal_db$DateTimeET_Start, format = "%Y-%m-%d %H:%M"), exo_cal_db$Parameter, exo_cal_db$Cal_Point, sep = "_")
  dupes2 <- exo_cal_db[exo_cal_db$UniqueID %in% df.wq$UniqueID, ]

  if (nrow(dupes2) > 0) {
    # Exit function and send a warning to user
    stop(paste(
      "This data file contains", nrow(dupes2),
      "records that appear to already exist in the database!
             Eliminate all duplicates before proceeding.",
      "The duplicate records include:", paste(head(dupes2$UniqueID, 15), collapse = ", ")
    ), call. = FALSE)
  }


  # WQ
  setIDs <- function() {
    query.wq <- dbGetQuery(pool, glue("SELECT max(ID) FROM [{schema}].[{ImportTable}]"))
    # Get current max ID
    if (is.na(query.wq)) {
      query.wq <- 0
    } else {
      query.wq <- query.wq
    }
    ID.max.wq <- as.numeric(unlist(query.wq))
    rm(query.wq)

    ### ID wq
    df.wq$ID <- seq.int(nrow(df.wq)) + ID.max.wq
  }
  df.wq$ID <- setIDs()

  # Remove UniqueID, reorder, and rename columns to match database
  df.wq <- df.wq %>%
    select(-UniqueID) %>%
    select(c(21, 1:20)) %>%
    rename(
      SondeID = sondeID,
      DateTimeET_Start = datetime_start,
      DateTimeET_End = datetime_end,
      Parameter = parameter,
      Cal_Point = cal_point,
      Standard = standard,
      Standard_Unit = standard_unit,
      PreCal_Value = precal_value,
      PreCal_Unit = precal_unit,
      PostCal_Value = postcal_value,
      PostCal_Unit = postcal_unit,
      Raw_Value = raw_value,
      Raw_Value_Unit = raw_value_unit,
      Temp = temp,
      Temp_Unit = temp_unit,
      Barometer = barometer,
      Barometer_Unit = barometer_unit,
      Stability = stability,
      QC_Score = qc_score,
      Staff = staff
    ) %>%
    mutate(ID = as.integer(ID)) %>%
    mutate(across(
      .cols = everything(),
      ~ str_replace(., "\U00B5", "u")
    ))


  # Reorder columns to match the database table exactly ####
  col.order.wq <- dbListFields(pool, schema_name = schema, name = ImportTable)
  df.wq <- df.wq[, col.order.wq]

  # change variable types to match database
  # df.wq$ID <- as.integer(df.wq$ID)
  # df.wq$DataSourceID <- as.integer(df.wq$DataSourceID)

  # # Change time to UTC in DateTimeET NOT NECESSARY FOR PROFILES
  # df.wq$DateTimeET <- format(df.wq$DateTimeET, tz = "America/New_York", usetz = TRUE) %>%
  #   lubridate::as_datetime()

  # Create a list of the processed datasets
  dfs <- list()
  dfs[[1]] <- df.wq
  dfs[[2]] <- path
  dfs[[3]] <- NULL # Removed condition to test for flags and put it in the setFlagIDS() function

  # # Close the database pool ----
  poolClose(pool)
  rm(pool)
  return(dfs)
} # END FUNCTION

# dfs <- PROCESS_DATA(file, rawdatafolder, filename.db, probe, ImportTable = ImportTable, ImportFlagTable = ImportFlagTable)
#
# # Extract each element needed
# df.wq     <- dfs[[1]]
# path      <- dfs[[2]]
# df.flags  <- dfs[[3]]
######################################################################## .
###                       Write Data to Database                    ####
######################################################################## .

IMPORT_DATA <- function(df.wq, df.flags = NULL, path, file, filename.db, processedfolder = NULL, ImportTable, ImportFlagTable = NULL) {
  # df.flags is an optional argument  - not used for this dataset

  ### Create a database pool connection using App credentials ----
  dsn <- "DCR_DWSP_App_R"
  schema <- "Quabbin"
  tz <- "America/New_York"
  tz_out <- "America/New_York"
  pool <- dbPool(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz, timezone_out = tz_out)

  poolWithTransaction(pool, function(conn) {
    pool::dbWriteTable(pool, DBI::Id(schema = schema, table = ImportTable), value = df.wq, append = TRUE, row.names = FALSE)
  })

  #* Close the database pool ----
  poolClose(pool)
  rm(pool)

  return("Import Successful")
}
### END

# IMPORT_DATA(df.wq, df.flags = NULL, path, file, filename.db, processedfolder = NULL,
#             ImportTable = ImportTable, ImportFlagTable = NULL)
