###############################  HEADER  ######################################
#  TITLE: ImportBuoy_Q.R
#  DESCRIPTION: This script will process/import MWRA buoy Profile Data to 'tblBuoyProfiles' database
#  AUTHOR(S): Evan Krause
#  ADAPTED FROM: 'ImportProfiles_Q.R' by authors Dan Crocker, Max Nyquist and Joy Trahan-Liptak
#  DATE LAST UPDATED: 2025-08-28
#  Last Update: initial script creation
#  GIT REPO:
#  R version 4.5.1 (2025-06-13)
##############################################################################.

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

########################################################################.
###                          Process Data                           ####
########################################################################.

PROCESS_DATA <- function(
  file,
  rawdatafolder,
  filename.db,
  probe = NULL,
  ImportTable,
  ImportFlagTable = NULL
) {
  # Start the function - takes 1 input (File)

  # Eliminate Scientific notation in numerical fields
  options(scipen = 999)

  # Get the full path to the file from WAVE_WIT_Local
  path <- paste0(rawdatafolder, "/", file)

  # Assign the sheet number from xlsx in path
  sheetNum <- as.numeric(length(excel_sheets(path)))

  # Assign the name of the rightmost sheet in file as sheetName
  sheetName <- excel_sheets(path)[sheetNum]

  # Read in the raw data - defaults to the last sheet added
  df.wq <- read_excel(path, sheet = sheetNum, col_names = T, trim_ws = T)

  # df.wq2 <- read_csv(path, col_names = T, trim_ws = T) #maybe later...

  ### Profile dataframe ####

  df.wq <- df.wq |>
    filter(Parameter != "ORP (mV)")

  df.wq <- df.wq |>
    mutate(
      DateTimeET = as_datetime(DateTimeET, tz="America/New_York"),
      #coerce to datetime class and rename to match DB
      "Probe_Type" = "YSI_EXO2_MWRABuoy",
      #create 'Probe_Type' col to match DB
      "Station" = "202B",
      #create 'Station' col and assign value "202B" to match DB
      FinalResult = Result,
      #rename 'Result' to 'FinalResult' to match DB
      UniqueID = NA,
      #create 'UniqueID' col for later assignment
      DataSource = paste(file, sheetName, sep = "_"),
      #create 'DataSource' col from combined file name and 'sheetName'
      ImportDate = today()
      #create 'ImportDate' col from date of import (today)
    )

  #String manipulation----

  df.wq$Parameter <- str_replace_all(
    df.wq$Parameter,
    c(
      "pH" = "pH (pH)", #alter pH rows to allow delimiter separation
      "ODO \\(%sat." = "odo (%)"
    ) #alter ODO (%sat) rows to distinguish from ODO (mg/L) rows and enable string manipulation
  )

  df.wq <- df.wq |> #separate 'Parameter' col into distinct 'Parameter' and 'units' cols, using first open parentheses as split point
    separate_wider_delim(
      cols = Parameter,
      delim = "(",
      names = c("Parameter", "Units"),
      cols_remove = T #removes old column
    )

  df.wq$Units <- substr(df.wq$Units, 1, nchar(df.wq$Units) - 1) #remove closing parentheses from rows in 'units' column

  df.wq$Units <- str_replace_all(df.wq$Units, c("C" = "Deg-C")) #replace 'units' col strings to match DB

  df.wq$Parameter <- str_replace_all(
    #replace 'Parameter' col strings to match DB
    df.wq$Parameter,
    c(
      "BGA-PC" = "Blue Green Algae",
      "SpCond" = "Specific Conductivity",
      "Temp" = "Water Temperature",
      "Turbidity" = "Turbidity NTU",
      "odo" = "Oxygen Saturation",
      "ODO" = "Dissolved Oxygen",
      "Chlorophyll " = "Chlorophyll"
    )
  )

  #create col 'UniqueID'
  df.wq$UniqueID <- paste(
    #combine 'Station', 'DateTimeET', 'FinalResult', and first 3 letters of 'Parameter' to form 'UniqueID' col
    df.wq$Station,
    df.wq$DateTimeET,
    df.wq$FinalResult,
    substr(df.wq$Parameter, 1, 3),
    sep = "_"
  )

  #duplicate checks----

  ## Make sure it is unique within the data file - if not then exit function and send warning
  dupecheck <- which(duplicated(df.wq$UniqueID))
  dupes <- df.wq$UniqueID[dupecheck] # These are the dupes

  if (length(dupes) > 0) {
    # Exit function and send a warning to userlength(dupes) # number of dupes
    stop(
      paste(
        "This data file contains",
        length(dupes),
        "records that appear to be duplicates. Eliminate all duplicates before proceeding.",
        "The duplicate records include:",
        paste(head(dupes, 15), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Connect to database
  dsn <- filename.db
  database <- "DCR_DWSP"
  schema <- "Quabbin"
  tz <- 'UTC'
  con <- dbConnect(
    odbc::odbc(),
    dsn = dsn,
    uid = dsn,
    pwd = config[["DB Connection PW"]],
    timezone = tz
  )

  #set db id ----
  Uniq <- dbGetQuery(
    con,
    glue("SELECT [UniqueID], [ID] FROM [{schema}].[{ImportTable}]")
  )
  dupes2 <- Uniq[Uniq$UniqueID %in% df.wq$UniqueID, ]

  if (nrow(dupes2) > 0) {
    # Exit function and send a warning to user
    stop(
      paste(
        "This data file contains",
        nrow(dupes2),
        "records that appear to already exist in the database!
               Eliminate all duplicates before proceeding.",
        "The duplicate records include:",
        paste(head(dupes2$UniqueID, 15), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  rm(Uniq)

  # DataSourceID
  df.wq <- df.wq |>
    #arrange rows by increasing "DateTimeET'
    arrange(DateTimeET) |>
    #create 'DataSourceID' col from numeric sequence of rows
    mutate(DataSourceID = seq(1, nrow(df.wq), 1))

  #
  setIDs <- function() {
    query.wq <- dbGetQuery(
      con,
      glue("SELECT max(ID) FROM [{schema}].[{ImportTable}]")
    )
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

  #DB matching ----
  # Get column names from db table
  cnames <- dbListFields(con, schema_name = schema, ImportTable)

  # Reorder columns to match the database table
  df.wq <- df.wq |> select(all_of(cnames))

  # change variable types to match database
  df.wq$ID <- as.integer(df.wq$ID)
  df.wq$DataSourceID <- as.integer(df.wq$DataSourceID)

  # Create a list of the processed datasets
  dfs <- list()
  dfs[[1]] <- df.wq
  dfs[[2]] <- path
  #dfs[[3]] <- NULL # Removed condition to test for flags and put it in the setFlagIDS() function

  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)
  return(dfs)
} # END FUNCTION

########################################################################.
###                       Write Data to Database                    ####
########################################################################.

IMPORT_DATA <- function(
  df.wq,
  df.flags = NULL,
  path,
  file,
  filename.db,
  processedfolder,
  ImportTable,
  ImportFlagTable = NULL
) {
  # df.flags is an optional argument  - not used for this dataset

  # Establish db connection
  dsn <- filename.db
  schema <- 'Quabbin'
  tz <- 'America/New_York'
  pool <- dbPool(
    odbc::odbc(),
    dsn = dsn,
    uid = dsn,
    pwd = config[["DB Connection PW"]],
    timezone = tz
  )

  #write to DB table
  poolWithTransaction(pool, function(conn) {
    pool::dbWriteTable(
      pool,
      DBI::Id(schema = schema, table = ImportTable),
      value = df.wq,
      append = TRUE,
      row.names = FALSE
    )
  })

  #* Close the database pool ----
  poolClose(pool)
  rm(pool)

  return("Import Successful")
}
