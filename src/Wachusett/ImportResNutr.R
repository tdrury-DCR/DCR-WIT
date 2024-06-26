###############################  HEADER  ######################################
#  TITLE: ImportResNutr.R
#  DESCRIPTION: This script will Format/Process MWRA data to DCR
#  AUTHOR(S): Nick Zinck/Dan Crocker, October, 2017
#  DATE LAST UPDATED: 2023-09-26 (JTL)
#  This script will process and import MWRA Projects: MDCMNTH
#  GIT REPO: 
#  R version 4.0.3 (2020-10-10)  x86_64
##############################################################################.

# # Load libraries needed
# 
# library(stringr)
# library(odbc)
# library(RODBC)
# library(DBI)
# library(lubridate)
# library(magrittr)
# # Tidyverse and readxl are loaded in App.r
# 
# # COMMENT OUT ABOVE CODE EXCEPT FOR LOADING LIBRARIES WHEN RUNNING IN SHINY

########################################################################.
####                 PROCESSING FUNCTION                            ####
########################################################################.
PROCESS_DATA <- function(file, rawdatafolder, filename.db, probe = NULL, ImportTable, ImportFlagTable = NULL){ # Start the function - takes 1 input (File)
options(scipen = 999) # Eliminate Scientific notation in numerical fields
# Get the full path to the file
path <- paste0(rawdatafolder,"/", file)

# Read in the data to a dataframe
df.wq <- read_excel(path, sheet= 1, col_names = T, trim_ws = T, na = "nil") %>%
  as.data.frame()   # This is the raw data - data comes in as xlsx file, so read.csv will not work
df.wq <- df.wq[,c(1:25)]
### Perform Data checks ###

# At this point there could be a number of checks to make sure data is valid
  # Check to make sure there are 25 variables (columns)
  if (ncol(df.wq) != 25) {
    # Send warning message to UI
    #warning1 <- print(paste0("There are not 25 columns of data in this file.\n Check the file before proceeding"))
    stop("There are not 25 columns of data in this file.\n Check the file before proceeding")
  }

  # Check to make sure column 1 is "Original Sample" or other?
  if (any(colnames(df.wq)[1] != "Original Sample" & df.wq[25] != "X Ldl")) {
    # Send warning message to UI
    #warning2 <- print(paste0("At least 1 column heading is unexpected.\n Check the file before proceeding"))
    stop("At least 1 column heading is unexpected.\n Check the file before proceeding")
  }

  # Check to see if there were any miscellaneous locations that did not get assigned a location
  if (length(which(str_detect(df.wq$Location, "MISC"),TRUE)) > 0) {
    #warning3 <- print(paste0("There are unspecified (MISC) locations that need to be corrected before importing data"))
    stop("There are unspecified (MISC) locations that need to be corrected before importing data")
  }
# Check to see if there were any GENERAL locations that did not get assigned a location
if (length(which(str_detect(df.wq$Location, "GENERAL-GEN"),TRUE)) > 0) {
  #warning3 <- print(paste0("There are unspecified (MISC) locations that need to be corrected before importing data"))
  stop("There are unspecified (GEN) locations that need to be corrected before importing data")
}
# 
# # Check to see if depths are reported correctly
if (length(which(df.wq$`Formatted Entry`[df.wq$`Display String` == "f"] < 1)) > 0) {
  #warning3 <- print(paste0("There are reported depths less than 1 ft. This likely indicates that values are in meters but units are in feet."))
  stop("There are reported depths less than 1 ft. This likely indicates that values are in meters but units are in feet.")
}

# Any other checks?  Otherwise data is validated, proceed to reformatting...
###


# Connect to db for queries below
dsn <- filename.db
database <- "DCR_DWSP"
schema <- "Wachusett"
tz <- 'America/New_York'
con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

########################################################################.
###                     START REFORMATTING THE DATA                 ####
########################################################################.

### Rename Columns in Raw Data
names(df.wq) = c("SampleGroup",
                 "SampleNumber",
                 "TextID",
                 "Location",
                 "Description",
                 "TripNum",
                 "LabRecDateET",
                 "SampleDate",
                 "SampleTime",
                 "PrepOnET",
                 "DateTimeAnalyzedET",
                 "AnalyzedBy",
                 "Analysis",
                 "ReportedName",
                 "Parameter",
                 "ResultReported",
                 "Units",
                 "Comment",
                 "SampledBy",
                 "Status",
                 "EDEP_Confirm",
                 "EDEP_MW_Confirm",
                 "Reportable",
                 "Method",
                 "DetectionLimit")


### Date and Time:
# SampleDateTime
# Split the Sample time into date and time
df.wq$SampleDate <- as.Date(df.wq$SampleDate)
#df.wq$SampleTime[is.na(df.wq$SampleTime)] <- paste(df.wq$SampleDate[is.na(df.wq$SampleTime)])

df.wq <- separate(df.wq, SampleTime, into = c("date", "Time"), sep = " ")

# Merge the actual date column with the new Time Column and reformat to POSIXct
df.wq$DateTimeET <- as.POSIXct(paste(as.Date(df.wq$SampleDate, format ="%Y-%m-%d"), df.wq$Time, sep = " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York", usetz = T)

# Fix other data types
df.wq$EDEP_Confirm <- as.character(df.wq$EDEP_Confirm)
df.wq$EDEP_MW_Confirm <- as.character(df.wq$EDEP_Confirm)
df.wq$Comment <- as.character(df.wq$Comment)
df.wq$SampleGroup <- as.character(df.wq$SampleGroup)
df.wq$SampleNumber <- as.character(df.wq$SampleNumber)
df.wq$PrepOnET <- as.POSIXct(df.wq$PrepOnET) # note - this col does not contain any data and could be removed

# # Recode Staff Gauge Depth to Water Depth (moved below after final result)
# df.wq$Parameter <- recode(df.wq$Parameter, 'Staff Gauge Depth' = "Water Depth")

# Fix the Parameter names  - change from MWRA name to ParameterName
# dbListTables(con, schema_name = schema)

params <- dbReadTable(con,  Id(schema = schema, table = "tblParameters"))
df.wq$Parameter <- params$ParameterName[match(df.wq$Parameter, params$ParameterMWRAName)]

# Delete possible Sample Address rows (Associated with MISC Sample Locations):
df.wq <- filter(df.wq, !is.na(ResultReported)) %>%  # Filter out any sample with no results (There shouldn't be, but they do get included sometimes)
  filter(!is.na(Parameter))
df.wq <- df.wq %>% slice(which(!grepl("Sample Address", df.wq$Parameter, fixed = TRUE)))
df.wq <- df.wq %>% slice(which(!grepl("(DEP)", df.wq$Parameter, fixed = TRUE))) # Filter out rows where Parameter contains  "(DEP)"
df.wq <- df.wq %>% slice(which(!grepl("X", df.wq$Status, fixed = TRUE))) # Filter out records where Status is X
# Fix the Location names
df.wq$Location %<>%
  gsub("WACHUSET-","", .) %>%
  gsub("FIELD-QC-","", .)


########################################################################.
###                            Add new Columns                      ####
########################################################################.
### Unique ID number
df.wq$UniqueID <- ""
df.wq$UniqueID <- paste(df.wq$Location, format(df.wq$DateTimeET, format = "%Y-%m-%d %H:%M"), params$ParameterAbbreviation[match(df.wq$Parameter, params$ParameterName)], sep = "_")

## Make sure it is unique within the data file - if not then exit function and send warning
dupecheck <- which(duplicated(df.wq$UniqueID))
dupes <- df.wq$UniqueID[dupecheck] # These are the dupes

if (length(dupes) > 0){
  # Exit function and send a warning to userlength(dupes) # number of dupes
  stop(paste0("This data file contains ", length(dupes),
             " records that appear to be duplicates. Eliminate all duplicates before proceeding"))
  #print(dupes) # Show the duplicate Unique IDs to user in Shiny
}
### Make sure records are not already in DB
Uniq <- dbGetQuery(con, glue("SELECT [UniqueID], [ID] FROM [{schema}].[{ImportTable}]"))
# flags <- dbGetQuery(con, glue("SELECT [SampleID], [FlagCode] FROM [{schema}].[{ImportFlagTable}] WHERE FlagCode = 102"))
dupes2 <- Uniq[Uniq$UniqueID %in% df.wq$UniqueID,]
# dupes2 <- filter(dupes2, !ID %in% flags$SampleID) # take out any preliminary samples (they should get overwritten during import)


# Uniq <- dbGetQuery(con,paste0("SELECT UniqueID, ID FROM ", ImportTable))
# dupes2 <- Uniq$UniqueID[Uniq$UniqueID %in% df.wq$UniqueID]

if (nrow(dupes2) > 0){
  # Exit function and send a warning to user
  stop(paste0("This data file contains ", length(dupes2),
              " records that appear to already exist in the database! Eliminate all duplicates before proceeding"))
  #print(dupes2) # Show the duplicate Unique IDs to user in Shiny
}
rm(Uniq)

### DataSource
df.wq <- df.wq %>% mutate(DataSource = paste0("MWRA_", file))

### DataSourceID
# Do some sorting first:
df.wq <- df.wq[with(df.wq, order(DateTimeET, Location, Parameter)),]

# Assign the numbers
df.wq$DataSourceID <- as.numeric(seq(1, nrow(df.wq), 1))


# note: some reported results are "A" for (DEP). These will be NA in the ResultFinal Columns
# ResultReported -
# Find all valid results, change to numeric and round to 6 digits in order to eliminate scientific notation
# Replace those results with the updated value converted back to character

edits <- str_detect(df.wq$ResultReported, paste(c("<",">"), collapse = '|')) %>%
  which(T)
update <- as.numeric(df.wq$ResultReported[-edits], digits = 6)
df.wq$ResultReported[-edits] <- as.character(update)

# Add new column for censored data
df.wq <- df.wq %>%
  mutate("IsCensored" = NA_integer_)

df.wq$IsCensored <- as.logical(df.wq$IsCensored)

if(length(edits) == 0) {
  df.wq$IsCensored <- FALSE
} else {
  df.wq$IsCensored[edits] <- TRUE
  df.wq$IsCensored[-edits] <- FALSE
}

### FinalResult (numeric)
# Make the variable
df.wq$FinalResult <- NA
# Set the vector for mapply to operate on
x <- df.wq$ResultReported
# Function to determine FinalResult
FR <- function(x) {
  if(str_detect(x, "<")){# BDL
    as.numeric(gsub("<","", x), digits = 4) # THEN strip "<" from reported result, make numeric, leave Result = Detect Limit.
  } else if (str_detect(x, ">")){
      as.numeric(gsub(">","", x)) # THEN strip ">" form reported result, make numeric.
  } else {
      as.numeric(x)
    }# ELSE THEN just use Result Reported for Result and make numeric
  }
df.wq$FinalResult <- mapply(FR,x) %>%
  round(digits = 4)

# Convert water depth reported in f to m, change param name and units columns
df.wq2 <- df.wq %>% 
  filter(!df.wq$ReportedName == "Staff Gauge Depth") 

depthdata <- df.wq %>% 
  filter(Parameter == "Staff Gauge Height") %>% 
  mutate(Parameter = "Water Depth")

if (nrow(df.wq %>% filter(Units == "f")) > 0) { # if units are feet
  depthdata <- depthdata %>% 
    mutate(Units = as.character("m"),
      FinalResult = round(as.numeric(ResultReported)/3.281, 1))
} else {
  depthdata <- depthdata
}

df.wq <- rbind(df.wq2, depthdata)

### Flag (numeric)
# Use similar function as to assign flags
df.wq$FlagCode <- NA
FLAG <- function(x) {
  if (str_detect(x, "<")) {
    104     # THEN set BDL (100 for all datasets except reservoir nutrients)
  } else if (str_detect(x, ">")){
      101     # THEN set to 101 for ADL
    } else {
      NA
    }
}
df.wq$FlagCode <- mapply(FLAG,x) %>% as.numeric()

  ### Storm Sample (Actually logical data - yes/no, but Access stores as -1 for yes and 0 for no. 1 bit
  # df.wq$StormSample <- 0 %>%  as.numeric()

### Storm SampleN (numeric)
# df.wq$StormSampleN <- NA %>% as.numeric

### Importdate (Date)
df.wq$ImportDate <- today()

########################################################################.
###                            Set IDs                              ####
########################################################################.

# Read Tables
# WQ
setIDs <- function(){
  query.wq <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{ImportTable}]"))
  # query.wq <- dbGetQuery(con, paste0("SELECT max(ID) FROM ", ImportTable))
  # Get current max ID
  if(is.na(query.wq)) {
    query.wq <- 0
  } else {
    query.wq <- query.wq
  }
  ID.max.wq <- as.numeric(unlist(query.wq))
  rm(query.wq)

  ### ID wq
  df.wq$ID <- seq.int(nrow(df.wq)) + ID.max.wq
}
df.wq$ID <- as.integer(setIDs())

# Flags
# First make sure there are flags in the dataset
setFlagIDs <- function(){
  if (all(is.na(df.wq$FlagCode)) == FALSE){ # Condition returns FALSE if there is at least 1 non-NA value, if so proceed
    query.flags <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{ImportFlagTable}]"))
    # Get current max ID
    if(is.na(query.flags)) {
      query.flags <- 0
    } else {
      query.flags <- query.flags
    }
    ID.max.flags <- as.numeric(unlist(query.flags))
    rm(query.flags)

    # Split the flags into a separate df and assign new ID

    df.flags <- as.data.frame(select(df.wq,c("ID","FlagCode"))) %>%
      rename("SampleID" = ID) %>%
      drop_na()

    ### ID flags
    df.flags$ID <- seq.int(nrow(df.flags)) + ID.max.flags
    df.flags$DataTableName <- ImportTable
    df.flags$DateFlagged = today()
    df.flags$ImportStaff = Sys.getenv("USERNAME")

    # Reorder df.flags columns to match the database table exactly # Add code to Skip if no df.flags
    df.flags <- df.flags[,c(3,4,1,2,5,6)]
  } else { # Condition TRUE - All FlagCodes are NA, thus no df.flags needed, assign NA
    df.flags <- NA
  } # End flags processing chunk
} # End set flags function
df.flags <- setFlagIDs()

########################################################################.
###                          Reformatting 2                         ####
########################################################################.

### Deselect Columns that do not need in Database
df.wq <- df.wq %>% select(-c(Description,
                             SampleDate,
                             date,
                             FlagCode,
                             Time
                             )
)

# Reorder remaining 32 columns to match the database table exactly
col.order.wq <- dbListFields(con, schema_name = schema, ImportTable)
df.wq <-  df.wq[,col.order.wq]

# Create a list of the processed datasets
dfs <- list()
dfs[[1]] <- df.wq
dfs[[2]] <- path
dfs[[3]] <- df.flags

# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)
return(dfs)
} # END FUNCTION

# #### COMMENT OUT WHEN RUNNING SHINY
# ########################################################################################################.
# # #RUN THE FUNCTION TO PROCESS THE DATA AND RETURN 2 DATAFRAMES and path AS LIST:
# dfs <- PROCESS_DATA(file, rawdatafolder, filename.db)
# 
# # Extract each element needed
# df.wq     <- dfs[[1]]
# path      <- dfs[[2]]
# df.flags  <- dfs[[3]]

########################################################################.
###                    Write data to Database                       ####
########################################################################.

IMPORT_DATA <- function(df.wq, df.flags = NULL, path, file, filename.db, processedfolder,ImportTable, ImportFlagTable = NULL){
  
  start <- now()
  dsn <- filename.db
  database <- "DCR_DWSP"
  schema <- 'Wachusett'
  tz <- 'America/New_York'
  
  con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

  # Import the data to the database - Need to use RODBC methods here. Tried odbc and it failed

  odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{ImportTable}")), value = df.wq, append = TRUE)
  
   ### Flag data ####
  if (class(df.flags) == "data.frame"){ # Check and make sure there is flag data to import 
    odbc::dbWriteTable(con, DBI::SQL(glue("{database}.{schema}.{ImportFlagTable}")), value = df.flags, append = TRUE)
  } else {
    print("There were no flags to import")
  }

  # Disconnect from db and remove connection obj
  dbDisconnect(con)
  rm(con)

  #Move the processed raw data file to the processed folder
  processed_subdir <- paste0("/", max(year(df.wq$DateTimeET))) # Raw data archived by year, subfolders = Year
  processed_dir <- paste0(processedfolder, processed_subdir)
  if(!file.exists(processed_dir)) {
    dir.create(processed_dir)
  }
  file.rename(path, paste0(processed_dir,"/", file))
  end <- now()
  return(print(glue("Import finished at {end}, \n elapsed time {round(end - start)} seconds")))  
  
}
### END

# IMPORT_DATA(df.wq, df.flags, path, file, filename.db, processedfolder,ImportTable, ImportFlagTable)
