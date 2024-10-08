###############################  HEADER  ######################################
#  TITLE: ImportMWRA.R
#  DESCRIPTION: This script will Format/Process MWRA data to DCR
#               This script will process and import MWRA Projects: WATTRB, WATTRN, MDCMNTH, WATBMP, QUABBIN, MDHEML
#  AUTHOR(S): Dan Crocker
#  DATE LAST UPDATED: 2020-01-21
#  GIT REPO: WIT
#  R version 3.5.3 (2019-03-11)  i386
##############################################################################.

# NOTE - THIS SECTION IS FOR TESTING THE FUNCTION OUTSIDE SHINY
# COMMENT OUT BELOW WHEN RUNNING FUNCTION IN SHINY

# # Load libraries needed ####
# 
# library(tidyverse)
# library(stringr)
# library(odbc)
# library(RODBC)
# library(DBI)
# library(lubridate)
# library(magrittr)
# library(readxl)
# library(testthat)
# library(glue)

# COMMENT OUT ABOVE CODE WHEN RUNNING IN SHINY!

########################################################################.
###                         PROCESSING FUNCTION                     ####
########################################################################.

PROCESS_DATA <- function(file, rawdatafolder, filename.db, probe = NULL, ImportTable, ImportFlagTable = NULL){ # Start the function - takes 1 input (File)
  # probe is an optional argument
options(scipen = 999) # Eliminate Scientific notation in numerical fields
# Get the full path to the file
path <- paste0(rawdatafolder,"/", file)

# Read in the data to a dataframe
df.wq <- read_excel(path, sheet = 1, col_names = T, trim_ws = T, na = "nil") %>%
  as.data.frame()   # This is the raw data - data comes in as xlsx file, so read.csv will not work
df.wq <- df.wq[,c(1:25)]

# Source WITQCTEST
source("src/Functions/WITQCTEST.R", local = TRUE)


########################################################################.
###                        Perform Data checks                      ####
########################################################################.

# At this point there could be a number of checks to make sure data is valid

  # Check to make sure there are 25 variables (columns)
  if (ncol(df.wq) != 25) {
    # Send warning message to UI if TRUE
    stop("There are not 25 columns of data in this file.\n Check the file before proceeding")
  }
  # Check to make sure column 1 is "Original Sample" or other?
  if (any(colnames(df.wq)[1] != "Original Sample" & df.wq[25] != "X Ldl")) {
    # Send warning message to UI if TRUE
    stop("At least 1 column heading is unexpected.\n Check the file before proceeding")
  }
  # Check to see if there were any miscellaneous locations that did not get assigned a location
  # if (length(which(str_detect(df.wq$Name, "WACHUSET-MISC"),TRUE)) > 0) {
  # #Send warning message to UI if TRUE
  #   warning("There are unspecified (MISC) - please review before importing data!")
  # }
  # Check to see if there were any GENERAL locations that did not get assigned a location
  if (length(which(str_detect(df.wq$Name, "GENERAL-GEN"),TRUE)) > 0) {
    # Send warning message to UI if TRUE
    stop("There are unspecified (GEN) locations that need to be corrected before importing data")
  }
# Any other checks?  Otherwise data is validated, proceed to reformatting...

### Connect to Database   
dsn <- filename.db
database <- "DCR_DWSP"
schema <- "Wachusett"
tz <- 'America/New_York'
con <- dbConnect(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)

########################################################################.
###                     START REFORMATTING THE DATA                 ####
########################################################################.

### Rename Columns in Raw Data ####
names(df.wq) <-  c("SampleGroup",
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



### Date and Time ####

# Split the Sample time into date and time
df.wq$SampleDate <- as_date(df.wq$SampleDate)
#df.wq$SampleTime[is.na(df.wq$SampleTime)] <- paste(df.wq$SampleDate[is.na(df.wq$SampleTime)])

df.wq <- separate(df.wq, SampleTime, into = c("date", "Time"), sep = " ")

# Merge the actual date column with the new Time Column and reformat to POSIXct
df.wq$DateTimeET <- as.POSIXct(paste(as.Date(df.wq$SampleDate, format ="%Y-%m-%d"), df.wq$Time, sep = " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York", usetz = T)

### Fix other data types ####
df.wq$EDEP_Confirm <- as.character(df.wq$EDEP_Confirm)
df.wq$EDEP_MW_Confirm <- as.character(df.wq$EDEP_Confirm)
df.wq$Comment <- as.character(df.wq$Comment)
df.wq$ResultReported <- as.character(df.wq$ResultReported)
df.wq$SampleGroup <- as.character(df.wq$SampleGroup)

if(all(!is.na(df.wq$LabRecDateET))) {
  df.wq$LabRecDateET <- as.POSIXct(paste(df.wq$LabRecDateET, format = "%Y-%m-%d %H:%M:SS", tz = "America/New_York", usetz = T))
} else {
  df.wq$LabRecDateET <- as_datetime(df.wq$LabRecDateET)
}

if(all(!is.na(df.wq$PrepOnET))) {
  df.wq$PrepOnET <- as.POSIXct(paste(df.wq$PrepOnET, format = "%Y-%m-%d %H:%M:SS", tz = "America/New_York", usetz = T))
} else {
  df.wq$PrepOnET <- as_datetime(df.wq$PrepOnET)
}

if(all(!is.na(df.wq$DateTimeAnalyzedET))) {
  df.wq$DateTimeAnalyzedET <- as.POSIXct(paste(df.wq$DateTimeAnalyzedET, format = "%Y-%m-%d %H:%M:SS", tz = "America/New_York", usetz = T))
} else {
  df.wq$DateTimeAnalyzedET <- as_datetime(df.wq$DateTimeAnalyzedET)
}
### Fix the Parameter names ####  - change from MWRA name to ParameterName
params <- dbReadTable(con,  Id(schema = schema, table = "tblParameters"))
df.wq$Parameter <- params$ParameterName[match(df.wq$Parameter, params$ParameterMWRAName)]

### Remove records with missing elements/uneeded data ####
# Delete possible Sample Address rows (Associated with MISC Sample Locations):
df.wq <- df.wq %>%  # Filter out any sample with no results (There shouldn't be, but they do get included sometimes)
  filter(!is.na(Parameter),
         !is.na(ResultReported))

df.wq <- df.wq %>% slice(which(!grepl("Sample Address", df.wq$Parameter, fixed = TRUE)))
df.wq <- df.wq %>% slice(which(!grepl("(DEP)", df.wq$Parameter, fixed = TRUE))) # Filter out rows where Parameter contains  "(DEP)"

# Need to generate a warning here if any status is X  - which means the results were tossed out and not approved... 
unapproved_data <- df.wq %>% slice(which(grepl("X", df.wq$Status, fixed = TRUE)))
if(nrow(unapproved_data > 0)) {
  stop("There seems to be unapproved data in this file (Status = 'X').\nThis data will be removed automatically. You should follow up with MWRA to obtain approved records.")
}

df.wq <- df.wq %>% slice(which(!grepl("X", df.wq$Status, fixed = TRUE))) # Filter out records where Status is X

# Check to make sure there is not storm sample data in the dataset (> 1 Loc/Date combination)
df_no_misc <- df.wq %>% filter(Location != "WACHUSET-MISC")
if (any(duplicated(paste0(df_no_misc$SampleDate, df_no_misc$Location, df_no_misc$Parameter)))) {
  # Send warning message to UI that it appears that there are storm samples in the data
  stop("There seems to be storm sample data in this file.\n There are more than 1 result for a parameter on a single day. 
         \nCheck the file before proceeding and split storm samples into separate file to import")
}

### Fix the Location names ####
df.wq$Location %<>%
  gsub("WACHUSET-","", .) %>%
  gsub("M754","MD75.4", .) %>% 
  gsub("BMP1","PRNW", .) %>%
  gsub("BMP2","HLNW", .) 

########################################################################.
###                           Add Unique ID                      ####
########################################################################.

### Unique ID number ####
df.wq$UniqueID <- NA_character_
df.wq$UniqueID <- paste(df.wq$Location, format(df.wq$DateTimeET, format = "%Y-%m-%d %H:%M"), params$ParameterAbbreviation[match(df.wq$Parameter, params$ParameterName)], sep = "_")

########################################################################.
###                           Check Duplicates                      ####
########################################################################.

## Make sure it is unique within the data file - if not then exit function and send warning
dupecheck <- which(duplicated(df.wq$UniqueID))
dupes <- df.wq$UniqueID[dupecheck] # These are the dupes

if (length(dupes) > 0){
  # Exit function and send a warning to userlength(dupes) # number of dupes
  stop(paste("This data file contains", length(dupes),
             "records that appear to be duplicates. Eliminate all duplicates before proceeding.",
             "The duplicate records include:", paste(head(dupes, 15), collapse = ", ")), call. = FALSE)
}
### Make sure records are not already in DB ####

Uniq <- dbGetQuery(con, glue("SELECT [UniqueID], [ID] FROM [{schema}].[{ImportTable}]"))
flags <- dbGetQuery(con, glue("SELECT [SampleID], [FlagCode] FROM [{schema}].[{ImportFlagTable}] WHERE FlagCode = 102"))
dupes2 <- Uniq[Uniq$UniqueID %in% df.wq$UniqueID,]
dupes2 <- filter(dupes2, !ID %in% flags$SampleID) # take out any preliminary samples (they should get overwritten during import)

if (nrow(dupes2) > 0){
  # Exit function and send a warning to user
  stop(paste("This data file contains", nrow(dupes2),
             "records that appear to already exist in the database!
Eliminate all duplicates before proceeding.",
             "The duplicate records include:", paste(head(dupes2$UniqueID, 15), collapse = ", ")), call. = FALSE)
}
rm(Uniq)

### DataSource ####
df.wq <- df.wq %>% 
  mutate(DataSource = paste0("MWRA_", file),
         Imported_By = username,
         QAQC_By = NA_character_)

### DataSourceID ####
# Do some sorting first:
df.wq <- df.wq[with(df.wq, order(DateTimeET, Location, Parameter)),]

# Assign the numbers
df.wq$DataSourceID <- seq(1, nrow(df.wq), 1)

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
    as.numeric(gsub("<","", x), digits =4)  # THEN strip "<" from reported result, make numeric
  } else if (str_detect(x, ">")){
      as.numeric(gsub(">","", x)) # THEN strip ">" form reported result, make numeric.
    } else {
      as.numeric(x)
    }# ELSE THEN just use Result Reported for Result and make numeric
}

df.wq$FinalResult <- mapply(FR,x) %>%
  round(digits = 4)

### Flag (numeric) ####
# Use similar function as to assign flags
df.wq$FlagCode <- NA
FLAG <- function(x) {
  if (str_detect(x, "<")) {
    104     # THEN set EDL (104 for all datasets except reservoir nutrients)
  } else if (str_detect(x, ">")){
      101     # THEN set to 101 for ADL
    } else {
      NA
    }
}
df.wq$FlagCode <- mapply(FLAG,x) %>% as.numeric()

### Flagging Duplicates ####

# Get duplicates
dups <- df.wq %>% filter(Location %in% c("WFD1","WFD2","WFD3")) %>%
  rename(Duplicate = Location) %>%
  mutate(Date = as.Date(DateTimeET))

# Get table to find which sites duplicates match with, needed for blanks too, so can't be under next If()
dup_df <- dbReadTable(con, Id(schema = schema, table = "tbl_Field_QC"))

# Only proceed if there are duplicates
if(nrow(dups)>0) {
  

  dup_df_rename <- dup_df %>% rename(Duplicate = Dup_Blank_code)
  
  # Create temporary dataframe to match regular samples with dups
  dups <- inner_join(dups, dup_df_rename, by=c("Date","Duplicate")) %>% 
    rename(Location = MWRA_Location,
           UniqueID_QC = UniqueID) %>%
    select(Date, Duplicate, Location, Parameter, Units, FinalResult, UniqueID_QC)
  
  # Add date to df.wq which follows through the whole code
  df.wq.date <- df.wq %>% mutate(Date = as.Date(DateTimeET))
  
  # Combine the duplicate dataframe with the full dataframe
  dups_combined <- inner_join(dups, df.wq.date, by=c("Date","Location","Parameter","Units")) %>%
    rename(TribResult = "FinalResult.y",
           DupResult = "FinalResult.x")
  
  ### Calculating RPD for bacteria dups and whether it passes. Uses BACT_DUP_TEST function from WITQCTEST.R
  bact_dups <- dups_combined %>% 
    filter(Parameter == "E. coli") %>%
    mutate(Log10DupResult = log10(DupResult),
           Log10TribResult = log10(TribResult),
           RPD = round(((abs(Log10TribResult-Log10DupResult)/((Log10TribResult+Log10DupResult)/2))*100),digits=1),
           Pass = BACT_DUP_TEST(TribResult, DupResult, RPD))
  
  ### Calculating RPD for non-bacteria dups and whether they pass
  dups_other <- dups_combined %>% 
    filter(!Parameter %in% c("E. coli", "Dissolved Oxygen", "Water Temperature","Oxygen Saturation")) %>%
    mutate(RPD = round(((abs(TribResult-DupResult)/((TribResult+DupResult)/2))*100),digits=1),
           Pass = if_else(RPD>30, "FAIL","PASS"))
  
  ### Combine dataframes once Pass/Fail determined
  dups_all <- bind_rows(bact_dups, dups_other)

  ### Filter out only the fails
  dups_fail <- filter(dups_all, Pass == "FAIL") 
  if (nrow(dups_fail > 0)) {
    ## Get UniqueIDs of the failed dups
    failed_dups <- dups_fail$UniqueID_QC
    
    ## Create unique identifer with Parameter and Date to help match with other samples collected that day
    dups_fail <- dups_fail %>% dplyr::select(Parameter, Date) %>%
      mutate(ParameterDate = paste0(Parameter,"_",Date))
    
    ## Only keep unique Parameter/Date combos
    dups_fail_param_date <- unique(dups_fail$ParameterDate)
    
    ## Flag failed dups with 129
    ## Flag samples on same date with same parameter as failed dup as 127
    df.wq <- df.wq %>% 
      rowwise() %>% 
      mutate(Date = as.Date(DateTimeET),
             ParameterDate = paste0(Parameter,"_",Date),
             DupFlags = ifelse(startsWith(Location,"M") & (ParameterDate %in% dups_fail_param_date), 127, 
                               ifelse(UniqueID %in% failed_dups, 129, NA)))
  } else {
    print("There were no failed duplicates to flag")
    df.wq <- df.wq %>% 
      mutate(Date = as.Date(DateTimeET),
             ParameterDate = paste0(Parameter,"_",Date))
  }
  
} else {
  
  # If no duplicates in dataset, DupFlags column is empty
  df.wq <- df.wq %>% 
    mutate(DupFlags = NA_integer_)
}

### Flagging Blanks ####

#Find Blanks in dataset
blanks <- df.wq %>% filter(Location %in% c("WFB1","WFB2")) %>%
  rename(Blank = Location) %>%
  mutate(Date = as.Date(DateTimeET))

#Only proceed if blanks exist
if(nrow(blanks)>0) {
  blank_df_rename <- dup_df %>% rename(Blank = Dup_Blank_code)
  
  blanks <- inner_join(blanks, blank_df_rename, by=c("Date","Blank")) %>% 
    rename(Location = MWRA_Location,
           UniqueID_QC = UniqueID) 
  
  # Blanks fail if greater than given value, else they pass. Values are mostly the low end of the "dynamic range" in MWRA SOPs
  blanks <- blanks %>% 
    mutate(Pass = 
             case_when((FinalResult > 1 & Parameter == "Turbidity NTU") ~ "FAIL",
                       (FinalResult > 10 & Parameter == "E. coli") ~ "FAIL",
                       (FinalResult > 0.5 & Parameter == "Alkalinity") ~ "FAIL",
                       (FinalResult > 0.005 & Parameter == "Ammonia-N") ~ "FAIL",
                       (FinalResult > 0.5 & Parameter == "Chloride") ~ "FAIL",
                       (FinalResult > 0.005 & Parameter == "Mean UV254") ~ "FAIL",
                       (FinalResult > 0.005 & Parameter == "Nitrate-N") ~ "FAIL",
                       (FinalResult > 0.005 & Parameter == "Nitrite-N") ~ "FAIL",
                       (FinalResult > 1 & Parameter == "Total Organic Carbon") ~ "FAIL",
                       (FinalResult > 0.1 & Parameter == "Total Kjeldahl Nitrogen") ~ "FAIL",
                       (FinalResult > 0.005 & Parameter == "Total Phosphorus") ~ "FAIL",
                       (FinalResult > 5 & Parameter == "Total Suspended Solids") ~ "FAIL",
                       TRUE ~ "PASS"))

  
  # Get only failed blanks
  blanks_fail <- filter(blanks, Pass == "FAIL")
  
  # Get UniqueIDs of failed blanks
  failed_blanks <- blanks_fail$UniqueID_QC
  
  # Calculate new field for Parameter/Date combo for failed blanks
  blanks_fail <- blanks_fail %>% dplyr::select(Parameter, Date) %>%
    mutate(ParameterDate = paste0(Parameter,"_",Date))
  
  # Only keep unique values of Parameter/Date
  blanks_fail_param_date <- unique(blanks_fail$ParameterDate)
  
  # Add flag 130 for failed blanks
  # Add flag 128 for samples sampled on same day with same parameter as a failed blank
  df.wq <- df.wq %>% 
    rowwise() %>% 
    mutate(Date = as.Date(DateTimeET),
           BlankFlags = if_else(startsWith(Location,"M") & (ParameterDate %in% blanks_fail_param_date), 128, 
                                if_else(UniqueID %in% failed_blanks, 130, NA)))
  
} else {
  # If no blanks in data, BlankFlags column is empty
  df.wq <- df.wq %>% 
    mutate(BlankFlags <- NA_integer_)
}

### Storm SampleN (numeric) ####
df.wq$StormSampleN <- NA_character_

### Import date (Date) ####
df.wq$ImportDate <- Sys.Date() %>% force_tz("America/New_York")

########################################################################.
###                      REMOVE ANY PRELIMINARY DATA                ####
########################################################################.

### Get Locations table to generate list of applicable locations
locations <- dbReadTable(con,  Id(schema = schema, table = "tblWatershedLocations"))

### Filter down to only locations with preliminary bacteria (Primary/secondary Tribs and Transect)
prelim_locs <- locations %>% 
  filter(LocationType %in% c("Tributary", "Transect"),
         LocationCategory != "Long-term Forestry") %>% 
  pull(LocationMWRA)

# Calculate the date range of import ####
datemin <- min(df.wq$DateTimeET)
datemax <- max(df.wq$DateTimeET)

# IDs to look for - all records in time period in question
qry <- glue("SELECT [ID],[Location] FROM [{schema}].[{ImportTable}] WHERE [DateTimeET] >= '{datemin}' AND [DateTimeET] <= '{datemax}'")
query.prelim <- dbGetQuery(con, qry) # This generates a list of possible IDs

query.prelim <- query.prelim %>% 
  filter(Location %in% prelim_locs) %>% 
  pull(ID)

if (length(query.prelim) > 0) {# If true there is at least one record in the time range of the data
  # SQL query that finds matching record ID from tblSampleFlagIndex Flagged 102 within date range in question
  qryS <- sprintf("SELECT [SampleID] FROM [Wachusett].[tblTribFlagIndex] WHERE [FlagCode] = 102 AND [SampleID] IN (%s)", paste(as.character(query.prelim), collapse=', '))
  qryDelete <- dbGetQuery(con, qryS) # Check the query to see if it returns any matches
  
  # If there are matching records then delete preliminary data (IDs flagged 102 in period of question)
  if(nrow(qryDelete) > 0) {
    ### Delete from flag table
    qryDeletePrelimFlags <- sprintf(glue("DELETE FROM [{schema}].[{ImportFlagTable}] WHERE [DataTableName] = 'tblMWRAResults' AND [SampleID] IN (%s)"), paste(as.character(query.prelim), collapse=', '))
    rs <- dbSendStatement(con, qryDeletePrelimFlags)
    print(paste(dbGetRowsAffected(rs), "preliminary record data flags were deleted during this import", sep = " "))
    dbClearResult(rs)
    ### Delete from tblMWRAResults
    qryDeletePrelimData <- sprintf(glue("DELETE FROM [{schema}].[{ImportTable}] WHERE [ID] IN (%s)"), paste(as.character(query.prelim), collapse=', '))
    rs <- dbSendStatement(con,qryDeletePrelimData)
    print(paste(dbGetRowsAffected(rs), "preliminary records were deleted during this import", sep = " ")) # Need to display this message to the Shiny UI
    dbClearResult(rs)
  }
}

########################################################################.
###                               SET IDs                           ####
########################################################################.

# Read Tables
# WQ ####
setIDs <- function(){
query.wq <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{ImportTable}]"))
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
df.wq$ID <- setIDs()

### Flags ####

# First make sure there are flags in the dataset
setFlagIDs <- function(){
  if(all(is.na(df.wq$FlagCode)) == FALSE){ # Condition returns FALSE if there is at least 1 non-NA value, if so proceed
  # Split the flags into a separate df and assign new ID
  df.flags.censored <- as.data.frame(select(df.wq, c("ID","FlagCode"))) %>%
    rename("SampleID" = ID) %>%
    drop_na()
  } else {
    df.flags.censored <- as.data.frame(NULL)
  }
 
  ##Dup flags
  if(all(is.na(df.wq$DupFlags)) == FALSE){ # Condition returns FALSE if there is at least 1 non-NA value, if so proceed
    # Split the flags into a separate df and assign new ID
    df.flags.dup <- as.data.frame(select(df.wq, c("ID","DupFlags"))) %>%
      rename("SampleID" = ID,
             "FlagCode" = DupFlags) %>%
      drop_na()
  } else {
    df.flags.dup <- as.data.frame(NULL)
  }
  
  ##Blank flags
  if(all(is.na(df.wq$BlankFlags)) == FALSE){ # Condition returns FALSE if there is at least 1 non-NA value, if so proceed
    # Split the flags into a separate df and assign new ID
    df.flags.blk <- as.data.frame(select(df.wq, c("ID","BlankFlags"))) %>%
      rename("SampleID" = ID,
             "FlagCode" = BlankFlags) %>%
      drop_na()
  } else {
    df.flags.blk <- as.data.frame(NULL)
  } 
  
  # Combine all flag dataframes (if one is empty, it'll be a blank dataframe that will not add a new row)
  df.flags <- bind_rows(df.flags.censored, df.flags.dup, df.flags.blk)
  
  
  if(nrow(df.flags) > 0){
      query.flags <- dbGetQuery(con, glue("SELECT max(ID) FROM [{schema}].[{ImportFlagTable}]"))
      # Get current max ID
      if(is.na(query.flags)) {
        query.flags <- 0
      } else {
        query.flags <- query.flags
      }
      ID.max.flags <- as.numeric(unlist(query.flags))
      rm(query.flags)
      
    ### ID flags ###
      df.flags$ID <- seq.int(nrow(df.flags)) + ID.max.flags
      df.flags$DataTableName <- ImportTable
      df.flags$DateFlagged <-  Sys.Date() %>% force_tz("America/New_York")
      df.flags$ImportStaff <-  username
      df.flags$Comment <- "Flag automatically added at import"
    
      # Reorder df.flags columns to match the database table exactly # Add code to Skip if no df.flags
      df.flags <- df.flags[,c(3,4,1,2,5,6,7)]
  } else { # Condition TRUE - All FlagCodes are NA, thus no df.flags needed, assign NA
    df.flags <- NA
  } # End flags processing chunk
} # End set flags function
df.flags <- setFlagIDs()

########################################################################.
###                        Check for unmatched times                ####
########################################################################.

### Check for sample location/time combination already in database. Create dataframe for records with no matches (possible time fix needed)

#Create empty dataframe
unmatchedtimes <- df.wq[NULL, names(df.wq)]
# Bring in tributary location IDs
locations.tribs <- na.omit(dbGetQuery(con, glue("SELECT [LocationMWRA] FROM [{schema}].[tblWatershedLocations] WHERE [LocationType] ='Tributary'")))

# Keep only locations of type "Tributary", remove WFB2 since it will always have unmatched times
df.timecheck <- dplyr::filter(df.wq, Location %in% locations.tribs$LocationMWRA, !Location %in% c("MISC","WFB2"))

rm(locations.tribs)

# Only do the rest of the unmatched times check if there are tributary locations in data being processed
if(nrow(df.timecheck)>0){

  # Find earliest date in df.wq
  mindatecheck <- min(df.wq$DateTimeET)
  # Retrieve all date/times from database from earliest in df.wq to present - from Field Parameter table
  databasetimes <- dbGetQuery(con, glue("SELECT [DateTimeET], [Location] FROM [{schema}].[tblTribFieldParameters] WHERE [DateTimeET] >= '{mindatecheck}'"))  
    
  #Loop adds row for every record without matching location/date/time in database
  for (i in 1:nrow(df.timecheck)){
    if ((df.timecheck$DateTimeET[i] %in% dplyr::filter(databasetimes, Location == df.timecheck$Location[i])$DateTimeET) == FALSE){
      unmatchedtimes <- bind_rows(unmatchedtimes,df.timecheck[i,])
   }}

  rm(mindatecheck, databasetimes)

}  

### Print unmatchedtimes to log, if present
if (nrow(unmatchedtimes) > 0){
  print(paste0(nrow(unmatchedtimes)," unmatched site/date/times in processed data."))
  print(unmatchedtimes[c("ID","UniqueID","ResultReported")], print.gap = 4, right = FALSE)
}

########################################################################.
###                           Final Reformatting                    ####
########################################################################. 

### Deselect Columns that do not need in Database ####
# df.wq <- df.wq %>% select(-c(Description,
#                              SampleDate,
#                              FlagCode,
#                              date,
#                              Time,
#                              DupFlags,
#                              BlankFlags,
#                              Date,
#                              ParameterDate
#                              )
# )

# Reorder remaining 30 columns to match the database table exactly ####
col.order.wq <- dbListFields(con, schema_name = schema, name = ImportTable)
df.wq <-  df.wq[,col.order.wq]

### QC Test ####
source("src/Functions/WITQCTEST.R", local = T)

qc_message <- QCCHECK( df.qccheck = df.wq, 
                       file = file, 
                       ImportTable = ImportTable)
print(qc_message)

### Create a list of the processed datasets ####
dfs <- list()
dfs[[1]] <- df.wq
dfs[[2]] <- path
dfs[[3]] <- df.flags # Removed condition to test for flags and put it in the setFlagIDS() function
dfs[[4]] <- unmatchedtimes # Samples with site/time combo not matching any record in the database

# Disconnect from db and remove connection obj
dbDisconnect(con)
rm(con)
return(dfs)
} # END FUNCTION ####

############## COMMENT OUT SECTION BELOW WHEN RUNNING SHINY #############.
# #RUN THE FUNCTION TO PROCESS THE DATA AND RETURN 2 DATAFRAMES and path AS LIST:
# dfs <- PROCESS_DATA(file, rawdatafolder, filename.db, ImportTable = ImportTable, ImportFlagTable = ImportFlagTable)
# #
# # # Extract each element needed
# df.wq     <- dfs[[1]]
# path      <- dfs[[2]]
# df.flags  <- dfs[[3]]
# unmatchedtimes <- dfs[[4]]


########################################################################.
###                        Write data to Database                   ####
########################################################################.


IMPORT_DATA <- function(df.wq, df.flags = NULL, path, file, filename.db, processedfolder, ImportTable, ImportFlagTable = NULL){
  start <- now()
  print(glue("Starting data import at {start}"))
  ### CONNECT TO DATABASE ####
  ### Set DB
  dsn <- filename.db
  database <- "DCR_DWSP"
  schema <- 'Wachusett'
  tz <- 'America/New_York'
  ### Connect to Database 
  pool <- dbPool(odbc::odbc(), dsn = dsn, uid = dsn, pwd = config[["DB Connection PW"]], timezone = tz)
  
  poolWithTransaction(pool, function(conn) {
    pool::dbWriteTable(pool, DBI::Id(schema = schema, table = ImportTable),value = df.wq, append = TRUE, row.names = FALSE)
  })
  
  ### Flag data ####
  if (class(df.flags) == "data.frame"){ # Check and make sure there is flag data to import 
    poolWithTransaction(pool, function(conn) {
      pool::dbWriteTable(pool, DBI::Id(schema = schema, table = ImportFlagTable), value = df.flags, append = TRUE, row.names = FALSE)
    })
  } else {
    print("There were no flags to import")
  }
  
  #* Close the database pool ----
  poolClose(pool)
  rm(pool)

  ### Move the processed raw data file to the processed folder ####
  processed_subdir <- paste0("/", max(year(df.wq$DateTimeET))) # Raw data archived by year, subfolders = Year
  processed_dir <- paste0(processedfolder, processed_subdir)
  if(!file.exists(processed_dir)) {
    dir.create(processed_dir)
  }
    
  file.rename(path, paste0(processed_dir,"/", file))

  end <- now()
  return(print(glue("Import finished at {end}, \n elapsed time {round(end - start)} seconds")))  
}
### END ####

# IMPORT_DATA(df.wq, df.flags, path, file, filename.db, processedfolder)
