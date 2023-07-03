# ---------------------------------------------------------------------------- #
# Clean Demographic Data and Create Table
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/1_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/further_clean/mdib_hd_dat.RData")

# ---------------------------------------------------------------------------- #
# Clean demographic data ----
# ---------------------------------------------------------------------------- #

# Restrict to demographic columns of interest at baseline

index_cols <- c("record_id", "redcap_event_name")

dem_cols <- c("age", "race___1", "race___2", "race___3", "race___4", "race___5", 
              "race___9", "race___99", "race_other", "ethnicity", "country", 
              "relationship_status", "live_alone", "gender", "sex", 
              "employment_status", "education")

dem_dat <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == "baseline_arm_1", 
                       c(index_cols, dem_cols)]

# Clean age

  # Note: No participants entered "99"

sum(dem_dat$age == 99, na.rm = TRUE) == 0

  # TODO: Participant 120 has NA, 0, or "" on all demographics (likely due to attrition)

sum(is.na(dem_dat$age)) == 1
# View(mdib_hd_dat[mdib_hd_dat$record_id == 120, ])





  # TODO (Was this how pna was coded for age? Note that data dictionary says
  # age was computed from "date_of_birth". Did they choose pna for that?): 
  # Participant 814 has value of 0

dem_dat$record_id[!is.na(dem_dat$age) & dem_dat$age == 0] == 814
# View(mdib_hd_dat[mdib_hd_dat$record_id == 814, ])




# Clean race

  # Note: No values of "other" or "prefer not to answer"

all(dem_dat[, c("race___9", "race___99")] == 0)
sum(!is.na(dem_dat$race_other)) == 0

  # Create a single column "race_coll", collapsing cases of more than once race

dem_dat$race_coll <- NA

race_cols_not_pna <- paste0("race___", c(1:5, 9))

for (i in 1:nrow(dem_dat)) {
  if (rowSums(dem_dat[i, race_cols_not_pna]) <= 1) {
    dem_dat$race_coll[[i]][dem_dat$race___1[[i]]  == 1] <- "American Indian or Alaska Native"
    dem_dat$race_coll[[i]][dem_dat$race___2[[i]]  == 1] <- "Asian"
    dem_dat$race_coll[[i]][dem_dat$race___3[[i]]  == 1] <- "Black or African American"
    dem_dat$race_coll[[i]][dem_dat$race___4[[i]]  == 1] <- "Native Hawaiian or Other Pacific Islander"
    dem_dat$race_coll[[i]][dem_dat$race___5[[i]]  == 1] <- "White"
    dem_dat$race_coll[[i]][dem_dat$race___9[[i]]  == 1] <- "Other"
    dem_dat$race_coll[[i]][dem_dat$race___99[[i]] == 1] <- "Prefer not to answer"
  } else if (rowSums(dem_dat[i, race_cols_not_pna]) > 1) {
    dem_dat$race_coll[[i]] <- "More than one race"
  }
}

  # Reorder levels

dem_dat$race_coll <-
  factor(dem_dat$race_coll,
         levels = c("American Indian or Alaska Native", "Asian", "Black or African American", 
                    "Native Hawaiian or Other Pacific Islander", "White",
                    "More than one race", "Other", "Prefer not to answer"))

# Clean ethnicity

dem_dat$ethnicity <- 
  factor(dem_dat$ethnicity, levels = 1:4,
         labels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown", 
                    "Prefer not to answer"))

# Clean country

  # Recode variations of United States

united_states <- c("America", "U.S", "U.S.", "united states", "United States",
                   "United states ", "United States ", "United States of America",
                   "US", "usa", "Usa", "USA", "USA ")

dem_dat$country[dem_dat$country %in% united_states] <- "United States"

  # Recode one participant's value of "" with NA

dem_dat$country[dem_dat$country == ""] <- NA

  # Participant 114 responded with a county (TODO: REDACT) in the United States

dem_dat$country[dem_dat$record_id == 114] <- "United States"

# Clean relationship status

dem_dat$relationship_status <- 
  factor(dem_dat$relationship_status, levels = c(0:9, 99),
         labels = c("Single", "Single, but casually dating",
                    "Single, but currently engaged to be married",
                    "Single, but currently living with someone in a marriage-like relationship",
                    "Married", "In a domestic or civil union", "Separated", "Divorced", 
                    "Widow/widower", "Other", "Prefer not to answer"))

# Clean living alone

dem_dat$live_alone <-
  factor(dem_dat$live_alone, levels = c(0:1, 99),
         labels = c("No", "Yes", "Prefer not to answer"))

# Clean gender identity

dem_dat$gender <-
  factor(dem_dat$gender, levels = c(0:4, 99),
         labels = c("Female", "Male", "Transgender Female", "Transgender Male",
                    "Other", "Prefer not to answer"))

# Clean sex assigned at birth

dem_dat$sex <-
  factor(dem_dat$sex, levels = c(0:3, 99),
         labels = c("Female", "Male", "Intersex", "Unknown or other", 
                    "Prefer not to answer"))

# Clean employment status

dem_dat$employment_status <-
  factor(dem_dat$employment_status, levels = c(0:8, 99),
         labels = c("Working full-time", "Working part-time", "Unemployed or laid off",
                    "Looking for work", "Homemaker/keeping house or raising children full-time",
                    "Retired", "Student", "Other", "Unknown", "Prefer not to answer"))

# Clean education

dem_dat$education <-
  factor(dem_dat$education, levels = c(1:8, 99),
         labels = c("Elementary School", "Junior High", "High School", "Some College",
                    "Associate's Degree", "Bachelor's Degree", "Master's Degree",
                    "Doctorate/ PhD", "Prefer not to answer"))

# ---------------------------------------------------------------------------- #
# Save cleaned data ----
# ---------------------------------------------------------------------------- #

save(dem_dat, file = "./data/further_clean/dem_dat.RData")

# ---------------------------------------------------------------------------- #
# Create demographics table ----
# ---------------------------------------------------------------------------- #

# TODO: Define function to compute descriptives
# (see https://github.com/jwe4ec/fy7e6/blob/develop/code/04_further_clean_demog_data_create_tables.R)





