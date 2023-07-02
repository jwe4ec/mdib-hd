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

# TODO: Deal with "prefer not to answer"





# Clean age

# TODO: One participant has NA (likely for "prefer not to answer")

sum(is.na(dem_dat$age)) == 1





  # TODO: Participant 814 has value of 0

dem_dat$record_id[!is.na(dem_dat$age) & dem_dat$age == 0] == 814





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

  # TODO (deal with prefer not to answer--see above): Reorder levels

dem_dat$race_coll <-
  factor(dem_dat$race_coll,
         levels = c("American Indian or Alaska Native", "Asian", "Black or African American", 
                    "Native Hawaiian or Other Pacific Islander", "White",
                    "More than one race", "Other", "Prefer not to answer"))

table(dem_dat$race_coll, useNA = "always")




