# ---------------------------------------------------------------------------- #
# Run Exploratory Analyses
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

source("./code/1a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/further_clean/mdib_hd_dat2_short_wide.RData")

# ---------------------------------------------------------------------------- #
# Inspect distribution of Neuro-QoL Anxiety ----
# ---------------------------------------------------------------------------- #

hist(mdib_hd_dat2_short_wide$neuroqol_anx_m.baseline)
hist(mdib_hd_dat2_short_wide$neuroqol_anx_m.followup)