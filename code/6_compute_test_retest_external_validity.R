# ---------------------------------------------------------------------------- #
# Compute Test-Retest Reliability and External Validity
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

# Load packages

pkgs <- c("cocor", "rockchalk")
groundhog.library(pkgs, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/further_clean/mdib_hd_dat2.RData")

# ---------------------------------------------------------------------------- #
# Compute test-retest reliability ----
# ---------------------------------------------------------------------------- #

# TODO





# ---------------------------------------------------------------------------- #
# Compute convergent, discriminant, concurrent, and predictive validity ----
# ---------------------------------------------------------------------------- #

# TODO





# ---------------------------------------------------------------------------- #
# Compute incremental validity ----
# ---------------------------------------------------------------------------- #

# TODO




