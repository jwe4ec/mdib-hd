# ---------------------------------------------------------------------------- #
# Clean Data
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

# Import HD MDIB data from RedCap

mdib_hd_dat <- read.csv("./data/bot_cleaned/final HD Aim 1 data_deid_2023-01-09_1525.csv")

# TODO: Clean data (see https://github.com/jwe4ec/ncats-r03/blob/main/code/2_clean_data.R)