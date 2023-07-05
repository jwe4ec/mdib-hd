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

source("./code/1a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import HD MDIB data from RedCap

mdib_hd_dat <- read.csv("./data/bot_cleaned/final HD Aim 1 data_deid_2023-01-09_1525_v2.csv")

# ---------------------------------------------------------------------------- #
# Remove blank rows ----
# ---------------------------------------------------------------------------- #

# For rows where "record_id" is NA, all columns are NA or "". Remove such rows.
# Note: These no longer appear; perhaps due to update to R (vs. R 4.2.2 used in 
# prior script at https://github.com/jwe4ec/ncats-r03)

sum(is.na(mdib_hd_dat$record_id)) == 237

sum(!(is.na(mdib_hd_dat[is.na(mdib_hd_dat$record_id), ]) |
        mdib_hd_dat[is.na(mdib_hd_dat$record_id), ] == "")) == 0

mdib_hd_dat <- mdib_hd_dat[!is.na(mdib_hd_dat$record_id), ]

# ---------------------------------------------------------------------------- #
# Remove participants ----
# ---------------------------------------------------------------------------- #

# Per Jessie Gibson, 7 participants consented to participate but never started 
# the surveys. These participants have values of NA, 0, or "" for all columns at 
# baseline. Remove these participants.

target_cols <- names(mdib_hd_dat)[!(names(mdib_hd_dat) %in% c("record_id", "redcap_event_name"))]

row_never_started <- vector(length = nrow(mdib_hd_dat))

for (i in 1:nrow(mdib_hd_dat)) {
  row_never_started[[i]] <- mdib_hd_dat$redcap_event_name[[i]] == "baseline_arm_1" &
    all(apply(mdib_hd_dat[i, target_cols], 2, function(x) is.na(x) | x %in% c(0, "")))
}

nrow(mdib_hd_dat[row_never_started, ]) == 7

mdib_hd_dat <- mdib_hd_dat[!row_never_started, ]

# Per Jessie Gibson on 7/5/2023, remove 3 participants who should have been removed 
# as part of bot cleaning, leaving 70 participants (but see further exclusions below)

exclude_ids <- c(23, 738, 814)

mdib_hd_dat <- mdib_hd_dat[!(mdib_hd_dat$record_id %in% exclude_ids), ]

length(unique(mdib_hd_dat$record_id)) == 70

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# Define items for negative bias (MDIB, BBSIQ; and benign bias for each just for
# reference), anxiety sensitivity (ASI), fear of negative evaluation (BFNE-II), 
# anxiety symptoms (Neuro-QoL Anxiety), social avoidance and distress (full SADS 
# at baseline and reduced SADS at follow-up), and alcohol use (AUDIT-C)

mdib_neg_items <- 
  names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat)) & grepl("neg", names(mdib_hd_dat))]
mdib_ben_items <- 
  names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat)) & grepl("benign", names(mdib_hd_dat))]

  # Note: The object for BBSIQ items is appended with "mdib" because prior analyses
  # (https://github.com/jwe4ec/pa-20-206) found that BBSIQ item names differ between
  # the MDIB and MindTrails-HD Data Server datasets. Although the MT-HD dataset is
  # not relevant to the present analyses, we retain the label for clarity.

bbsiq_neg_items_mdib <- 
  names(mdib_hd_dat)[grepl("bbsiq", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat)) &
                       grepl("neg", names(mdib_hd_dat))]
bbsiq_ben_items_mdib <- 
  names(mdib_hd_dat)[grepl("bbsiq", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat)) &
                       grepl("benign", names(mdib_hd_dat))]

asi_items <- names(mdib_hd_dat)[grepl("asi_", names(mdib_hd_dat))]

bfne2_items <- names(mdib_hd_dat)[grepl("bfne_", names(mdib_hd_dat))]

neuroqol_anx_items <-
  names(mdib_hd_dat)[grepl("neuroqol", names(mdib_hd_dat)) & !(grepl("complete", names(mdib_hd_dat)))]

sads_items <- names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & !(grepl("_v2", names(mdib_hd_dat)))]

sads_red_items <- names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & grepl("_v2", names(mdib_hd_dat))]

auditc_items <- names(mdib_hd_dat)[grepl("alcohol_audit_c", names(mdib_hd_dat))]

all(mdib_neg_items == c("md_bbsiq_1b_neg", "md_bbsiq_2a_neg", "md_bbsiq_3c_neg", 
                        "md_bbsiq_4c_neg", "md_bbsiq_5a_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_8b_neg", "md_bbsiq_9c_neg", 
                        "md_bbsiq_10a_neg", "md_bbsiq_11c_neg", "md_bbsiq_12b_neg"))

all(mdib_ben_items == c("md_bbsiq_1a_benign", "md_bbsiq_1c_benign", "md_bbsiq_2b_benign", 
                        "md_bbsiq_2c_benign", "md_bbsiq_3a_benign", "md_bbsiq_3b_benign", 
                        "md_bbsiq_4a_benign", "md_bbsiq_4b_benign", "md_bbsiq_5b_benign", 
                        "md_bbsiq_5c_benign", "md_bbsiq_6a_benign", "md_bbsiq_6c_benign", 
                        "md_bbsiq_7b_benign", "md_bbsiq_7c_benign", "md_bbsiq_8a_benign", 
                        "md_bbsiq_8c_benign", "md_bbsiq_9a_benign", "md_bbsiq_9b_benign", 
                        "md_bbsiq_10b_benign", "md_bbsiq_10c_benign", "md_bbsiq_11a_benign", 
                        "md_bbsiq_11b_benign", "md_bbsiq_12a_benign", "md_bbsiq_12c_benign"))

all(bbsiq_neg_items_mdib == c("bbsiq_1c_neg", "bbsiq_2b_neg", "bbsiq_3c_neg", 
                              "bbsiq_4c_neg", "bbsiq_5a_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_8c_neg", "bbsiq_9b_neg", 
                              "bbsiq_10b_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_13c_neg", "bbsiq_14c_neg"))

all(bbsiq_ben_items_mdib == c("bbsiq_1a_benign", "bbsiq_1b_benign", "bbsiq_2a_benign", 
                              "bbsiq_2c_benign", "bbsiq_3a_benign", "bbsiq_3b_benign", 
                              "bbsiq_4a_benign", "bbsiq_4b_benign", "bbsiq_5b_benign", 
                              "bbsiq_5c_benign", "bbsiq_6b_benign", "bbsiq_6c_benign", 
                              "bbsiq_7a_benign", "bbsiq_7c_benign", "bbsiq_8a_benign", 
                              "bbsiq_8b_benign", "bbsiq_9a_benign", "bbsiq_9c_benign", 
                              "bbsiq_10a_benign", "bbsiq_10c_benign", "bbsiq_11a_benign", 
                              "bbsiq_11c_benign", "bbsiq_12b_benign", "bbsiq_12c_benign", 
                              "bbsiq_13a_benign", "bbsiq_13b_benign", "bbsiq_14a_benign", 
                              "bbsiq_14b_benign"))

all(asi_items == c("asi_1", "asi_2", "asi_3", "asi_4", "asi_5", "asi_6", "asi_7", 
                   "asi_8", "asi_9", "asi_10", "asi_11", "asi_12", "asi_13", "asi_14", 
                   "asi_15", "asi_16"))

all(bfne2_items == c("bfne_1", "bfne_2", "bfne_3", "bfne_4", "bfne_5", "bfne_6", 
                     "bfne_7", "bfne_8", "bfne_9", "bfne_10", "bfne_11", "bfne_12"))

all(neuroqol_anx_items == c("neuroqol_edanx53", "neuroqol_edanx46", "neuroqol_edanx48", 
                            "neuroqol_edanx41", "neuroqol_edanx54", "neuroqol_edanx55", 
                            "neuroqol_edanx18", "neuroqol_nqanx07"))

all(sads_items == c("sad_1", "sad_2", "sad_3", "sad_4", "sad_5", "sad_6", "sad_7", 
                    "sad_8", "sad_9", "sad_10", "sad_11", "sad_12", "sad_13", "sad_14", 
                    "sad_15", "sad_16", "sad_17", "sad_18", "sad_19", "sad_20", "sad_21", 
                    "sad_22", "sad_23", "sad_24", "sad_25", "sad_26", "sad_27", "sad_28"))

all(sads_red_items == c("sad_20_v2", "sad_27_v2", "sad_13_v2", "sad_12_v2", 
                        "sad_24_v2", "sad_15_v2", "sad_4_v2", "sad_16_v2"))

all(auditc_items == c("alcohol_audit_c_1", "alcohol_audit_c_2", "alcohol_audit_c_3"))

length(mdib_neg_items) == 12
length(mdib_ben_items) == 24
length(bbsiq_neg_items_mdib) == 14
length(bbsiq_ben_items_mdib) == 28
length(asi_items) == 16
length(bfne2_items) == 12
length(neuroqol_anx_items) == 8
length(sads_items) == 28
length(sads_red_items) == 8
length(auditc_items) == 3

# Define items for purported MDIB scales (internal threats = catastrophizing 
# about disease progression, external threats = negative social evaluation) and 
# BBSIQ scales (internal threats, external threats)

mdib_neg_int_items <- c("md_bbsiq_1b_neg", "md_bbsiq_4c_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_8b_neg", "md_bbsiq_12b_neg")
mdib_neg_ext_items <- c("md_bbsiq_2a_neg", "md_bbsiq_3c_neg", "md_bbsiq_5a_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_9c_neg", "md_bbsiq_10a_neg", 
                        "md_bbsiq_11c_neg")

length(mdib_neg_int_items) == 5
length(mdib_neg_ext_items) == 7

bbsiq_neg_int_items_mdib <- c("bbsiq_2b_neg", "bbsiq_3c_neg", "bbsiq_5a_neg", 
                              "bbsiq_8c_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_14c_neg")
bbsiq_neg_ext_items_mdib <- c("bbsiq_1c_neg", "bbsiq_4c_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_9b_neg", "bbsiq_10b_neg", 
                              "bbsiq_13c_neg")

length(bbsiq_neg_int_items_mdib) == 7
length(bbsiq_neg_ext_items_mdib) == 7

# TODO: Define items for ASI subscales (physical, cognitive, and social concerns).
# Waiting on ILL delivery of articles https://doi.org/10.1016/0887-6185(96)00021-7
# and https://doi.org/10.1037//0021-843x.105.3.474, which Zinbarg et al. (1998)
# suggests may report the factor structure.

asi_phy_items <- c()
asi_cog_items <- c()
asi_soc_items <- c()





# Define items for 8-item BFNE-II, which is preferred (see Carleton et al., 2007; 
# https://doi.org/bgn7v6)

bfne2_8_items <- bfne2_items[!(bfne2_items %in% c("bfne_2", "bfne_4", 
                                                  "bfne_7", "bfne_11"))]

all(bfne2_8_items == c("bfne_1", "bfne_3", "bfne_5", "bfne_6", "bfne_8", "bfne_9", 
                       "bfne_10", "bfne_12"))

length(bfne2_8_items) == 8

# Store items in list

mdib_dat_items <- list(mdib_neg = mdib_neg_items,
                       mdib_neg_int = mdib_neg_int_items,
                       mdib_neg_ext = mdib_neg_ext_items,
                       mdib_ben = mdib_ben_items,
                       bbsiq_neg = bbsiq_neg_items_mdib,
                       bbsiq_neg_int = bbsiq_neg_int_items_mdib,
                       bbsiq_neg_ext = bbsiq_neg_ext_items_mdib,
                       bbsiq_ben = bbsiq_ben_items_mdib,
                       asi = asi_items,
                       asi_phy = asi_phy_items,
                       asi_cog = asi_cog_items,
                       asi_soc = asi_soc_items,
                       bfne2 = bfne2_items,
                       bfne2_8 = bfne2_8_items,
                       neuroqol_anx = neuroqol_anx_items,
                       sads = sads_items,
                       sads_red = sads_red_items,
                       auditc = auditc_items)

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Recode "prefer not to answer" (coded as 99) as NA

target_items <- c(mdib_dat_items$mdib_neg,
                  mdib_dat_items$mdib_ben,
                  mdib_dat_items$bbsiq_neg,
                  mdib_dat_items$bbsiq_ben,
                  mdib_dat_items$asi,
                  mdib_dat_items$bfne2,
                  mdib_dat_items$neuroqol_anx,
                  mdib_dat_items$sads,
                  mdib_dat_items$sads_red,
                  mdib_dat_items$auditc)

mdib_hd_dat[, target_items][mdib_hd_dat[, target_items] == 99] <- NA

# ---------------------------------------------------------------------------- #
# Recode BFNE-II ----
# ---------------------------------------------------------------------------- #

# Given that Carleton et al. (2007) uses a 0-4 scale, recode our 1-5 scale to a
# 0-4 scale (response options on REDCap did not have numbers listed next to them)

all(range(mdib_hd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(1, 5))

mdib_hd_dat[, mdib_dat_items$bfne2] <- mdib_hd_dat[, mdib_dat_items$bfne2] - 1

all(range(mdib_hd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(0, 4))

# ---------------------------------------------------------------------------- #
# Remove participants with incomplete MDIB data at baseline ----
# ---------------------------------------------------------------------------- #

# Remove 5 participants with incomplete MDIB data at baseline, leaving an overall 
# analysis sample of 65 participants

mdib_items <- c(mdib_dat_items$mdib_neg, mdib_dat_items$mdib_ben)

incompl_mdib_bl_data_ids <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == "baseline_arm_1" &
                                          rowSums(is.na(mdib_hd_dat[, mdib_items])) > 0, "record_id"]

length(incompl_mdib_bl_data_ids) == 5

mdib_hd_dat <- mdib_hd_dat[!(mdib_hd_dat$record_id %in% incompl_mdib_bl_data_ids), ]

length(unique(mdib_hd_dat$record_id)) == 65

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/further_clean")

save(mdib_hd_dat, file = "./data/further_clean/mdib_hd_dat.RData")

dir.create("./data/helper")

save(mdib_dat_items, file = "./data/helper/mdib_dat_items.RData")