# ---------------------------------------------------------------------------- #
# Run Exploratory Factor Analysis
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

pkgs <- c("psych", "lavaan")
groundhog.library(pkgs, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./data/further_clean/mdib_hd_dat2.RData")
load("./data/helper/mdib_dat_items.RData")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Restrict to baseline and MDIB columns

mdib_items <- c(mdib_dat_items$mdib_ben, mdib_dat_items$mdib_neg)

mdib_bl <- mdib_hd_dat2[mdib_hd_dat2$redcap_event_name == "baseline_arm_1", mdib_items]

# Rename columns to reflect benign/negative, internal/external threat, scenario, and item number

all(mdib_items ==
  c("md_bbsiq_1a_benign",  "md_bbsiq_1c_benign",  "md_bbsiq_2b_benign",  "md_bbsiq_2c_benign",
    "md_bbsiq_3a_benign",  "md_bbsiq_3b_benign",  "md_bbsiq_4a_benign",  "md_bbsiq_4b_benign",
    "md_bbsiq_5b_benign",  "md_bbsiq_5c_benign",  "md_bbsiq_6a_benign",  "md_bbsiq_6c_benign",
    "md_bbsiq_7b_benign",  "md_bbsiq_7c_benign",  "md_bbsiq_8a_benign",  "md_bbsiq_8c_benign",
    "md_bbsiq_9a_benign",  "md_bbsiq_9b_benign",  "md_bbsiq_10b_benign", "md_bbsiq_10c_benign",
    "md_bbsiq_11a_benign", "md_bbsiq_11b_benign", "md_bbsiq_12a_benign", "md_bbsiq_12c_benign",
    "md_bbsiq_1b_neg",     "md_bbsiq_2a_neg",     "md_bbsiq_3c_neg",     "md_bbsiq_4c_neg",
    "md_bbsiq_5a_neg",     "md_bbsiq_6b_neg",     "md_bbsiq_7a_neg",     "md_bbsiq_8b_neg",
    "md_bbsiq_9c_neg",     "md_bbsiq_10a_neg",    "md_bbsiq_11c_neg",    "md_bbsiq_12b_neg"))

mdib_items_rename <-
  c("mdib_ben_int_remember_1a", "mdib_ben_int_remember_1c", "mdib_ben_ext_server_2b",     "mdib_ben_ext_server_2c",
    "mdib_ben_ext_reminder_3a", "mdib_ben_ext_reminder_3b", "mdib_ben_int_cleaning_4a",   "mdib_ben_int_cleaning_4b",
    "mdib_ben_ext_neighbor_5b", "mdib_ben_ext_neighbor_5c", "mdib_ben_int_email_6a",      "mdib_ben_int_email_6c",
    "mdib_ben_ext_exercise_7b", "mdib_ben_ext_exercise_7c", "mdib_ben_int_medication_8a", "mdib_ben_int_medication_8c",
    "mdib_ben_ext_walk_9a",     "mdib_ben_ext_walk_9b",     "mdib_ben_ext_job_10b",       "mdib_ben_ext_job_10c",
    "mdib_ben_ext_stumble_11a", "mdib_ben_ext_stumble_11b", "mdib_ben_int_cough_12a",     "mdib_ben_int_cough_12c",
    "mdib_neg_int_remember_1b", "mdib_neg_ext_server_2a",   "mdib_neg_ext_reminder_3c",   "mdib_neg_int_cleaning_4c",
    "mdib_neg_ext_neighbor_5a", "mdib_neg_int_email_6b",    "mdib_neg_ext_exercise_7a",   "mdib_neg_int_medication_8b",
    "mdib_neg_ext_walk_9c",     "mdib_neg_ext_job_10a",     "mdib_neg_ext_stumble_11c",   "mdib_neg_int_cough_12b")

meaning <-
  c("ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "neg", "neg", "neg", "neg",
    "neg", "neg", "neg", "neg",
    "neg", "neg", "neg", "neg")

domain <-
  c("int", "int", "ext", "ext",
    "ext", "ext", "int", "int",
    "ext", "ext", "int", "int",
    "ext", "ext", "int", "int",
    "ext", "ext", "ext", "ext",
    "ext", "ext", "int", "int",
    "int", "ext", "ext", "int",
    "ext", "int", "ext", "int",
    "ext", "ext", "ext", "int")

mdib <- data.frame(items        = mdib_items,
                   items_rename = mdib_items_rename,
                   meaning      = meaning,
                   domain       = domain)

names(mdib_bl) <- mdib_items_rename[match(names(mdib_bl), mdib_items)]

# Order columns by meaning and then domain

mdib <- mdib[order(mdib$meaning, mdib$domain), ]

mdib_bl <- mdib_bl[match(mdib$items_rename, names(mdib_bl))]

# ---------------------------------------------------------------------------- #
# Inspect scree plot ----
# ---------------------------------------------------------------------------- #

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

efa_path   <- "./results/efa/"
scree_path <- paste0(efa_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound (see 
# Montoya & Edwards, 2021, p. 416) of 4 factors (based on principal axis factoring;
# "PA-PAF-m" in Lim & Jahng, 2019) or 4 components (based on principal component 
# analysis; "PA-PCA-m"). Lim and Jahng (2019) found that PA-PCA-m is better across 
# a wide variety of situations and recommend that. (Note that "ncomp" suggests 
# 2 components, whereas the eigenvalues of 4 components actually exceed the mean. 
# William Revelle confirmed via email on 7/13/2023 that "fa.parallel" defaults to 
# a "quant" argument of .95, meaning the 95% CI is used as the threshold rather 
# than the mean, in contrast to the documentation, which needs to be fixed.)

pdf(paste0(scree_path, "mdib_bl_scree_pa.pdf"), height = 6, width = 6)
(result <- fa.parallel(mdib_bl, fm = "ml", n.iter = 100))
dev.off()

result$ncomp == 2

sum(result$pc.values > result$pc.sim)  == 4
sum(result$pc.values > result$pc.simr) == 4

# ---------------------------------------------------------------------------- #
# Run EFA ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 2 to 9 factors, but parallel analysis 
# suggests a smaller number (up to 4, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 2, 3, 4, or 5.

# TODO: estimator "MLM" gives warnings re nonpositive definite for "oblimin" and "geomin".
# Adding "check.vcov = FALSE" (see http://127.0.0.1:26718/library/lavaan/html/lavOptions.html) 
# or changing the estimator to "ML" removes the warnings.

fit_oblimin <- efa(data = mdib_bl, nfactors = 2:5, rotation = "oblimin", estimator = "MLM")
fit_geomin  <- efa(data = mdib_bl, nfactors = 2:5, rotation = "geomin",  estimator = "MLM")





fit_oblimin <- efa(data = mdib_bl, nfactors = 2:5, rotation = "oblimin", estimator = "MLM", check.vcov = FALSE)
fit_geomin  <- efa(data = mdib_bl, nfactors = 2:5, rotation = "geomin",  estimator = "MLM", check.vcov = FALSE)
fit_promax  <- efa(data = mdib_bl, nfactors = 2:5, rotation = "promax",  estimator = "MLM")

sink(paste0(efa_path, "oblimin_mlm.txt"))
summary(fit_oblimin, se = TRUE, zstat = TRUE, pvalue = TRUE)
sink()

sink(paste0(efa_path, "geomin_mlm.txt"))
summary(fit_geomin,  se = TRUE, zstat = TRUE, pvalue = TRUE)
sink()

sink(paste0(efa_path, "promax_mlm.txt"))
summary(fit_promax,  se = TRUE, zstat = TRUE, pvalue = TRUE)
sink()

sink(paste0(efa_path, "oblimin_mlm_detail.txt"))
summary(fit_oblimin, se = TRUE, zstat = TRUE, pvalue = TRUE)
sink()

sink(paste0(efa_path, "geomin_mlm_detail.txt"))
summary(fit_geomin,  se = TRUE, zstat = TRUE, pvalue = TRUE)
sink()

sink(paste0(efa_path, "promax_mlm_detail.txt"))
summary(fit_promax,  se = TRUE, zstat = TRUE, pvalue = TRUE)
sink()





# TODO: For reference on EMA, see "twincogFA.R" and "PHysCompFA.R" from 11/4/2019
# multivariate class with Steve Boker




